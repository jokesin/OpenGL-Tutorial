with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Matrices;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Objects.Vertex_Arrays;
with GL.Toggles;
with GL.Types;
with GL.Types.Colors;
with GL.Uniforms;

with Glfw;
with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Windows.Context;

with Maths;
with Program_Loader;
with Utilities;

with Interfaces.C;

with GL;

package body GL_Tutorials.Ex_3_Restarting_Primitives is

   use GL.Types;

   Vertex_Array_Object          : GL.Objects.Vertex_Arrays.Vertex_Array_Object;

   Index_Buffer                 : GL.Objects.Buffers.Buffer;
   Vertex_Buffer                : GL.Objects.Buffers.Buffer;

   Render_Program               : GL.Objects.Programs.Program;

   vPosition                    : constant GL.Attributes.Attribute := 0;
   vColour                      : constant GL.Attributes.Attribute := 1;

   Render_Model_Matrix_Loc      : GL.Uniforms.Uniform;
   Render_Projection_Matrix_Loc : GL.Uniforms.Uniform;

   Aspect                       : Single;

   procedure Setup(Main_Window : in out Glfw.Windows.Window);
   procedure Render;


   ---------------
   -- Main_Loop --
   ---------------

   procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is

      use type Glfw.Input.Button_State;

      Running : Boolean := True;
   begin
      Setup(Main_Window);
      while Running loop
         Render;

         Glfw.Windows.Context.Swap_Buffers(Main_Window'Access);
         Glfw.Input.Poll_Events;
         Running := Running and not
           (Main_Window.Key_State(Glfw.Input.Keys.Escape) = Glfw.Input.Pressed);
         Running := Running and not Main_Window.Should_Close;
      end loop;

   exception
      when others =>
         Put_Line("An exception occured in Main_Loop");
         raise;
   end Main_Loop;


   procedure Setup(Main_Window : in out Glfw.Windows.Window) is
         use GL.Objects.Buffers;
         use GL.Objects.Programs;
         use GL.Objects.Shaders;
         use GL.Toggles;
         use Program_Loader;
         use type Interfaces.C.int;

      function Get_Size is new Utilities.Get_Bytes_Count(GL.Types.Singles.Vector3_Pointers);
      function Get_Size is new Utilities.Get_Bytes_Count(GL.Types.Singles.Vector4_Pointers);

      begin

         GL.Toggles.Enable(Cull_Face);

         declare
            Width, Height : Glfw.Size;
         begin
            Main_Window.Get_Size(Width, Height);
            Aspect := Single( Width ) / Single ( Height );
         end;

         Render_Program := Program_From((Src("shaders\gl_3_restarting_primitives\vertex_shader.glsl", Vertex_Shader),
                                        Src("shaders\gl_3_restarting_primitives\fragment_shader.glsl", Fragment_Shader)));
         Use_Program(Render_Program);
         Render_Model_Matrix_Loc := Uniform_Location(Render_Program, "model_matrix");
         Render_Projection_Matrix_Loc := Uniform_Location(Render_Program, "projection_matrix");


         declare
            Block_Size    : Int := Get_Size(Vertex_Data.Cube_Positions) + Get_Size(Vertex_Data.Cube_Colors);
            Colour_Offset : Int := Get_Size(Vertex_Data.Cube_Positions);
         begin

            -- выделим имя для буфера вершин
            Vertex_Array_Object.Initialize_Id;-- glGenVertexArrays
            Vertex_Array_Object.Bind; -- glBindVertexArray

            -- установим буфер индексов
            Index_Buffer.Initialize_Id;-- GenBuffers
            Element_Array_Buffer.Bind(Index_Buffer);--glBindBuffer(GL_ELEMENT_ARRAY_BUFFER...
            Utilities.Load_Element_Buffer(Element_Array_Buffer, Vertex_Data.Cube_Indices, Static_Draw);

            -- установим атрибуты вершин
            Vertex_Buffer.Initialize_Id;-- GenBuffers
            Array_Buffer.Bind(Vertex_Buffer);--glBindBuffer(GL_ARRAY_BUFFER...

            GL.Objects.Buffers.Allocate(Array_Buffer, Long(Block_Size),Static_Draw);
            Utilities.Set_Buffer_Sub_Data(Array_Buffer, 0, Vertex_Data.Cube_Positions);
            Utilities.Set_Buffer_Sub_Data(Array_Buffer, Colour_Offset, Vertex_Data.Cube_Colors);


            GL.Attributes.Set_Vertex_Attrib_Pointer2(vPosition, 4, Single_Type, 0, 0);
            GL.Attributes.Set_Vertex_Attrib_Pointer2(vColour, 4, Single_Type, 0, Colour_Offset);

            GL.Attributes.Enable_Vertex_Attrib_Array(vPosition);
            GL.Attributes.Enable_Vertex_Attrib_Array(vColour);
         end;

      exception
         when others =>
            Put_Line("An exception occured in Setup.");
            raise;
   end Setup;

   procedure Render is
         use GL.Objects.Buffers;
         use GL.Objects.Programs;
         use GL.Toggles;
         use Glfw;
         use Interfaces.C;
         use type Singles.Matrix4;

         T : Seconds := Time / 20.0;-- mod 5000) / 5000.0;
         oX : Singles.Vector3 := (1.0, 0.0, 0.0);
         oY : Singles.Vector3 := (0.0, 1.0, 0.0);
         oZ : Singles.Vector3 := (0.0, 0.0, 1.0);


         Model_Matrix : Singles.Matrix4 :=
           Maths.Translation_Matrix((0.0, 0.0, -5.0)) *
             Maths.Rotation_Matrix(Maths.Degree(T * 360.0), oY) *
               Maths.Rotation_Matrix(Maths.Degree(T * 720.0), oZ);


         Proj_Matrix  : Singles.Matrix4 := Maths.Perspective_Matrix(Top    => 1.0,
                                                                    Bottom => -1.0,
                                                                    Left   => -Aspect,
                                                                    Right  => Aspect,
                                                                    Near   => 1.0,
                                                                    Far    => 100.0);
         Black : Colors.Color := (0.0,0.0,0.0,1.0);

      begin

         --Put_Line("Seconds : " & T'Img);

         Utilities.Clear_Background_Colour(Black);
         Use_Program(Render_Program);

         -- установим матрицы модели и проекции
         GL.Uniforms.Set_Single(Render_Model_Matrix_Loc, Model_Matrix);
         GL.Uniforms.Set_Single(Render_Projection_Matrix_Loc, Proj_Matrix);

         -- установки для вызова glDrawElements
         Vertex_Array_Object.Bind;
         Element_Array_Buffer.Bind(Index_Buffer);

         -- рисуем с индексом рестарта
         -- вызываем только одну команду рисования
         GL.Toggles.Enable(Primitive_Restart);
         Primitive_Restart_Index(16#FFFF#);
         GL.Objects.Buffers.Draw_Elements(Triangle_Strip, 17, UShort_Type, 0);

         -- без рестарта надо уже 2 (куб из двух полос друг в друга)
         --GL.Objects.Buffers.Draw_Elements(Triangle_Strip, 8, UShort_Type, 0);
         --GL.Objects.Buffers.Draw_Elements(Triangle_Strip, 8, UShort_Type, 9);
      exception
         when  others =>
            Put_Line ("An exceptiom occurred in Render.");
            raise;
      end Render;
end GL_Tutorials.Ex_3_Restarting_Primitives;
