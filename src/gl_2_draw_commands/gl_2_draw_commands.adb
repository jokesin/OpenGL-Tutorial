with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Matrices;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Objects.Vertex_Arrays;
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

package body GL_2_Draw_Commands is

   ---------------
   -- Main_Loop --
   ---------------

   procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is

      use GL.Types;

      Vertex_Array_Object          : GL.Objects.Vertex_Arrays.Vertex_Array_Object;

      Index_Buffer                 : GL.Objects.Buffers.Buffer;
      Vertex_Buffer                : GL.Objects.Buffers.Buffer;

      Render_Program               : GL.Objects.Programs.Program;

      vPosition                    : constant GL.Attributes.Attribute := 0;
      vColour                      : constant GL.Attributes.Attribute := 1;

      Render_Model_Matrix_Loc      : GL.Uniforms.Uniform;
      Render_Projection_Matrix_Loc : GL.Uniforms.Uniform;

      Aspect :Single;
      --------------------------------------------------------------

      procedure Setup is
         use GL.Objects.Buffers;
         use GL.Objects.Programs;
         use GL.Objects.Shaders;
         use Program_Loader;

         use type Interfaces.C.int;


      begin

         declare
            Width, Height : Glfw.Size;
         begin
            Main_Window.Get_Size(Width, Height);
            Aspect := Single( Width ) / Single ( Height );
         end;

         Render_Program := Program_From((Src("shaders\gl_2_draw_commands\vertex_shader.glsl", Vertex_Shader),
                                        Src("shaders\gl_2_draw_commands\fragment_shader.glsl", Fragment_Shader)));
         Use_Program(Render_Program);
         Render_Model_Matrix_Loc := Uniform_Location(Render_Program, "model_matrix");
         Render_Projection_Matrix_Loc := Uniform_Location(Render_Program, "projection_matrix");



         declare
            Block_Size    : Int :=Utilities.Get_Size(Vertex_Data.Vertex_Positions) +
              Utilities.Get_Size(Vertex_Data.Vertex_Colors);
            Colour_Offset : Int := Utilities.Get_Size(Vertex_Data.Vertex_Positions);
         begin

            -- выделим имя для буфера вершин
            Vertex_Array_Object.Initialize_Id;-- glGenVertexArrays
            Vertex_Array_Object.Bind; -- glBindVertexArray

            -- установим буфер индексов
            Index_Buffer.Initialize_Id;-- GenBuffers
            Element_Array_Buffer.Bind(Index_Buffer);--glBindBuffer(GL_ELEMENT_ARRAY_BUFFER...
            Utilities.Load_Element_Buffer(Element_Array_Buffer, Vertex_Data.Vertex_Indices, Static_Draw);

            -- установим атрибуты вершин
            Vertex_Buffer.Initialize_Id;-- GenBuffers
            Array_Buffer.Bind(Vertex_Buffer);--glBindBuffer(GL_ARRAY_BUFFER...

            GL.Objects.Buffers.Allocate(Array_Buffer, Long(Block_Size),Static_Draw);
            Utilities.Set_Buffer_Sub_Data(Array_Buffer, 0, Vertex_Data.Vertex_Positions);
            Utilities.Set_Buffer_Sub_Data(Array_Buffer, Colour_Offset, Vertex_Data.Vertex_Colors);


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

      ---------------------------------------------------------------

      procedure Render is
         use GL.Objects.Buffers;
         use GL.Objects.Programs;
         use Glfw;

         Model_Matrix : Singles.Matrix4 := GL.Types.Singles.Identity4;
         Proj_Matrix  : Singles.Matrix4 := Maths.Perspective_Matrix(Top    => 1.0,
                                                                    Bottom => -1.0,
                                                                    Left   => -Aspect,
                                                                    Right  => Aspect,
                                                                    Near   => 1.0,
                                                                    Far    => 100.0);
         Black : Colors.Color := (0.0,0.0,0.0,1.0);

      begin
         Utilities.Clear_Background_Colour(Black);
         Use_Program(Render_Program);

         -- установим матрицы модели и проекции
         GL.Uniforms.Set_Single(Render_Projection_Matrix_Loc, Proj_Matrix);

         -- установки для вызова glDrawElements
         Vertex_Array_Object.Bind;
         Element_Array_Buffer.Bind(Index_Buffer);

         -- DrawArrays...
         Model_Matrix := Maths.Translation_Matrix((-3.0, 0.0, -5.0));
         GL.Uniforms.Set_Single(Render_Model_Matrix_Loc, Model_Matrix);
         GL.Objects.Vertex_Arrays.Draw_Arrays(Triangles, 0, 3);

         -- DrawElements...
         Model_Matrix := Maths.Translation_Matrix((-1.0, 0.0, -5.0));
         GL.Uniforms.Set_Single(Render_Model_Matrix_Loc, Model_Matrix);
         GL.Objects.Buffers.Draw_Elements(Triangles, 3, UShort_Type, 0);

         -- glDrawElementsBaseVertex...
         Model_Matrix := Maths.Translation_Matrix((1.0, 0.0, -5.0));
         GL.Uniforms.Set_Single(Render_Model_Matrix_Loc, Model_Matrix);
         GL.Objects.Buffers.Draw_Elements_Base_Vertex(Triangles, 3, UShort_Type, 0, 1);

         -- glDrawArraysInstanced
         Model_Matrix := Maths.Translation_Matrix((3.0, 0.0, -5.0));
         GL.Uniforms.Set_Single(Render_Model_Matrix_Loc, Model_Matrix);
         GL.Objects.Buffers.Draw_Arrays_Instanced(Triangles, 0, 3, 1);
      exception
         when  others =>
            Put_Line ("An exceptiom occurred in Render.");
            raise;
      end Render;

      -------------------------------------------------------------
      use type Glfw.Input.Button_State;

      Running : Boolean := True;
   begin
      Setup;
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

end GL_2_Draw_Commands;
