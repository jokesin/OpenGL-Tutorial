with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion;

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
with Interfaces.C.Pointers;

with GL;

with System.Machine_Code;

package body GL_3_Instanced_Rendering is

   use GL.Types;

   Vertex_Array_Object : GL.Objects.Vertex_Arrays.Vertex_Array_Object;

   Index_Buffer  : GL.Objects.Buffers.Buffer;
   Vertex_Buffer : GL.Objects.Buffers.Buffer;
   Color_Buffer  : GL.Objects.Buffers.Buffer;
   Matrix_Buffer : GL.Objects.Buffers.Buffer;

   Render_Program : GL.Objects.Programs.Program;

   View_Matrix_Loc       : GL.Uniforms.Uniform;
   Projection_Matrix_Loc : GL.Uniforms.Uniform;

   Position_Loc : GL.Attributes.Attribute;
   Normal_Loc   : GL.Attributes.Attribute;
   Color_Loc    : GL.Attributes.Attribute;
   Matrix_Loc   : GL.Attributes.Attribute;

   Aspect                       : Single;

   INSTANCE_COUNT : constant := 50;

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

      -- TODO разобраться с освобождением ресурсов
      Render_Program.Clear;
      Vertex_Buffer.Clear;
      Index_Buffer.Clear;
      Color_Buffer.Clear;
      Matrix_Buffer.Clear;
      Vertex_Array_Object.Clear;
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


   begin

      GL.Toggles.Enable(Cull_Face);

      declare
         Width, Height : Glfw.Size;
      begin
         Main_Window.Get_Size(Width, Height);
         Aspect := Single( Width ) / Single ( Height );
      end;

      Render_Program := Program_From((Src("shaders\gl_3_instanced_rendering\vertex_shader.glsl", Vertex_Shader),
                                     Src("shaders\gl_3_instanced_rendering\fragment_shader.glsl", Fragment_Shader)));
      Use_Program(Render_Program);
      View_Matrix_Loc := Uniform_Location(Render_Program, "view_matrix");
      Projection_Matrix_Loc := Uniform_Location(Render_Program, "projection_matrix");


      declare
         use GL;
         use GL.Attributes;
         use Maths.Single_Math_Functions;

         Block_Size     : Int := Utilities.Get_Size(Vertex_Data.Cube_Positions) +
           Utilities.Get_Size(Vertex_Data.Cube_Normals);
         Normals_Offset : Int := Utilities.Get_Size(Vertex_Data.Cube_Positions);

         Cube_Colors    : Singles.Vector4_Array(1..INSTANCE_COUNT);
         A,B,C          : Single;
      begin

         -- выделим имя для буфера вершин
         Vertex_Array_Object.Initialize_Id;-- glGenVertexArrays
         Vertex_Array_Object.Bind; -- glBindVertexArray

         -- установим буфер индексов
         Index_Buffer.Initialize_Id;-- GenBuffers
         Element_Array_Buffer.Bind(Index_Buffer);--glBindBuffer(GL_ELEMENT_ARRAY_BUFFER...
         Utilities.Load_Element_Buffer(Element_Array_Buffer, Vertex_Data.Cube_Indices, Static_Draw);

         -- получим позиции атрибутов
         Position_Loc := Attrib_Location(Render_Program, "position");
         Normal_Loc := Attrib_Location(Render_Program, "normal");
         Color_Loc := Attrib_Location(Render_Program, "color");
         Matrix_Loc := Attrib_Location(Render_Program, "model_matrix");

         -- установим атрибуты вершин
         Vertex_Buffer.Initialize_Id;-- GenBuffers
         Array_Buffer.Bind(Vertex_Buffer);--glBindBuffer(GL_ARRAY_BUFFER...

         GL.Objects.Buffers.Allocate(Array_Buffer, Long(Block_Size), Static_Draw);
         Utilities.Set_Buffer_Sub_Data(Array_Buffer, 0, Vertex_Data.Cube_Positions);
         Utilities.Set_Buffer_Sub_Data(Array_Buffer, Normals_Offset, Vertex_Data.Cube_Normals);

         GL.Attributes.Set_Vertex_Attrib_Pointer2(Position_Loc, 4, Single_Type, 0, 0);
         GL.Attributes.Set_Vertex_Attrib_Pointer2(Normal_Loc, 3, Single_Type, 0, Normals_Offset);

         GL.Attributes.Enable_Vertex_Attrib_Array(Position_Loc);
         GL.Attributes.Enable_Vertex_Attrib_Array(Normal_Loc);

         -- установим цвет
         for K in 1..Int(INSTANCE_COUNT) loop
            A := Single(K) / 4.0;
            B := Single(K) / 5.0;
            C := Single(K) / 6.0;

            Cube_Colors(K)(X) := 0.5 * (Sin(A + 1.0) + 1.0);
            Cube_Colors(K)(Y) := 0.5 * (Sin(B + 2.0) + 1.0);
            Cube_Colors(K)(Z) := 0.5 * (Sin(C + 3.0) + 1.0);
            Cube_Colors(K)(W) := 1.0;
         end loop;
         Color_Buffer.Initialize_Id;
         Array_Buffer.Bind(Color_Buffer);
         Utilities.Load_Vertex_Buffer_4(Array_Buffer,Cube_Colors,Dynamic_Draw);

         Array_Buffer.Bind(Color_Buffer);
         GL.Attributes.Set_Vertex_Attrib_Pointer2(Color_Loc, 4, Single_Type, 0, 0);
         GL.Attributes.Enable_Vertex_Attrib_Array(Color_Loc);
         GL.Attributes.Vertex_Attrib_Divisor(Color_Loc, 1);

         -- также поступим с матрицей модели
         Matrix_Buffer.Initialize_Id;
         Array_Buffer.Bind(Matrix_Buffer);
         GL.Objects.Buffers.Allocate(Array_Buffer,
                                     INSTANCE_COUNT * Singles.Matrix4'Size / System.Storage_Unit,
                                     Dynamic_Draw);


         -- пройдемся по каждой колонке матрицы для установки Vertex_Attrib_Divisor
         for K in 0..Attribute(3) loop
            GL.Attributes.Set_Vertex_Attrib_Pointer2(Matrix_Loc + K, 4,
                                                     Single_Type,
                                                     Singles.Matrix4'Size / System.Storage_Unit,
                                                     Int(K) * Singles.Vector4'Size / System.Storage_Unit);
            GL.Attributes.Enable_Vertex_Attrib_Array(Matrix_Loc + K);
            GL.Attributes.Vertex_Attrib_Divisor(Matrix_Loc + K, 1);
         end loop;

      end;

   exception
      when others =>
         Put_Line("An exception occured in Setup.");
         raise;
   end Setup;

   procedure Render is
      use GL.Attributes;
      use GL.Objects;
      use GL.Objects.Buffers;
      use GL.Objects.Programs;
      use GL.Toggles;
      use Glfw;
      use Interfaces.C;
      use type Singles.Matrix4;
      use type GL.Attributes.Attribute;

      T     : Seconds := Time / 20.0;-- mod 5000) / 5000.0;
      A,B,C : Single;
      oX    : Singles.Vector3 := (1.0, 0.0, 0.0);
      oY    : Singles.Vector3 := (0.0, 1.0, 0.0);
      oZ    : Singles.Vector3 := (0.0, 0.0, 1.0);

      View_Matrix  : Singles.Matrix4 :=
        Maths.Translation_Matrix((0.0, 0.0, -60.0));

      Model_Matrix : Singles.Matrix4;

      Proj_Matrix  : Singles.Matrix4 := Maths.Perspective_Matrix(Top    => 1.0,
                                                                 Bottom => -1.0,
                                                                 Left   => -Aspect,
                                                                 Right  => Aspect,
                                                                 Near   => 1.0,
                                                                 Far    => 1000.0);


      Black        : Colors.Color := (0.0,0.0,0.0,1.0);

      ---------------------------------------------------------------

      procedure Set_Model_Matrix_Sub_Data with Inline is
      begin
         Array_Buffer.Bind(Matrix_Buffer);
         for K in 1..Attribute(INSTANCE_COUNT) loop
            A := 5.0 * Single(K) / 4.0;
            B := 5.0 * Single(K) / 5.0;
            C := 5.0 * Single(K) / 6.0;

            Model_Matrix := Maths.Rotation_Matrix(Maths.Degree(A + Single(T) * 360.0), oX) *
              Maths.Rotation_Matrix(Maths.Degree(B + Single(T) * 360.0), oY) *
              Maths.Rotation_Matrix(Maths.Degree(C + Single(T) * 360.0), oZ) *
              Maths.Translation_Matrix((2.0 + A, 2.0 + B, -1.0 + C));

            Utilities.Set_Buffer_Sub_Data_M(Array_Buffer,
                                            GL.Types.Int((K - 1) * Model_Matrix'Size / System.Storage_Unit),
                                            (1=>Model_Matrix));

         end loop;
      end Set_Model_Matrix_Sub_Data;

      ---------------------------------------------------------------

      procedure Set_Model_Matrix_Mapped with Inline is
         use GL;
         use GL.Types;
         Model_Matrices_Pointer : Singles.Matrix4_Pointers.Pointer;

      begin
         Array_Buffer.Bind(Matrix_Buffer);
         Utilities.Map(Array_Buffer, Write_Only, Model_Matrices_Pointer);
         declare
            Model_Matrices : aliased Singles.Matrix4_Array:= Singles.Matrix4_Pointers.Value(Model_Matrices_Pointer, INSTANCE_COUNT);
         begin
            for K in 1..GL.Types.Int(INSTANCE_COUNT) loop
               A := 5.0 * Single(K) / 4.0;
               B := 5.0 * Single(K) / 5.0;
               C := 5.0 * Single(K) / 6.0;
               Model_Matrices(Model_Matrices'First + K - 1) := Maths.Rotation_Matrix(Maths.Degree(A + Single(T) * 360.0), oX) *
                 Maths.Rotation_Matrix(Maths.Degree(B + Single(T) * 360.0), oY) *
                 Maths.Rotation_Matrix(Maths.Degree(C + Single(T) * 360.0), oZ) *
                 Maths.Translation_Matrix((2.0 + A, 2.0 + B, -1.0 + C));
            end loop;
            Singles.Matrix4_Pointers.Copy_Array(Source => Model_Matrices(Model_Matrices'First)'Unchecked_Access,
                                                Target => Model_Matrices_Pointer,
                                                Length => INSTANCE_COUNT);
         end;

         GL.Objects.Buffers.Unmap(Array_Buffer);
      end Set_Model_Matrix_Mapped;

      ---------------------------------------------------------------

      procedure Set_Model_Matrix_Mapped_Direct with Inline is
         use GL;
         use GL.Types;

         subtype Model_Matrix_Array is Singles.Matrix4_Array(1..INSTANCE_COUNT);
         type Model_Matrices_Access is access all Model_Matrix_Array;
         Model_Matrices : Model_Matrices_Access;

         Model_Matrices_Pointer : Singles.Matrix4_Pointers.Pointer;

         function To_Array_Access is new Ada.Unchecked_Conversion(Singles.Matrix4_Pointers.Pointer,
                                                                  Model_Matrices_Access);
      begin
         Array_Buffer.Bind(Matrix_Buffer);
         Utilities.Map(Array_Buffer, Write_Only, Model_Matrices_Pointer);

         Model_Matrices := To_Array_Access(Model_Matrices_Pointer);

         for K in Model_Matrices'Range loop
            A := 5.0 * Single(K) / 4.0;
            B := 5.0 * Single(K) / 5.0;
            C := 5.0 * Single(K) / 6.0;
            Model_Matrices(K) := Maths.Rotation_Matrix(Maths.Degree(A + Single(T) * 360.0), oX) *
              Maths.Rotation_Matrix(Maths.Degree(B + Single(T) * 360.0), oY) *
              Maths.Rotation_Matrix(Maths.Degree(C + Single(T) * 360.0), oZ) *
              Maths.Translation_Matrix((2.0 + A, 2.0 + B, -1.0 + C));
         end loop;

         GL.Objects.Buffers.Unmap(Array_Buffer);
      end Set_Model_Matrix_Mapped_Direct;

      ---------------------------------------------------------------

   begin

      --Put_Line("Seconds : " & T'Img);

      Utilities.Clear_Background_Colour(Black);

      -- установить матрицу модели
      --Set_Model_Matrix_Sub_Data;
      --Set_Model_Matrix_Mapped;
      Set_Model_Matrix_Mapped_Direct;

      -- запускаем программу
      Use_Program(Render_Program);

      -- установим матрицы модели и проекции
      GL.Uniforms.Set_Single(View_Matrix_Loc, View_Matrix);
      GL.Uniforms.Set_Single(Projection_Matrix_Loc, Proj_Matrix);

      -- установки для вызова glDrawElements

      Vertex_Array_Object.Bind;
      Element_Array_Buffer.Bind(Index_Buffer);

      -- рисуем с индексом рестарта
      -- вызываем только одну команду рисования
      GL.Toggles.Enable(Primitive_Restart);
      Primitive_Restart_Index(16#FFFF#);
      GL.Objects.Buffers.Draw_Elements_Instanced(Triangle_Strip, 17, UShort_Type, 0, INSTANCE_COUNT);

      -- без рестарта надо уже 2 (куб из двух полос друг в друга)
      --GL.Objects.Buffers.Draw_Elements_Instanced(Triangle_Strip, 8, UShort_Type, 0, 1);
      --GL.Objects.Buffers.Draw_Elements_Instanced(Triangle_Strip, 8, UShort_Type, 9, 1);
   exception
      when  others =>
         Put_Line ("An exceptiom occurred in Render.");
         raise;
   end Render;


end GL_3_Instanced_Rendering;
