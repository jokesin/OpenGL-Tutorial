with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion;

with GL.Attributes;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Objects.Vertex_Arrays;
with GL.Rasterization;
with GL.Toggles;
with GL.Types;
with GL.Types.Colors;
with GL.Uniforms;

with Glfw;
with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Windows.Context;

with Interfaces.C;

with System;


with Maths;
with Program_Loader;
with Shapes;
with Utilities;

package body GL_Tutorials.Ex_4_Stencil_Test is

   Vertex_Array_Object : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Vertex_Buffer       : GL.Objects.Buffers.Buffer;
   Index_Buffer        : GL.Objects.Buffers.Buffer;
   Matrix_Buffer       : GL.Objects.Buffers.Buffer;
   Render_Program      : GL.Objects.Programs.Program;

   View_Matrix_Loc       : GL.Uniforms.Uniform;
   Projection_Matrix_Loc : GL.Uniforms.Uniform;

   Position_Loc : GL.Attributes.Attribute;
   Normal_Loc   : GL.Attributes.Attribute;
   Color_Loc    : GL.Attributes.Attribute;
   Matrix_Loc   : GL.Attributes.Attribute;

   Aspect : GL.Types.Single;

   Render_Index_Count : GL.Types.Int;

   procedure Setup(Main_Window : in out Glfw.Windows.Window);
   procedure Render;


   ----------------------------------------------------------------------
   ----------------------------------------------------------------------

   procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is
      use GL.Types;
      use Glfw;
      use type Glfw.Input.Button_State;

      Is_Checking_Frame_Time           : Boolean := False;
      Start_Time, End_Time, Check_Time : Uint;
      Frames                           : UInt := 0;

      Running : Boolean := True;
   begin
      Setup(Main_Window);
      while Running loop

         if not Is_Checking_Frame_Time then
            Start_Time := Uint(Time);
            Is_Checking_Frame_Time := True;
         end if;

         Render;

         End_Time := Uint(Time);
         Check_Time := End_Time - Start_Time;
         if (Check_Time < 1) then
            Frames := Frames + 1;
         else
            Put("Frames per second : " & Frames'Img & ASCII.CR);
            Frames := 0;
            Is_Checking_Frame_Time := False;
         end if;

         Glfw.Windows.Context.Swap_Buffers(Main_Window'Access);
         Glfw.Input.Poll_Events;
         Running := Running and not
           (Main_Window.Key_State(Glfw.Input.Keys.Escape) = Glfw.Input.Pressed);
         Running := Running and not Main_Window.Should_Close;
      end loop;

      Render_Program.Clear;
      Vertex_Buffer.Clear;
      Vertex_Array_Object.Clear;
   exception
      when others     =>
         Put_Line("An exception occured in Main_Loop");
         raise;
   end Main_Loop;

   ----------------------------------------------------------------------
   ----------------------------------------------------------------------

   procedure Render is
      use Glfw;
      use GL.Types;
      use GL.Objects;
      use GL.Objects.Buffers;
      use GL.Objects.Programs;
      use type Interfaces.C.double;

      T     : Seconds := Time / 20.0;-- mod 5000) / 5000.0;
      A,B,C : Single;
      oX    : Singles.Vector3 := (1.0, 0.0, 0.0);
      oY    : Singles.Vector3 := (0.0, 1.0, 0.0);
      oZ    : Singles.Vector3 := (0.0, 0.0, 1.0);

      View_Matrix  : Singles.Matrix4 :=
        Maths.Translation_Matrix((0.0, 0.0, -10.0));
      Proj_Matrix  : Singles.Matrix4 := Maths.Perspective_Matrix(Top    => 1.0,
                                                                 Bottom => -1.0,
                                                                 Left   => -Aspect,
                                                                 Right  => Aspect,
                                                                 Near   => 1.0,
                                                                 Far    => 100.0);

      Black : Colors.Color := (0.5,0.0,0.5,1.0);

      ---------------------------------------------------------------

      procedure Set_Model_Matrix_Mapped_Direct with Inline is
         use GL;
         use GL.Types;
         use type Singles.Matrix4;

         subtype Model_Matrix_Array is Singles.Matrix4_Array(1..1);
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
            Model_Matrices(K) := Maths.Rotation_Matrix(Maths.Degree(A + Single(T) * 720.0), oX) *
              Maths.Rotation_Matrix(Maths.Degree(B + Single(T) * 360.0), oY);-- *
              --Maths.Rotation_Matrix(Maths.Degree(C + Single(T) * 360.0), oZ) *
              --Maths.Translation_Matrix((2.0 + A, 2.0 + B, -1.0 + C));
         end loop;

         GL.Objects.Buffers.Unmap(Array_Buffer);
      end Set_Model_Matrix_Mapped_Direct;

      ---------------------------------------------------------------

   begin

      Set_Model_Matrix_Mapped_Direct;

      Utilities.Clear_Background_Colour(Black);

      -- запускаем программу
      Use_Program(Render_Program);

      -- установим матрицы модели и проекции
      GL.Uniforms.Set_Single(View_Matrix_Loc, View_Matrix);
      GL.Uniforms.Set_Single(Projection_Matrix_Loc, Proj_Matrix);

      -- подготовка к DrawElementsInstanced
      Vertex_Array_Object.Bind;
      Element_Array_Buffer.Bind(Index_Buffer);

      GL.Objects.Buffers.Draw_Elements_Instanced(Triangles, Render_Index_Count, UShort_Type, 0, 1);
   exception
      when  others =>
         Put_Line ("An exceptiom occurred in Render.");
         raise;
   end Render;

   ----------------------------------------------------------------------
   ----------------------------------------------------------------------

   procedure Setup(Main_Window : in out Glfw.Windows.Window) is
      use GL;
      use GL.Attributes;
      use GL.Types;
      use GL.Objects.Buffers;

      use GL.Objects.Shaders;
      use GL.Objects.Programs;
      use Maths.Single_Math_Functions;
      use Program_Loader;
      use Shapes;

      A,B,C : Single;

      Sphere      : Polyhedron := Shapes.Init(3.0, 80, 60);
      Vertices     : Shape_Vertex_Array := Sphere.Get_Vertices;
      Normals      : Shape_Normal_Array := Sphere.Get_Normals;
      Indices      : Shape_Indices_Array := Sphere.Get_Indices;
      Colors       : Singles.Vector4_Array(Vertices'Range) := (others => (others => 0.0));
      Color_Offset : Int := Utilities.Get_Size(Vertices);
      Normal_Offset : Int := Color_Offset + Utilities.Get_Size(Colors);
      Block_Size   : Long := Long(Utilities.Get_Size(Vertices) +
                                    Utilities.Get_Size(Colors) +
                                    Utilities.Get_Size(Normals));
   begin

      Render_Index_Count := Indices'Length;

      declare
         Width, Height : Glfw.Size;
      begin
         Main_Window.Get_Size(Width, Height);
         Aspect := Single( Width ) / Single ( Height );
      end;

      --GL.Toggles.Enable(GL.Toggles.Cull_Face);
      GL.Rasterization.Set_Polygon_Mode(GL.Rasterization.Line);

      Render_Program := Program_From((Src("shaders\gl_4_stencil_test\vertex_shader.glsl", Vertex_Shader),
                                     Src("shaders\gl_4_stencil_test\fragment_shader.glsl", Fragment_Shader)));
      Use_Program(Render_Program);
      View_Matrix_Loc := Uniform_Location(Render_Program, "view_matrix");
      Projection_Matrix_Loc := Uniform_Location(Render_Program, "projection_matrix");

      Vertex_Array_Object.Initialize_Id;
      Vertex_Array_Object.Bind;


      Index_Buffer.Initialize_Id;
      Element_Array_Buffer.Bind(Index_Buffer);
      Utilities.Load_Element_Buffer(Element_Array_Buffer, Indices, Static_Draw);

      -- получим позиции атрибутов
      Position_Loc := Attrib_Location(Render_Program, "position");
      Color_Loc := Attrib_Location(Render_Program, "color");
      Normal_Loc := Attrib_Location(Render_Program, "normal");
      Matrix_Loc := Attrib_Location(Render_Program, "model_matrix");

      -- сгенерируем цвета
      for K in Colors'Range loop
         A := Single(K) / 4.0;
         B := Single(K) / 5.0;
         C := Single(K) / 6.0;

         Colors(K)(X) := 0.5 * (Sin(A + 1.0) + 1.0);
         Colors(K)(Y) := 0.5 * (Sin(B + 2.0) + 1.0);
         Colors(K)(Z) := 0.5 * (Sin(C + 3.0) + 1.0);
         Colors(K)(W) := 1.0;
      end loop;

      Vertex_Buffer.Initialize_Id;
      Array_Buffer.Bind(Vertex_Buffer);
      Allocate(Array_Buffer,Block_Size,Static_Draw);
      Utilities.Set_Buffer_Sub_Data(Array_Buffer, 0, Vertices);
      Utilities.Set_Buffer_Sub_Data(Array_Buffer, Color_Offset, Colors);
      Utilities.Set_Buffer_Sub_Data(Array_Buffer, Normal_Offset, Normals);

      GL.Attributes.Set_Vertex_Attrib_Pointer2(Position_Loc, 4, Single_Type, 0, 0);
      GL.Attributes.Set_Vertex_Attrib_Pointer2(Color_Loc, 4, Single_Type, 0, Color_Offset);
      GL.Attributes.Set_Vertex_Attrib_Pointer2(Normal_Loc, 3, Single_Type, 0, Normal_Offset);

      GL.Attributes.Enable_Vertex_Attrib_Array(Position_Loc);
      GL.Attributes.Enable_Vertex_Attrib_Array(Color_Loc);
      GL.Attributes.Enable_Vertex_Attrib_Array(Normal_Loc);


      -- настроим получение матрицы
      Matrix_Buffer.Initialize_Id;
      Array_Buffer.Bind(Matrix_Buffer);
      Allocate(Array_Buffer, Singles.Matrix4'Size / System.Storage_Unit, Dynamic_Draw);

      -- пройдемс€ по каждой колонке матрицы дл€ установки Vertex_Attrib_Divisor
      -- чтобы матрица работала целиком дл€ всех вершин указанных экземпл€ров
      for K in 0..Attribute(3) loop
         GL.Attributes.Set_Vertex_Attrib_Pointer2(Matrix_Loc + K, 4,
                                                  Single_Type,
                                                  Singles.Matrix4'Size / System.Storage_Unit,
                                                  Int(K) * Singles.Vector4'Size / System.Storage_Unit);
         GL.Attributes.Enable_Vertex_Attrib_Array(Matrix_Loc + K);
         GL.Attributes.Vertex_Attrib_Divisor(Matrix_Loc + K, 1);
      end loop;
   exception
      when others => Put_Line("An exception occured in Setup.");
         raise;
   end Setup;
end GL_Tutorials.Ex_4_Stencil_Test;
