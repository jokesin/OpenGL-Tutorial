with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion;

with GL.Attributes;
with GL.Matrices;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Objects.Textures;
with GL.Objects.Textures.Targets;
with GL.Objects.Vertex_Arrays;
with GL.Pixels;
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

package body GL_Tutorials.Ex_3_Instanced_Rendering_2 is

   use GL.Types;

   Vertex_Array_Object : GL.Objects.Vertex_Arrays.Vertex_Array_Object;

   Index_Buffer  : GL.Objects.Buffers.Buffer;
   Vertex_Buffer : GL.Objects.Buffers.Buffer;

   Color_Buffer        : GL.Objects.Buffers.Buffer;
   Model_Matrix_Buffer : GL.Objects.Buffers.Buffer;

   Color_TBO        : GL.Objects.Textures.Texture;
   Model_Matrix_TBO : GL.Objects.Textures.Texture;

   Render_Program : GL.Objects.Programs.Program;

   View_Matrix_Loc       : GL.Uniforms.Uniform;
   Projection_Matrix_Loc : GL.Uniforms.Uniform;
   Color_TBO_Loc         : GL.Uniforms.Uniform;
   Model_Matrix_TBO_Loc  : GL.Uniforms.Uniform;

   Position_Loc : GL.Attributes.Attribute;
   Normal_Loc   : GL.Attributes.Attribute;

   Aspect : Single;

   INSTANCE_COUNT : constant := 20000;


   procedure Setup(Main_Window : in out Glfw.Windows.Window);
   procedure Render;

   ---------------
   -- Main_Loop --
   ---------------

   procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is
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

      -- TODO разобраться с освобождением ресурсов
      Render_Program.Clear;
      Vertex_Buffer.Clear;
      Index_Buffer.Clear;
      Color_Buffer.Clear;
      Model_Matrix_Buffer.Clear;
      Color_TBO.Clear;
      Model_Matrix_TBO.Clear;
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

      Render_Program := Program_From((Src("shaders\gl_3_instanced_rendering_2\vertex_shader.glsl", Vertex_Shader),
                                     Src("shaders\gl_3_instanced_rendering_2\fragment_shader.glsl", Fragment_Shader)));
      Use_Program(Render_Program);

      View_Matrix_Loc       := Uniform_Location(Render_Program, "view_matrix");
      Projection_Matrix_Loc := Uniform_Location(Render_Program, "projection_matrix");

      -- Настроим TBO
      Color_TBO_Loc        := Uniform_Location(Render_Program, "color_tbo");
      Model_Matrix_TBO_Loc := Uniform_Location(Render_Program, "model_matrix_tbo");
      -- Установим текстурные слоты для наших TBO
      GL.Uniforms.Set_Int(Color_TBO_Loc, 0);
      GL.Uniforms.Set_Int(Model_Matrix_TBO_Loc, 1);

      declare
         use GL;
         use GL.Attributes;

         use Maths.Single_Math_Functions;

         Block_Size     : Int := Get_Size(Vertex_Data.Cube_Positions) + Get_Size(Vertex_Data.Cube_Normals);
         Normals_Offset : Int := Get_Size(Vertex_Data.Cube_Positions);

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
         Normal_Loc   := Attrib_Location(Render_Program, "normal");

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


         -- Теперь установим TBO для экземплярных цветов и матриц

         -- Сначала создадим TBO для сохранения цветов, присоединим буфер к нему и
         -- инициализируем его формат. Буфер создается заранее для сохранения одного
         -- vec4 на экземпляр

         Color_TBO.Initialize_Id;--glGenTextures
         --TODO проверить тип
         Gl.Objects.Textures.Targets.Texture_Buffer.Bind(Color_TBO);--glBindTexture(GL_TEXTURE_BUFFER

         -- сгенерируем цвета для объектов
         for K in 1..Int(INSTANCE_COUNT) loop
            A := Single(K) / 4.0;
            B := Single(K) / 5.0;
            C := Single(K) / 6.0;

            Cube_Colors(K)(X) := 0.5 * (Sin(A + 1.0) + 1.0);
            Cube_Colors(K)(Y) := 0.5 * (Sin(B + 2.0) + 1.0);
            Cube_Colors(K)(Z) := 0.5 * (Sin(C + 3.0) + 1.0);
            Cube_Colors(K)(W) := 1.0;
         end loop;

         -- Создать буфер, инициализировать и присоединить к буферу текстуры
         Color_Buffer.Initialize_Id;
         --TODO check type
         Texture_Buffer.Bind(Color_Buffer);
         Utilities.Load_Vertex_Buffer_4(Texture_Buffer,Cube_Colors,Static_Draw);
         Texture_Buffer.Allocate(GL.Pixels.RGBA32F, Color_Buffer);

         -- также поступим с TBO матриц модели.
         Model_Matrix_TBO.Initialize_Id;
         GL.Objects.Textures.Set_Active_Unit(1);
         GL.Objects.Textures.Targets.Texture_Buffer.Bind(Model_Matrix_TBO);

         Model_Matrix_Buffer.Initialize_Id;
         Texture_Buffer.Bind(Model_Matrix_Buffer);
         GL.Objects.Buffers.Allocate(Texture_Buffer,
                                     INSTANCE_COUNT * Singles.Matrix4'Size / System.Storage_Unit,
                                     Dynamic_Draw);
         Texture_Buffer.Allocate(GL.Pixels.RGBA32F, Model_Matrix_Buffer);

         GL.Objects.Textures.Set_Active_Unit(0);
         --System.Machine_Code.Asm("int $3");
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

      subtype Model_Matrix_Array_Type is Singles.Matrix4_Array(1..INSTANCE_COUNT);

      Proj_Matrix  : Singles.Matrix4 := Maths.Perspective_Matrix(Top    => 1.0,
                                                                 Bottom => -1.0,
                                                                 Left   => -Aspect,
                                                                 Right  => Aspect,
                                                                 Near   => 1.0,
                                                                 Far    => 1500.0);


      Black        : Colors.Color := (0.0,0.0,0.0,1.0);

      ---------------------------------------------------------------

      procedure Set_Model_Matrix_Loaded is
         Model_Matrix_Array : Model_Matrix_Array_Type;
      begin
         for K in Model_Matrix_Array'Range loop
            A := 5.0 * Single(K) / 4.0;
            B := 5.0 * Single(K) / 5.0;
            C := 5.0 * Single(K) / 6.0;
            Model_Matrix_Array(K) := Maths.Rotation_Matrix(Maths.Degree(A + Single(T) * 360.0), oX) *
              Maths.Rotation_Matrix(Maths.Degree(B + Single(T) * 360.0), oY) *
              Maths.Rotation_Matrix(Maths.Degree(C + Single(T) * 360.0), oZ) *
              Maths.Translation_Matrix((2.0 + A, 2.0 + B, -1.0 + C));
         end loop;

         -- Привяжем большой VBO и изменим его содержимое
         Texture_Buffer.Bind(Model_Matrix_Buffer);
         Utilities.Load_To_Buffer(Texture_Buffer, Model_Matrix_Array, Dynamic_Draw);
      end Set_Model_Matrix_Loaded;

      ---------------------------------------------------------------

      procedure Set_Model_Matrix_Mapped_Direct with Inline is
         use GL;
         use GL.Types;

         type Model_Matrices_Access is access all Model_Matrix_Array_Type;
         Model_Matrices : Model_Matrices_Access;

         Model_Matrices_Pointer : Singles.Matrix4_Pointers.Pointer;

         function To_Array_Access is new Ada.Unchecked_Conversion(Singles.Matrix4_Pointers.Pointer,
                                                                  Model_Matrices_Access);
      begin
         Texture_Buffer.Bind(Model_Matrix_Buffer);
         Utilities.Map(Texture_Buffer, Write_Only, Model_Matrices_Pointer);

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

         GL.Objects.Buffers.Unmap(Texture_Buffer);
      end Set_Model_Matrix_Mapped_Direct;

      ---------------------------------------------------------------
   begin

      Set_Model_Matrix_Loaded;
      --Set_Model_Matrix_Mapped_Direct;

      -- Очистим экран
      Utilities.Clear_Background_Colour(Black);

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


end GL_Tutorials.Ex_3_Instanced_Rendering_2;
