with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Objects.Vertex_Arrays;
with GL.Types;
with GL.Types.Colors;

with Glfw;
with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Windows.Context;

with Program_Loader;
with Utilities;

package body GL_Tutorials.Ex_1_Simple_Draw is

   ---------------
   -- Main_Loop --
   ---------------

   procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is
   Vertex_Array_Object : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Vertex_Buffer       : GL.Objects.Buffers.Buffer;
   Render_Program      : GL.Objects.Programs.Program;
   Num_Vertices        : GL.Types.Int := 6;
   vPosition           : GL.Attributes.Attribute := 0;

   ---------------------------------------------------------------

   procedure Render is
      use GL.Types;
      use GL.Objects.Buffers;
      use GL.Objects.Programs;

      Black : Colors.Color := (0.5,0.0,0.5,1.0);
   begin
      Utilities.Clear_Background_Colour(Black);
      Use_Program(Render_Program);

      GL.Attributes.Enable_Vertex_Attrib_Array(vPosition);
      Array_Buffer.Bind(Vertex_Buffer);

      GL.Attributes.Set_Vertex_Attrib_Pointer(vPosition, 2, Single_Type, 0, 0);
      GL.Objects.Vertex_Arrays.Draw_Arrays(Triangles, 0, Num_Vertices);

      GL.Attributes.Disable_Vertex_Attrib_Array(vPosition);

   exception
      when  others =>
            Put_Line ("An exceptiom occurred in Render.");
            raise;
   end Render;

   --------------------------------------------------------------

   procedure Setup is
      use GL.Types;
      use GL.Objects.Buffers;
      use GL.Objects.Shaders;
      use Program_Loader;

   begin
      Vertex_Array_Object.Initialize_Id;
      Vertex_Array_Object.Bind;

      Vertex_Buffer.Initialize_Id;
      Array_Buffer.Bind(Vertex_Buffer);
      Utilities.Load_Vertex_Buffer_2(Array_Buffer, Vertex_Data.Vertices, Static_Draw);

      Render_Program := Program_From((Src("shaders\gl_1_simple_draw\vertex_shader.glsl", Vertex_Shader),
                                     Src("shaders\gl_1_simple_draw\fragment_shader.glsl", Fragment_Shader)));

   exception
      when others =>
         Put_Line("An exception occured in Setup.");
         raise;
   end Setup;
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

end GL_Tutorials.Ex_1_Simple_Draw;
