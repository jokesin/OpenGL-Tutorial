with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw;
with Glfw.Windows;

with Initialize;
with GL_1_Simple_Draw;
with GL_2_Draw_Commands;
with GL_3_Restarting_Primitives;

procedure OGL_Main is
   Main_Window : Glfw.Windows.Window;
   Program_Name : String := "Example 2";
   Window_Title : constant String := Program_Name;

begin
   Glfw.Init;
   Initialize(Main_Window, Window_Title);
   --GL_1_Simple_Draw.Main_Loop(Main_Window);
   --GL_2_Draw_Commands.Main_Loop(Main_Window);
   GL_3_Restarting_Primitives.Main_Loop(Main_Window);

   Glfw.Shutdown;

exception
   when Ex : Constraint_Error =>
      Put(Program_Name & " returned a constrained error : ");
      Put_Line(Exception_Information(Ex));
   when Ex : others =>
      Put("An exception occured in " & Program_Name & " : ");
      Put_Line(Exception_Information(Ex));
end OGL_Main;
