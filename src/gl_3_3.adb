with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw;
with Glfw.Windows;

with Initialize;
with GL_Tutorials.Ex_3_Instanced_Rendering_2;

procedure GL_3_3 is
Main_Window : Glfw.Windows.Window;
   Program_Name : String := "OpenGL Tutorial - Instanced Rendering 2";
   Window_Title : constant String := Program_Name;

begin
   Glfw.Init;
   Initialize(Main_Window, Window_Title);
   
   GL_Tutorials.Ex_3_Instanced_Rendering_2.Main_Loop(Main_Window);

   Glfw.Shutdown;

exception
   when Ex : Constraint_Error =>
      Put(Program_Name & " returned a constrained error : ");
      Put_Line(Exception_Information(Ex));
   when Ex : others =>
      Put("An exception occured in " & Program_Name & " : ");
      Put_Line(Exception_Information(Ex));
   
end GL_3_3;
