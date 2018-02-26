with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Buffers;
with GL.Context;

package body Utilities is

   ----------------------------------------------------------------
   procedure Clear_Background_Colour (Colour : GL.Types.Colors.Color) is
    begin
        GL.Buffers.Set_Color_Clear_Value (Colour);
        GL.Buffers.Clear ((False, False, False, True));
   end Clear_Background_Colour;

   ----------------------------------------------------------------

   procedure Show_GL_Data is
      GL_Version               : String := GL.Types.Int'Image(GL.Context.Major_Version) & "." &
        GL.Types.Int'Image(GL.Context.Minor_Version);
      Renderer                 : String := GL.Context.Renderer;
      Shading_Language_Version : String := GL.Context.Primary_Shading_Language_Version;
   begin
      New_Line;
      Put_Line("OpenGL version supported : " & GL_Version);
      Put_Line("Renderer : " & Renderer);
      Put_Line("Primary_Shading_Language_Version : " & Shading_Language_Version);
   end Show_GL_Data;


end Utilities;
