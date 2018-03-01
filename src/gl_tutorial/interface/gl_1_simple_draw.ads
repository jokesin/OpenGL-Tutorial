with GL.Types;

with Glfw.Windows;

package GL_1_Simple_Draw is

   procedure Main_Loop(Main_Window : in out Glfw.Windows.Window);
   
private
   
   package Vertex_Data is
      use GL.Types;
      
      Vertices : Singles.Vector2_Array :=(
                                          (-0.9, -0.9),--triangle 1
                                          (0.85, -0.9),
                                          (-0.9, 0.85),
                                          (0.9, -0.85),--triangle 2
                                          (0.9, 0.9),
                                          (-0.85, 0.9)
                                         );
   end Vertex_Data;
   

end GL_1_Simple_Draw;
