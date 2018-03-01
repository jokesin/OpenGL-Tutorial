with GL.Types;
with Glfw.Windows;

package GL_3_Restarting_Primitives is

   procedure Main_Loop(Main_Window : in out Glfw.Windows.Window);
   
private

   use GL.Types;
   package Vertex_Data is
      Cube_Positions : constant Singles.Vector4_Array := (
                                                          (-1.0, -1.0, -1.0, 1.0),
                                                          (-1.0, -1.0,  1.0, 1.0),
                                                          (-1.0,  1.0, -1.0, 1.0),
                                                          (-1.0,  1.0,  1.0, 1.0),
                                                          ( 1.0, -1.0, -1.0, 1.0),
                                                          ( 1.0, -1.0,  1.0, 1.0),
                                                          ( 1.0,  1.0, -1.0, 1.0),
                                                          ( 1.0,  1.0,  1.0, 1.0)
                                                         );
   
      Cube_Colors : constant Singles.Vector4_Array := (
                                                       (1.0, 1.0, 1.0, 1.0),
                                                       (1.0, 1.0, 0.0, 1.0),
                                                       (1.0, 0.0, 1.0, 1.0),
                                                       (1.0, 0.0, 0.0, 1.0),
                                                       (0.0, 1.0, 1.0, 1.0),
                                                       (0.0, 1.0, 0.0, 1.0),
                                                       (0.0, 0.0, 1.0, 1.0),
                                                       (0.5, 0.5, 0.5, 1.0)
                                                      );
   
      Cube_Indices : constant UShort_Array := ( 
                                                0, 1, 2, 3, 6, 7, 4, 5, -- Первая полоса
                                                16#FFFF#, -- индекс рестарта
                                                2, 6, 0, 4, 1, 5, 3, 7 -- Вторая полоса
                                               );
   end Vertex_Data;
   
                                              

end GL_3_Restarting_Primitives;
