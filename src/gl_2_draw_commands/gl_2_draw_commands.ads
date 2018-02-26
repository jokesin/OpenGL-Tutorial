with GL.Types;

with Glfw.Windows;

package GL_2_Draw_Commands is

   procedure Main_Loop(Main_Window : in out Glfw.Windows.Window);

private
   
   package Vertex_Data is

   use GL.Types;
   
   -- 4 вершины
   Vertex_Positions : Singles.Vector4_Array :=(
                                               (-1.0, -1.0, 0.0, 1.0),
                                               ( 1.0, -1.0, 0.0, 1.0),
                                               (-1.0,  1.0, 0.0, 1.0),
                                               (-1.0, -1.0, 0.0, 1.0)
                                              );
   
   -- ÷вета дл€ каждой вершины
   Vertex_Colors : Singles.Vector4_Array :=(
                                            (1.0, 1.0, 1.0, 1.0),
                                            (1.0, 1.0, 0.0, 1.0),
                                            (1.0, 0.0, 1.0, 1.0),
                                            (0.0, 1.0, 1.0, 1.0)
                                           );
   
   -- “ри индекса (мы планируем нарисовать один треугольник)
   Vertex_Indices : UShorts.Vector3_Array :=(
                                             1 => (0,1,2)
                                            );

   end Vertex_Data;
   
end GL_2_Draw_Commands;
