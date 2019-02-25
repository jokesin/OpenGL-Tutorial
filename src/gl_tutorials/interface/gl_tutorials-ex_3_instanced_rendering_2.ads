package GL_Tutorials.Ex_3_Instanced_Rendering_2 is

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
   
      Cube_Normals : constant Singles.Vector3_Array := (
                                                        (-1.0, -1.0, -1.0),
                                                        (-1.0, -1.0,  1.0),
                                                        (-1.0,  1.0, -1.0),
                                                        (-1.0,  1.0,  1.0),
                                                        ( 1.0, -1.0, -1.0),
                                                        ( 1.0, -1.0,  1.0),
                                                        ( 1.0,  1.0, -1.0),
                                                        ( 1.0,  1.0,  1.0)
                                                       );      
   
      Cube_Indices : constant UShort_Array := ( 
                                                0, 1, 2, 3, 6, 7, 4, 5, -- ������ ������
                                                16#FFFF#, -- ������ ��������
                                                2, 6, 0, 4, 1, 5, 3, 7 -- ������ ������
                                               );
   end Vertex_Data;
    

end GL_Tutorials.Ex_3_Instanced_Rendering_2;
