with GL.Objects.Buffers;
with Interfaces.C.Pointers;

package GL_Tutorials.Ex_4_Vertex_Colors is

   procedure Main_Loop(Main_Window : in out Glfw.Windows.Window);
   
private
   
   package Vertex_Data is
      use GL.Types;
      
      type Vertex_Type is record
         Color : UBytes.Vector4;
         Position : Singles.Vector2;
      end record;
      
      type Vertex_Array is array (Size range <>) of aliased Vertex_Type
        with Convention => C;      
      
      package Vertex_Pointers is new Interfaces.C.Pointers
        (Index              => Size,
         Element            => Vertex_Type,
         Element_Array      => Vertex_Array,
         Default_Terminator => Vertex_Type'(others=><>));
      
      Vertices : Vertex_Array := (((255,   0,   0, 255),( -0.9,  -0.9)),
                                  ((0,   255,   0, 255),(  0.85, -0.9)),
                                  ((0,     0, 255, 255),( -0.9,   0.85)),
                                  ((10,   10,  10, 255),(  0.9,  -0.85)),
                                  ((100, 100, 100, 255),(  0.9,   0.9)),
                                  ((255, 255, 255, 255),( -0.85,  0.9))
                                 );

      procedure Load_To_Buffer is new
        GL.Objects.Buffers.Load_To_Buffer(Vertex_Pointers);

   end Vertex_Data;
   

end GL_Tutorials.Ex_4_Vertex_Colors;
