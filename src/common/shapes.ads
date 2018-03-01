with Ada.Finalization;
with GL.Types;
package Shapes is

   type Sphere_Type(Vertex_Count : GL.Types.Int;
                    Index_Count : GL.Types.Int)
   is new Ada.Finalization.Controlled with private;
   
   type Sphere is access all Sphere_Type;
   
   subtype Shape_Vertex_Array is GL.Types.Singles.Vector4_Array;
   type Shape_Vertex_Array_Ptr is access all Shape_Vertex_Array;
   subtype Shape_Normal_Array is GL.Types.Singles.Vector3_Array;
   type Shape_Normal_Array_Ptr is access all Shape_Normal_Array;
   subtype Shape_Indices_Array is GL.Types.UShort_Array;
   type Shape_Indices_Array_Ptr is access all Shape_Indices_Array;

   subtype Face_Count is GL.Types.Int range 3..GL.Types.Int'Last;
   function Init(Radius : GL.Types.Single;
                 H_Sectors : Face_Count) return Sphere;
   function Get_Vertices(Object : access Sphere_Type) return Shape_Vertex_Array;
   function Get_Normals(Object : access Sphere_Type) return Shape_Normal_Array;
   function Get_Indices(Object : access Sphere_Type) return Shape_Indices_Array;
   
private
   use GL.Types;
   
   type Sphere_Type(Vertex_Count : GL.Types.Int;
                    Index_Count : GL.Types.Int)
   is new Ada.Finalization.Controlled with
      record
         Vertices : Shape_Vertex_Array_Ptr;
         Normals  :Shape_Normal_Array_Ptr;
         Indices  : Shape_Indices_Array_Ptr;
      end record;
   
   overriding
   procedure Finalize (Object : in out Sphere_Type);
   

end Shapes;
