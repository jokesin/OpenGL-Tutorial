with Ada.Finalization;
with GL.Types;
package Shapes is

   subtype Face_Count is GL.Types.Int range 3..GL.Types.Int'Last;
   subtype Edge_Div_Count is GL.Types.Int range 1..GL.Types.Int'Last;
   subtype Edge_Count is GL.Types.Int range 2..GL.Types.Int'Last;
   
   type Polyhedron_Type(Vertex_Count : GL.Types.Int;
                        Index_Count : GL.Types.Int)
   is abstract new Ada.Finalization.Controlled with private;
   
   type Polyhedron is access all Polyhedron_Type;
   
   subtype Shape_Vertex_Array is GL.Types.Singles.Vector4_Array;
   type Shape_Vertex_Array_Ptr is access all Shape_Vertex_Array;
   subtype Shape_Normal_Array is GL.Types.Singles.Vector3_Array;
   type Shape_Normal_Array_Ptr is access all Shape_Normal_Array;
   subtype Shape_Indices_Array is GL.Types.UInt_Array;
   type Shape_Indices_Array_Ptr is access all Shape_Indices_Array;
   
   function Get_Vertices(Object : access Polyhedron_Type) return Shape_Vertex_Array;
   function Get_Normals(Object : access Polyhedron_Type) return Shape_Normal_Array;
   function Get_Indices(Object : access Polyhedron_Type) return Shape_Indices_Array;
   function Get_Indices(Object : access Polyhedron_Type) return Shape_Indices_Array_Ptr;
   procedure Draw_Elements_Instanced(Object : access Polyhedron_Type;
                                     Prim_Count : GL.Types.Size);
   
   type Sphere_Type is new Polyhedron_Type with private;
   type Sphere is access all Sphere_Type;
   
   function Init(Radius    : GL.Types.Single;
                 H_Sectors : Face_Count;
                 V_Edges   : Edge_Count := 2) 
                 return Sphere;
   
   type Cylinder_Type is new Polyhedron_Type with private;
   type Cylinder is access all Cylinder_Type;
   
   function Init(Height    : GL.Types.Single;
                 Radius    : GL.Types.Single;
                 H_Sectors : Face_Count;
                 Edge_Div  : Edge_Div_Count := 1) 
                 return Cylinder;
   

private
   use GL.Types;
   
   type Polyhedron_Type(Vertex_Count : GL.Types.Int;
                        Index_Count : GL.Types.Int)
   is new Ada.Finalization.Controlled with
      record
         Vertices : Shape_Vertex_Array_Ptr;
         Normals  : Shape_Normal_Array_Ptr;
         Indices  : Shape_Indices_Array_Ptr;
      end record;
   
   overriding
   procedure Finalize (Object : in out Polyhedron_Type);
   
   type Sphere_Type is new Polyhedron_Type with
      record
         Radius : Single;
      end record;
   
   type Cylinder_Type is new Polyhedron_Type with null record;
   
end Shapes;
