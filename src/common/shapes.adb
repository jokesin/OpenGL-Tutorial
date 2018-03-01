with Ada.Unchecked_Deallocation;
with Ada.Numerics; use Ada.Numerics;
with GL; use GL;
with GL.Types; use GL.Types;

with Maths; use Maths;

package body Shapes is

   use Maths.Single_Math_Functions;

   function Init(Radius : GL.Types.Single;
                 H_Sectors : Face_Count) return Sphere is

      Vertex_Count : Int := H_Sectors + 2;
      Index_Count  : Int := H_Sectors * 6;
      ------------------------------------------------------------------------
      function Gen_Vertices return Shape_Vertex_Array_Ptr is
         Shape_Vertices : Shape_Vertex_Array_Ptr := new Shape_Vertex_Array(1..Vertex_Count);

         H_Angle_Step : Radian := Radians(360.0 / Degree(H_Sectors));
         H_Angle_Start : Radian := -Pi / 2.0; -- берем относительно oX-oZ

         I : Int := Shape_Vertices'First+1;
      begin

         -- выставим верхнюю и нижнюю вершины
         Shape_Vertices(Shape_Vertices'First) := ( 0.0, Radius, 0.0, 1.0);
         Shape_Vertices(Shape_Vertices'Last)  := ( 0.0, -Radius, 0.0, 1.0);

         -- рассчитаем промежуточные значения
         for K in 1..Int(Shape_Vertices'Length-2) loop
            Shape_Vertices(I)(X) := -Radius * Cos(Single(H_Angle_Start + Radian((K - 1)) * H_Angle_Step));
            Shape_Vertices(I)(Y) := 0.0;
            Shape_Vertices(I)(Z) := Radius * Sin(Single(H_Angle_Start + Radian((K - 1)) * H_Angle_Step));
            Shape_Vertices(I)(W) := 1.0;
            I := I+1;
         end loop;

         return Shape_Vertices;
      end Gen_Vertices;

      ------------------------------------------------------------------------
      function Gen_Normals(Vertices : Shape_Vertex_Array_Ptr) return Shape_Normal_Array_Ptr is
         Shape_Normals : Shape_Normal_Array_Ptr := new Shape_Normal_Array(1..Vertex_Count);
      begin
         --в случае сферической поверхности мы можем просто использовать координаты вершин
         for K in Shape_Normals'Range loop
            Shape_Normals(K) := (Vertices(K)(X), Vertices(K)(Y),Vertices(K)(Z));
         end loop;

         return Shape_Normals;
      end Gen_Normals;

      ------------------------------------------------------------------------

      function Gen_Indices return Shape_Indices_Array_Ptr is
         Shape_Indices : Shape_Indices_Array_Ptr := new Shape_Indices_Array(1..Index_Count);
         I : Int := Shape_Indices'First;
      begin
         -- установим "верх"
         for K in 1..H_Sectors loop
            Shape_Indices(I) := UShort(K);
            if (K + 1) = Vertex_Count - 1 then
               Shape_Indices(I+1) := 1;
            else
               Shape_Indices(I+1) := UShort(K) + 1;
            end if;
            Shape_Indices(I+2) := 0;
            I := I+3;
         end loop;

         -- установим "низ"
         I := Shape_Indices'Length - H_Sectors * 3 + 1;
         for K in 1..H_Sectors loop
            Shape_Indices(I) := UShort(K);
            Shape_Indices(I+1) := UShort(Vertex_Count) - 1;
            if (K + 1) = Vertex_Count - 1 then
               Shape_Indices(I+2) := 1;
            else
               Shape_Indices(I+2) := UShort(K) + 1;
            end if;
            I := I+3;
         end loop;

        return Shape_Indices;

      end Gen_Indices;

      ------------------------------------------------------------------------

      New_Sphere : Sphere := new Sphere_Type'(Ada.Finalization.Controlled with
                                              Vertex_Count => Vertex_Count,
                                              Index_Count  => Index_Count,
                                              Vertices     => <>,
                                              Normals      => <>,
                                              Indices      => <>);

   begin
      New_Sphere.Vertices:= Gen_Vertices;
      New_Sphere.Normals := Gen_Normals(New_Sphere.Vertices);
      New_Sphere.Indices := Gen_Indices;

      return New_Sphere;
   end Init;

   function Get_Vertices
     (Object : access Sphere_Type)
      return Shape_Vertex_Array
   is
   begin
      return Object.Vertices.all;
   end Get_Vertices;

   function Get_Normals(Object : access Sphere_Type) return Shape_Normal_Array is
   begin
      return Object.Normals.all;
   end Get_Normals;

   function Get_Indices (Object : access Sphere_Type) return Shape_Indices_Array is
   begin
      return Object.Indices.all;
   end Get_Indices;

   overriding
   procedure Finalize (Object : in out Sphere_Type) is
      procedure Free_Vertices is new Ada.Unchecked_Deallocation(Object => Shape_Vertex_Array,
                                                                Name   => Shape_Vertex_Array_Ptr);
      procedure Free_Normals is new Ada.Unchecked_Deallocation(Object => Shape_Normal_Array,
                                                               Name   => Shape_Normal_Array_Ptr);
      procedure Free_Indices is new Ada.Unchecked_Deallocation(Object => Shape_Indices_Array,
                                                               Name   => Shape_Indices_Array_Ptr);
   begin
      Free_Vertices(Object.Vertices);
      Free_Normals(Object.Normals);
      Free_Indices(Object.Indices);
   end Finalize;


end Shapes;
