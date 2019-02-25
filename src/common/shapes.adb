with Ada.Unchecked_Deallocation;
with Ada.Numerics; use Ada.Numerics;
with GL; use GL;
with GL.Objects.Buffers;
with GL.Types; use GL.Types;

with Maths; use Maths;

package body Shapes is

   use Maths.Single_Math_Functions;

   function Init(Radius : GL.Types.Single;
                 H_Sectors : Face_Count;
                 V_Edges : Edge_Count := 2) return Sphere is

      Vertex_Count : Int := H_Sectors * (V_Edges - 1) + 2;
      Index_Count  : Int := H_Sectors * 6 * (V_Edges - 1);
      ------------------------------------------------------------------------
      function Gen_Vertices return Shape_Vertex_Array_Ptr is
         Shape_Vertices : Shape_Vertex_Array_Ptr := new Shape_Vertex_Array(1..Vertex_Count);

         H_Angle_Step : Radian := Radians(360.0 / Degree(H_Sectors));
         H_Angle_Start : Radian := -Pi / 2.0; -- берем относительно oX-oZ

         V_Angle_Start : Radian := 0.0;
         V_Angle_Step : Radian := Radians(180.0 / Degree(V_Edges));
         I : Int := Shape_Vertices'First + 1;
         J : Int := 1;
      begin

         -- Выставим верхние и нижние значения
         Shape_Vertices(Shape_Vertices'First) := (0.0,  Radius, 0.0, 1.0);
         Shape_Vertices(Shape_Vertices'Last)  := (0.0, -Radius, 0.0, 1.0);

         -- рассчитаем значения X,Y и Z, выставим W ( сверху вниз )
         -- прим. : если не умножать значения X и Z дополнительно на синусы, то получается труба (цилиндр)
         for K in 1..Shape_Vertices'Length - 2 loop
            Shape_Vertices(I)(X) := -Radius * Cos(Single(H_Angle_Start + Radian((K - 1)) * H_Angle_Step)) *
              Sin(Single(V_Angle_Start + Radian(J) * V_Angle_Step));
            Shape_Vertices(I)(Y) :=  Radius * Cos(Single(V_Angle_Start + Radian(J) * V_Angle_Step));
            Shape_Vertices(I)(Z) :=  Radius * Sin(Single(H_Angle_Start + Radian((K - 1)) * H_Angle_Step)) *
              Sin(Single(V_Angle_Start + Radian(J) * V_Angle_Step));
            Shape_Vertices(I)(W) :=  1.0;
            I := I+1;

            -- сделаем шаг для вычисления значений Y
            if K mod Integer(H_Sectors) = 0 then
               J := J + 1;
            end if;

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
            Shape_Indices(I) := UInt(K);
            if K = H_Sectors then
               Shape_Indices(I+1) := 1;
            else
               Shape_Indices(I+1) := UInt(K) + 1;
            end if;
            Shape_Indices(I+2) := 0;
            I := I + 3;
         end loop;

         -- установим промежуточные значения
         for K in 1..H_Sectors * (V_Edges - 2) loop

            if K = 1 then
               Shape_Indices(I) := UInt(K * H_Sectors) + 1;
            else
               Shape_Indices(I) := Shape_Indices(I-6) + 1;
            end if;

            -- последняя грань текущей полосы?
            if K mod H_Sectors = 0 then
               Shape_Indices(I+1) := Shape_Indices(I) - UInt(H_Sectors - 1);
               Shape_Indices(I+2) := Shape_Indices(I+1) - 1;
            else
               Shape_Indices(I+1) := Shape_Indices(I) + 1;
               Shape_Indices(I+2) := Shape_Indices(I) - UInt(H_Sectors);
            end if;

            Shape_Indices(I+3) := Shape_Indices(I+2);
            Shape_Indices(I+4) := Shape_Indices(I+1);
            -- последняя грань текущей полосы?
            if K mod H_Sectors = 0 then
               Shape_Indices(I+5) := Shape_Indices(I+4) - UInt(H_Sectors);
            else
               Shape_Indices(I+5) := Shape_Indices(I+2) + 1;
            end if;

            I := I + 6;
         end loop;


         -- установим "низ"
         for K in 1..H_Sectors loop
            Shape_Indices(I) := UInt(K + H_Sectors * (V_Edges - 2));

            Shape_Indices(I+1) := UInt(Vertex_Count) - 1;
            if K = H_Sectors then
               Shape_Indices(I+2) := Shape_Indices((I+2)-H_Sectors*3+1);
            else
               Shape_Indices(I+2) := Shape_Indices(I) + 1;
            end if;
            I := I + 3;
         end loop;

        return Shape_Indices;

      end Gen_Indices;

      ------------------------------------------------------------------------

      New_Sphere : Sphere := new Sphere_Type'(Ada.Finalization.Controlled with
                                              Vertex_Count => Vertex_Count,
                                              Index_Count  => Index_Count,
                                              Radius => Radius,
                                              Vertices     => <>,
                                              Normals      => <>,
                                              Indices      => <>);

   begin
      New_Sphere.Vertices:= Gen_Vertices;
      New_Sphere.Normals := Gen_Normals(New_Sphere.Vertices);
      New_Sphere.Indices := Gen_Indices;

      return New_Sphere;
   end Init;


   --------------------------------------------------------------------------------------------
   --------------------------------------------------------------------------------------------


   function Init(Height    : GL.Types.Single;
                 Radius    : GL.Types.Single;
                 H_Sectors : Face_Count;
                 Edge_Div  : Edge_Div_Count := 1)
                 return Cylinder
   is
      Vertex_Count : Int := H_Sectors * (Edge_Div + 1) + 2;
      Index_Count  : Int := H_Sectors * 6 * (Edge_Div + 1);

      ------------------------------------------------------------------------
      function Gen_Vertices return Shape_Vertex_Array_Ptr is
         Shape_Vertices : Shape_Vertex_Array_Ptr := new Shape_Vertex_Array(1..Vertex_Count);

         Cyl_Height : Single := Height / 2.0;

         H_Angle_Step : Radian := Radians(360.0 / Degree(H_Sectors));
         H_Angle_Start : Radian := -Pi / 2.0; -- берем относительно oX-oZ

         V_Angle_Start : Radian := 0.0;
         V_Angle_Step : Radian := Radians(180.0 / Degree(Edge_Div));
         I : Int := Shape_Vertices'First;
         J : Int := 0;
      begin

         -- Выставим верхние и нижние значения (центры)
         Shape_Vertices(Shape_Vertices'First) := (0.0,  Cyl_Height, 0.0, 1.0);
         Shape_Vertices(Shape_Vertices'Last)  := (0.0, -Cyl_Height, 0.0, 1.0);

         -- рассчитаем значения X,Y и Z, выставим W ( сверху вниз )
         -- прим. : если не умножать значения X и Z дополнительно на синусы, то получается труба (цилиндр)
         for K in 1..Shape_Vertices'Length - 2 loop
            I := I+1;
            Shape_Vertices(I)(X) := -Radius * Cos(Single(H_Angle_Start + Radian((K - 1)) * H_Angle_Step));
            Shape_Vertices(I)(Y) :=  Cyl_Height * Cos(Single(V_Angle_Start + Radian(J) * V_Angle_Step));
            Shape_Vertices(I)(Z) :=  Radius * Sin(Single(H_Angle_Start + Radian((K - 1)) * H_Angle_Step));
            Shape_Vertices(I)(W) :=  1.0;

            -- сделаем шаг для вычисления значений Y
            if K mod Integer(H_Sectors) = 0 then
               J := J + 1;
            end if;

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
            Shape_Indices(I) := UInt(K);
            if K = H_Sectors then
               Shape_Indices(I+1) := 1;
            else
               Shape_Indices(I+1) := UInt(K) + 1;
            end if;
            Shape_Indices(I+2) := 0;
            I := I + 3;
         end loop;

         -- установим промежуточные значения
         for K in 1..H_Sectors * Edge_Div loop

            if K = 1 then
               Shape_Indices(I) := UInt(K * H_Sectors) + 1;
            else
               Shape_Indices(I) := Shape_Indices(I-6) + 1;
            end if;

            -- последняя грань текущей полосы?
            if K mod H_Sectors = 0 then
               Shape_Indices(I+1) := Shape_Indices(I) - UInt(H_Sectors - 1);
               Shape_Indices(I+2) := Shape_Indices(I+1) - 1;
            else
               Shape_Indices(I+1) := Shape_Indices(I) + 1;
               Shape_Indices(I+2) := Shape_Indices(I) - UInt(H_Sectors);
            end if;

            Shape_Indices(I+3) := Shape_Indices(I+2);
            Shape_Indices(I+4) := Shape_Indices(I+1);
            -- последняя грань текущей полосы?
            if K mod H_Sectors = 0 then
               Shape_Indices(I+5) := Shape_Indices(I+4) - UInt(H_Sectors);
            else
               Shape_Indices(I+5) := Shape_Indices(I+2) + 1;
            end if;

            I := I + 6;
         end loop;


         -- установим "низ"
         for K in 1..H_Sectors loop
            if K = H_Sectors then
               Shape_Indices(I) := UInt(H_Sectors * Edge_Div + 1);
            else
               Shape_Indices(I) := UInt(K + H_Sectors * Edge_Div + 1);
            end if;
            Shape_Indices(I+1) := UInt((Vertex_Count - 1) - H_Sectors + (K - 1));

            Shape_Indices(I+2) := UInt(Vertex_Count - 1);

            I := I + 3;
         end loop;

        return Shape_Indices;

      end Gen_Indices;

      ------------------------------------------------------------------------
      New_Cylinder : Cylinder := new Cylinder_Type'(Ada.Finalization.Controlled with
                                                    Vertex_Count => Vertex_Count,
                                                    Index_Count  => Index_Count,
                                                    Vertices     => <>,
                                                    Normals      => <>,
                                                    Indices      => <>);
   begin
      New_Cylinder.Vertices := Gen_Vertices;
      New_Cylinder.Normals := Gen_Normals(New_Cylinder.Vertices);
      New_Cylinder.Indices := Gen_Indices;
      return New_Cylinder;
   end Init;


   ----------------------------------
   ----- COMMON METHODS -------------
   ----------------------------------
   function Get_Vertices
     (Object : access Polyhedron_Type)
      return Shape_Vertex_Array
   is
   begin
      return Object.Vertices.all;
   end Get_Vertices;

   function Get_Normals(Object : access Polyhedron_Type) return Shape_Normal_Array is
   begin
      return Object.Normals.all;
   end Get_Normals;

   function Get_Indices (Object : access Polyhedron_Type) return Shape_Indices_Array is
   begin
      return Object.Indices.all;
   end Get_Indices;

   function Get_Indices(Object : access Polyhedron_Type) return Shape_Indices_Array_Ptr is
   begin
      return Object.Indices;
   end Get_Indices;

   procedure Draw_Elements_Instanced(Object : access Polyhedron_Type;
                                     Prim_Count : GL.Types.Size) is
   begin
      GL.Objects.Buffers.Draw_Elements_Instanced(Triangles, UInt(Object.Index_Count), UInt_Type, 0, 1);
   end Draw_Elements_Instanced;

   overriding
   procedure Finalize (Object : in out Polyhedron_Type) is
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
