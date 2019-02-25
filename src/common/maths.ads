with Interfaces.C.Pointers;

with Ada.Numerics.Generic_Elementary_Functions;

with GL.Types; use GL.Types;

package Maths is
   
   package Single_Math_Functions is new
     Ada.Numerics.Generic_Elementary_Functions (GL.Types.Single);
   
   type Degree is new Single;
   type Radian is new Single;
   
   function Degrees (Angle : Radian) return Degree;
   function Radians (Angle : Degree) return Radian;
   
   function Rotation_Matrix (Angle : Degree; Axis : Singles.Vector3)
                             return Singles.Matrix4;
   
   function Rotation_Matrix (Angle : Radian; Axis : Singles.Vector3)
                             return Singles.Matrix4;
   function Scaling_Matrix (Scale_Factor : Singles.Vector3) return Singles.Matrix4;
   function Translation_Matrix (Change : Singles.Vector3)
                                return Singles.Matrix4;
   function Perspective_Matrix (Top, Bottom, Left, Right, Near, Far : Single)
                                return GL.Types.Singles.Matrix4;
   
end Maths;
