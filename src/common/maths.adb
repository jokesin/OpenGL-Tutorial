with Ada.Numerics;

with Quaternions;

package body Maths is

   use type GL.Types.Singles.Matrix4;
   use type GL.Types.Singles.Vector3;

   Zero_Matrix4 : constant GL.Types.Singles.Matrix4 :=
     (others => (others => 0.0));


   package Single_Quaternion is new Quaternions(GL.Types.Single);

   Radians_Per_Degree : constant Radian := Ada.Numerics.Pi / 180.0;
   Degrees_Per_Radian : constant Degree := 180.0 / Ada.Numerics.Pi;

   -------------
   -- Degrees --
   -------------

   function Degrees (Angle : Radian) return Degree is
   begin
      return Degree(Angle) * Degrees_Per_Radian;
   end Degrees;

   -------------
   -- Radians --
   -------------

   function Radians (Angle : Degree) return Radian is
   begin
      return Radian(Angle) * Radians_Per_Degree;
   end Radians;

   ---------------------
   -- Rotation_Matrix --
   ---------------------

   function Rotation_Matrix
     (Angle : Degree;
      Axis : Singles.Vector3)
      return Singles.Matrix4
   is
   begin
      return Rotation_Matrix(Radians(Angle), Axis);
   end Rotation_Matrix;

   ---------------------
   -- Rotation_Matrix --
   ---------------------

   function Rotation_Matrix
     (Angle : Radian;
      Axis : Singles.Vector3)
      return Singles.Matrix4
   is
      use GL;
      use Maths.Single_Math_Functions;
      use Single_Quaternion;

      aQuaternion : Single_Quaternion.Quaternion;
      theMatrix   : GL.Types.Singles.Matrix4:= GL.Types.Singles.Identity4;
      NQ          : Single_Quaternion.Quaternion;
      Half_Angle  : constant Single := 0.5 * Single(Angle);
      Sine        : constant Single := Sin(Half_Angle);
   begin
      aQuaternion := (Cos(Half_Angle), Axis(GL.X) * Sine, Axis(GL.Y) * Sine, Axis(GL.Z) * Sine);
      NQ := aQuaternion.Normalize;

      theMatrix (X, X) := 1.0 - 2.0 * (NQ.Y ** 2 + NQ.Z ** 2);
      theMatrix (Y, X) := 2.0 * (NQ.X * NQ.Y - NQ.W * NQ.Z);
      theMatrix (Z, X) := 2.0 * (NQ.X * NQ.Z + NQ.W * NQ.Y);

      theMatrix (X, Y) := 2.0 * (NQ.X * NQ.Y + NQ.W * NQ.Z);
      theMatrix (Y, Y) := 1.0 - 2.0 * (NQ.X ** 2 + NQ.Z ** 2);
      theMatrix (Z, Y) := 2.0 * (NQ.Y * NQ.Z - NQ.W * NQ.X);

      theMatrix (X, Z) := 2.0 * (NQ.X * NQ.Z - NQ.W * NQ.Y);
      theMatrix (Y, Z) := 2.0 * (NQ.Y * NQ.Z + NQ.W * NQ.X);
      theMatrix (Z, Z) := 1.0 - 2.0 * (NQ.X ** 2 + NQ.Y ** 2);
      return theMatrix;
   end Rotation_Matrix;

   --------------------
   -- Scaling_Matrix --
   --------------------

   function Scaling_Matrix
     (Scale_Factor : Singles.Vector3)
      return Singles.Matrix4
   is
      use GL;
      theMatrix : Singles.Matrix4 := Singles.Identity4;
   begin
      theMatrix(X, X) := Scale_Factor(X);
      theMatrix(Y, Y) := Scale_Factor(Y);
      theMatrix(Z, Z) := Scale_Factor(Z);
      return theMatrix;
   end Scaling_Matrix;

   ------------------------
   -- Translation_Matrix --
   ------------------------

   function Translation_Matrix
     (Change : Singles.Vector3)
      return Singles.Matrix4
   is
      use GL;
      theMatrix : Singles.Matrix4 := Singles.Identity4;
   begin
      theMatrix(W, X) := Change(X);
      theMatrix(W, Y) := Change(Y);
      theMatrix(W, Z) := Change(Z);
      return theMatrix;
   end Translation_Matrix;

   ------------------------
   -- Perspective_Matrix --
   ------------------------

   function Perspective_Matrix
     (Top, Bottom, Left, Right, Near, Far : Single)
      return GL.Types.Singles.Matrix4
   is
      use GL;
      dX : constant Single := Right - Left;
      dY : constant Single := Top - Bottom;
      dZ : constant Single := Far - Near;
      Matrix : Gl.Types.Singles.Matrix4 := Zero_Matrix4;
   begin
      Matrix(X, X) := 2.0 * Near / dX;
      Matrix(Z, X) := (Right + Left) / dX;
      Matrix(Y, Y) := 2.0 * Near / dY;
      Matrix(Z, Y) := (Top + Bottom) / dY;
      Matrix(Z, Z) := -(Far + Near) / dZ;
      Matrix(W, Z) := -2.0 * Far * Near / dZ;
      Matrix(Z, W) := -1.0;
      return Matrix;
   end Perspective_Matrix;

end Maths;
