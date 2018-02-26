with Ada.Numerics.Generic_Elementary_Functions;

package body Quaternions is

   package Elementary_Functions is new Ada.Numerics.Generic_Elementary_Functions(Real);

   ---------------
   -- Normalize --
   ---------------

   function Normalize(Q : in Quaternion) return Quaternion
   is
      Length : Real :=  Elementary_Functions.Sqrt(Q.W**2 + Q.X**2 + Q.Y**2 + Q.Z**2 );
   begin
      return (Q.W / Length,
              Q.X / Length,
              Q.Y / Length,
              Q.Z / Length);
   end Normalize;


   ---------------
   -- Normalize --
   ---------------

   procedure Normalize(Q : in out Quaternion)
   is
   begin
      Q := Normalize(Q);
   end Normalize;

   ---------------
   -- Conjugate --
   ---------------

   function Conjugate (Q : Quaternion) return Quaternion is
   begin
      return (Q.W, -Q.X, -Q.Y, -Q.Z);
   end Conjugate;

   ---------
   -- "*" --
   ---------

   function "*" (L,R : Quaternion) return Quaternion is
      W : constant Real := (L.W * R.W) - (L.X * R.X) - (L.Y * R.Y) - (L.Z * R.Z);
      X : constant Real := (L.W * R.X) + (L.X * R.W) + (L.Y * R.Z) - (L.Z * R.Y);
      Y : constant Real := (L.W * R.Y) - (L.X * R.Z) + (L.Y * R.W) + (L.Z * R.X);
      Z : constant Real := (L.W * R.Z) + (L.X * R.Y) - (L.Y * R.X) + (L.Z * R.W);
   begin
      return (W,X,Y,Z);
   end "*";

end Quaternions;
