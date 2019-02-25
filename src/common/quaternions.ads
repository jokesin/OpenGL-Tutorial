generic
   type Real is digits <>;
package Quaternions is

   type Quaternion is tagged
      record
         W,X,Y,Z : Real;
      end record;

   procedure Normalize(Q : in out Quaternion);
   function Normalize(Q : in Quaternion) return Quaternion;
   function Conjugate(Q : Quaternion) return Quaternion;
   function "*"(L,R : Quaternion) return Quaternion;
end Quaternions;
