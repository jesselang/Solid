with Ada.Numerics.Generic_Complex_Types;
with Ada.Numerics.Generic_Complex_Elementary_Functions;
with Interfaces;

package Solid.Audio.Analysis is
   -- This package provides base types for transforms between the time and frequency domains.
   package Complex   is new Ada.Numerics.Generic_Complex_Types (Solid.Audio.Sample);
   package Functions is new Ada.Numerics.Generic_Complex_Elementary_Functions (Complex);

   subtype List_Index is Interfaces.Unsigned_32;
   type Complex_List is array (List_Index range <>) of Complex.Complex;
   type Complex_Handle is access Complex_List;
end Solid.Audio.Analysis;
