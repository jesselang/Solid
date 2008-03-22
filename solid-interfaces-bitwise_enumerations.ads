generic -- Solid.Interfaces.Bitwise_Enumerations
   type Enumeration is (<>);
package Solid.Interfaces.Bitwise_Enumerations is
   type Bitfield is mod 2 ** 32;

   function Is_Set (B : Bitfield; Value : Enumeration) return Boolean;

   type Value_List is array (Positive range <>) of Enumeration;

   function Values (B : Bitfield) return Value_List;

   function Bits (V : Value_List) return Bitfield;
end Solid.Interfaces.Bitwise_Enumerations;
