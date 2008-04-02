generic -- Solid.Interfaces.Bitwise_Enumerations
   type Enumeration is (<>);
   type Bitfield is mod <>;
package Solid.Interfaces.Bitwise_Enumerations is

   function Is_Set (B : Bitfield; Value : Enumeration) return Boolean;

   type Value_List is array (Positive range <>) of Enumeration;

   function Values (B : Bitfield) return Value_List;

   function Bits (E : Enumeration) return Bitfield;

   function Bits (V : Value_List) return Bitfield;
end Solid.Interfaces.Bitwise_Enumerations;
