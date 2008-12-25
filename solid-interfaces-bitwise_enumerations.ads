generic -- Solid.Interfaces.Bitwise_Enumerations
   type Enumeration is (<>);
   type Modular is mod <>;
package Solid.Interfaces.Bitwise_Enumerations is
   function Valid return Boolean;

   Not_Valid : exception;

   subtype Bitfield is Modular;

   function Is_Set (B : Bitfield; Value : Enumeration) return Boolean;

   type Value_List is array (Positive range <>) of Enumeration;

   function Values (B : Bitfield) return Value_List;

   function Bits (E : Enumeration) return Bitfield;

   function Bits (V : Value_List) return Bitfield;

   function Mask (B : Bitfield; Mask : Value_List) return Enumeration;
end Solid.Interfaces.Bitwise_Enumerations;
