
with System;
-- with Ada.Unchecked_Conversion;

package body Solid.Interfaces.Bitwise_Enumerations is
   Package_Valid : Boolean := False;

   function Valid return Boolean is
   begin -- Valid
      return Package_Valid;
   end Valid;

   -- function To_Bit is new Ada.Unchecked_Conversion (Enumeration, Bitfield);

   function Is_Set (B : Bitfield; Value : Enumeration) return Boolean is
      Value_Bit : constant Bitfield := Value'Enum_Rep; --To_Bit (Value);
   begin -- Is_Set
      return (B and Value_Bit) = Value_Bit;
   end Is_Set;

   function Values (B : Bitfield) return Value_List is
      Result : Value_List (1 .. Enumeration'Pos (Enumeration'Last) + 1);
      Last   : Natural := Natural'First;
   begin -- Values
      for Value in Enumeration'Range loop
         if Is_Set (B, Value => Value) then
            Last := Last + 1;
            Result (Last) := Value;
         end if;
      end loop;

      return Result (Result'First .. Last);
   end Values;

   function Bits (E : Enumeration) return Bitfield is
   begin -- Bits
      return E'Enum_Rep;
   end Bits;

   function Bits (V : Value_List) return Bitfield is
      Result    : Bitfield := Bitfield'First;
      Value     : Enumeration;
      Value_Bit : Bitfield;
   begin -- Bits
      for Index in V'Range loop
         Value := V (Index);
         Value_Bit := Bits (Value); -- Value'Enum_Rep; -- To_Bit (V (Index) );
         Result := Result or Value_Bit;
      end loop;

      return Result;
   end Bits;

   function Mask (B : Bitfield; Mask : Value_List) return Enumeration is
      Mask_Bits : constant Bitfield := Bits (Mask);
      Sum_Values : constant Value_List := Values (B and Mask_Bits);
   begin -- Mask
      if Sum_Values'Length /= 1 then
         raise Constraint_Error with "No values found.";
      else
         return Sum_Values (1);
      end if;

   end Mask;

   Last_Value : constant Enumeration := Enumeration'Last;

   Value_Sum : Bitfield := Bitfield'First;
   Value_Bit : Bitfield := Bitfield'First;
begin -- Solid.Interfaces.Bitwise_Enumerations
   -- Sanity check.
   -- Length check.
   --~ if Last_Value'Enum_Rep > Bitfield'Last then -- GNAT bug box.  Yay.
      --~ null;
   --~ end if;
   -- Value check.
   --~ All_Values : for Value in Enumeration loop
      --~ Value_Bit := Bits (Value);

   --~ end loop All_Values;

   Package_Valid := True;
end Solid.Interfaces.Bitwise_Enumerations;
