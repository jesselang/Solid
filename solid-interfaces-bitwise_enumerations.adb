-- with Ada.Unchecked_Conversion;

package body Solid.Interfaces.Bitwise_Enumerations is
   -- function To_Bit is new Ada.Unchecked_Conversion (Enumeration, Bitfield);

   function Is_Set (B : Bitfield; Value : Enumeration) return Boolean is
      Value_Bit : constant Bitfield := Value'Enum_Rep; --To_Bit (Value);
   begin -- Is_Set
      return (B and Value_Bit) = Value_Bit;
   end Is_Set;

   function Values (B : Bitfield) return Value_List is
      Result : Value_List (1 .. Enumeration'Pos (Enumeration'Last) + 1);
      Last   : Natural := 0;
   begin -- Values
      for Value in Enumeration'Range loop
         if Is_Set (B, Value => Value) then
            Last := Last + 1;
            Result (Last) := Value;
         end if;
      end loop;

      return Result (Result'First .. Last);
   end Values;

   function Bits (V : Value_List) return Bitfield is
      Result    : Bitfield := 0;
      Value     : Enumeration;
      Value_Bit : Bitfield;
   begin -- Bits
      for Index in V'Range loop
         Value := V (Index);
         Value_Bit := Value'Enum_Rep; -- To_Bit (V (Index) );
         Result := Result or Value_Bit;
      end loop;

      return Result;
   end Bits;
end Solid.Interfaces.Bitwise_Enumerations;
