with Ada.Unchecked_Conversion;
with System.Storage_Elements;

package body Solid.Platform.Endian is
   use type System.Storage_Elements.Storage_Offset;

   Byte_Size : constant System.Storage_Elements.Storage_Offset := (Discrete'Size + System.Storage_Unit - 1) / System.Storage_Unit;

   function Swap (From : Discrete) return Discrete is
      subtype Byte_List is System.Storage_Elements.Storage_Array (1 .. Byte_Size);

      function To_Bytes    is new Ada.Unchecked_Conversion (Source => Discrete,  Target => Byte_List);
      function To_Discrete is new Ada.Unchecked_Conversion (Source => Byte_List, Target => Discrete);

      Byte : Byte_List                              := To_Bytes (From);
      Last : System.Storage_Elements.Storage_Offset := Byte_Size;
      Temp : System.Storage_Elements.Storage_Element;
   begin -- Swap
      All_Pairs : for First in Byte'First .. Byte_Size / 2 loop
         Temp         := Byte (First);
         Byte (First) := Byte (Last);
         Byte (Last)  := Temp;

         Last := Last - 1;
      end loop All_Pairs;

      return To_Discrete (Byte);
   end Swap;

   use type System.Bit_Order;

   function To_Big (From : Discrete) return Discrete is
   begin -- To_Big
      if System.Default_Bit_Order = System.Low_Order_First then
         return Swap (From);
      end if;

      return From;
   end To_Big;

   function To_Little (From : Discrete) return Discrete is
   begin -- To_Little
      if System.Default_Bit_Order = System.High_Order_First then
         return Swap (From);
      end if;

      return From;
   end To_Little;
end Solid.Platform.Endian;
