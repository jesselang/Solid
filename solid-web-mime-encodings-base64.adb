with Ada.Unchecked_Conversion;
with Solid.Text_Streams;
with System;

package body Solid.Web.MIME.Encodings.Base64 is
   type Base64_Index is range 0 .. 63;
   for Base64_Index'Size use 6;

   subtype Data_Index is Ada.Streams.Stream_Element_Offset range 0 .. 63;

   Pad_Data    : constant Ada.Streams.Stream_Element                    := Ada.Streams.Stream_Element'Val (Character'Pos ('=') );
   Base64_Data : constant Ada.Streams.Stream_Element_Array (Data_Index) :=
      Solid.Text_Streams.To_Stream ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/");

   subtype Encoded_Block is Ada.Streams.Stream_Element_Array (1 .. 4);

   subtype Block_Bound is Ada.Streams.Stream_Element_Offset range 1 .. 3;

   type Uncoded_Block is record
      One, Two, Three: Ada.Streams.Stream_Element;
      Last : Block_Bound;
   end record;
   for Uncoded_Block'Bit_Order use System.High_Order_First; -- Representation clause is required to use 'Bit_Order.
   for Uncoded_Block use record
      One   at 0 range  0 ..  7;
      Two   at 0 range  8 .. 15;
      Three at 0 range 16 .. 23;
      Last  at 0 range 24 .. 31; -- Tack this on the end.
   end record;
   pragma Pack (Uncoded_Block);

   subtype Index_Bound is Ada.Streams.Stream_Element_Offset range 1 .. 4;

   type Index_Block is record
      One, Two, Three, Four : Base64_Index;
      Last : Index_Bound;
   end record;
   for Index_Block'Bit_Order use System.High_Order_First; -- Representation clause is required to use 'Bit_Order.
   for Index_Block use record
      One   at 0 range  0 ..  5;
      Two   at 0 range  6 .. 11;
      Three at 0 range 12 .. 17;
      Four  at 0 range 18 .. 23;
      Last  at 0 range 24 .. 31; -- Tack this on the end.
   end record;
   pragma Pack (Index_Block);

   function Encode (Item : Ada.Streams.Stream_Element_Array) return Ada.Streams.Stream_Element_Array is
      function Encoded_Length return Ada.Streams.Stream_Element_Offset;
      -- returns the length that Item will be once encoded.

      function Encoded_Length return Ada.Streams.Stream_Element_Offset is
         Length : Ada.Streams.Stream_Element_Offset;

         use type Ada.Streams.Stream_Element_Offset;
      begin -- Encoded_Length
         Length := Item'Length * 4 / 3;

         loop
            exit when Length rem 4 = 0;

            Length := Length + 1;
         end loop;

         return Length;
      end Encoded_Length;

      procedure Encode (Uncoded : in Uncoded_Block; Encoded : out Encoded_Block);

      procedure Encode (Uncoded : in Uncoded_Block; Encoded : out Encoded_Block) is
         Indices : Index_Block;
         for Indices'Address use Uncoded'Address; -- Overlay Indices upon Uncoded.

         use type Ada.Streams.Stream_Element_Offset;
      begin -- Encode
         Encoded (Encoded'First)     := Base64_Data (Data_Index (Indices.One) );
         Encoded (Encoded'First + 1) := Base64_Data (Data_Index (Indices.Two) );

         if Uncoded.Last > 1 then
            Encoded (Encoded'First + 2) := Base64_Data (Data_Index (Indices.Three) );

            if Uncoded.Last > 2 then
               Encoded (Encoded'First + 3) := Base64_Data (Data_Index (Indices.Four) );
            else
               Encoded (Encoded'First + 3) := Pad_Data;
            end if;
         else
            Encoded (Encoded'First + 2) := Pad_Data;
            Encoded (Encoded'First + 3) := Pad_Data;
         end if;
      end Encode;

      procedure Prepare (Item : in Ada.Streams.Stream_Element_Array; Uncoded : out Uncoded_Block);
      -- Sets Uncoded to contain the value of Item, padding with zeros as needed.
      -- Raises Invalid_Length if Item's length is invalid.

      procedure Prepare (Item : in Ada.Streams.Stream_Element_Array; Uncoded : out Uncoded_Block) is
         use type Ada.Streams.Stream_Element_Offset;
      begin -- Prepare
         Uncoded.Two   := 0; -- Initialize the second and third components, which may not be used.
         Uncoded.Three := 0;

         case Item'Length is
            when 1 =>
               Uncoded.One   := Item (Item'First);
            when 2 =>
               Uncoded.One   := Item (Item'First);
               Uncoded.Two   := Item (Item'First + 1);
            when 3 =>
               Uncoded.One   := Item (Item'First);
               Uncoded.Two   := Item (Item'First + 1);
               Uncoded.Three := Item (Item'First + 2);
            when others =>
               raise Invalid_Length;
         end case;

         Uncoded.Last := Item'Length; -- Let the case statement handle an invalid length.
      end Prepare;

      use type Ada.Streams.Stream_Element_Offset;

      Result       : Ada.Streams.Stream_Element_Array (1 .. Encoded_Length);
      Uncoded      : Uncoded_Block;
      Item_Start   : Ada.Streams.Stream_Element_Offset := Item'First;
      Item_End     : Ada.Streams.Stream_Element_Offset := Item'First + 2; -- 3 elements.
      Result_Start : Ada.Streams.Stream_Element_Offset := Result'First;
   begin -- Encode
      Encode_Block : loop
         if Item_End >= Item'Last then
            Prepare (Item => Item (Item_Start .. Item'Last), Uncoded => Uncoded);
            Encode  (Uncoded => Uncoded, Encoded => Result (Result_Start .. Result_Start + 3) );

            exit Encode_Block;
         else
            Prepare (Item => Item (Item_Start .. Item_End), Uncoded => Uncoded);
            Encode  (Uncoded => Uncoded, Encoded => Result (Result_Start .. Result_Start + 3) );
         end if;

         Item_Start   := Item_Start + 3;
         Item_End     := Item_End + 3;
         Result_Start := Result_Start + 4;
      end loop Encode_Block;

      return Result;
   exception -- Encode
      when Invalid_Length => -- From Prepare
         raise Encoding_Error;
   end Encode;

   function Decode (Item : Ada.Streams.Stream_Element_Array) return Ada.Streams.Stream_Element_Array is
      function Decoded_Length return Ada.Streams.Stream_Element_Offset;

      function Decoded_Length return Ada.Streams.Stream_Element_Offset is
         Last : Ada.Streams.Stream_Element_Offset := Item'Last;

         use type Ada.Streams.Stream_Element;
         use type Ada.Streams.Stream_Element_Offset;
      begin -- Decoded_Length
         Ignore_Padding : loop
            exit Ignore_Padding when Item (Last) /= Pad_Data;

            Last := Last - 1;
         end loop Ignore_Padding;

         return (Last - Item'First + 1) * 3 / 4;
      end Decoded_Length;

      function Decode (Encoded : Encoded_Block) return Ada.Streams.Stream_Element_Array;
      -- Returns the decoded value of Encoded.

      function Decode (Encoded : Encoded_Block) return Ada.Streams.Stream_Element_Array is
         Found   : Boolean := False;
         Indices : Index_Block := (Last => 4, others => 0);
         Uncoded : Uncoded_Block;
         for Uncoded'Address use Indices'Address; -- Overlay Result upon Indices.

         use type Ada.Streams.Stream_Element;
         use type Ada.Streams.Stream_Element_Offset;
      begin -- Decode
         All_Elements : for Encoded_Index in Encoded'Range loop
            if Encoded (Encoded_Index) = Pad_Data then
               case Index_Bound (Encoded_Index) is
                  when 1 | 2 =>
                     raise Encoding_Error; -- Only the last two elements of an encoded block may be padded.
                  when 3 | 4 =>
                     Indices.Last := Encoded_Index - 1; -- Mark the last used element.

                     exit All_Elements;
               end case;
            else
               All_Data : for Index in Base64_Data'Range loop
                  if Encoded (Encoded_Index) = Base64_Data (Index) then
                     Found := True;

                     case Index_Bound (Encoded_Index) is
                        when 1 =>
                           Indices.One := Base64_Index (Index);
                        when 2 =>
                           Indices.Two := Base64_Index (Index);
                        when 3 =>
                           Indices.Three := Base64_Index (Index);
                        when 4 =>
                           Indices.Four := Base64_Index (Index);
                     end case;

                     exit All_Data;
                  end if;
               end loop All_Data;

               if not Found then
                  raise Encoding_Error; -- Invalid element.  Not a pad, not a base 64 data element.
               end if;
            end if;
         end loop All_Elements;

         case Indices.Last is
            when 1 =>
               raise Encoding_Error; -- This should not be possible.
            when 2 =>
               return (1 => Uncoded.One);
            when 3 =>
               return (1 => Uncoded.One, 2 => Uncoded.Two);
            when 4 =>
               return (1 => Uncoded.One, 2 => Uncoded.Two, 3 => Uncoded.Three);
         end case;
      end Decode;

      use type Ada.Streams.Stream_Element_Offset;

      Result       : Ada.Streams.Stream_Element_Array (1 .. Decoded_Length);
      Result_Start : Ada.Streams.Stream_Element_Offset := Result'First;
      Result_End   : Ada.Streams.Stream_Element_Offset := Result'First + 2; -- 3 elements.
      Item_Start   : Ada.Streams.Stream_Element_Offset := Item'First;
   begin -- Decode
      if Item'Length rem 4 /= 0 then
         raise Invalid_Length with "Item must be a multiple of 4 elements.";
      end if;

      Decode_Block : loop
         if Result_End >= Result'Last then
            Result (Result_Start .. Result'Last) := Decode (Item (Item_Start .. Item_Start + 3) );

            exit Decode_Block;
         else
            Result (Result_Start .. Result_End) := Decode (Item (Item_Start .. Item_Start + 3) );
         end if;

         Item_Start   := Item_Start + 4;
         Result_Start := Result_Start + 3;
         Result_End   := Result_End + 3;
      end loop Decode_Block;

      return Result;
   end Decode;
end Solid.Web.MIME.Encodings.Base64;
