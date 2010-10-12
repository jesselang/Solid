with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Unchecked_Deallocation;
with PragmARC.Images.Image;

package body Solid.Formats.JSON is
   function "=" (Left : Value; Right : Value) return Boolean is
   begin -- "="
      return Left.Object.all = Right.Object.all;
   end "=";

   function "+" (Right : String) return Value is
   begin -- "+"
      return (Ada.Finalization.Controlled with
               Object => new Value_Info'(ID => JSON_String, String_Value => Ada.Strings.Unbounded.To_Unbounded_String (Right) ),
               Count  => new Integer'(1) );
   end "+";

   function "+" (Right : Integer) return Value is
   begin -- "+"
      return (Ada.Finalization.Controlled with
              Object => new Value_Info'(ID => JSON_Number, Number_Value => Number (Right) ),
              Count  => new Integer'(1) );
   end "+";

   function "+" (Right : Float) return Value is
   begin -- "+"
      return (Ada.Finalization.Controlled with
              Object => new Value_Info'(ID => JSON_Number, Number_Value => Number (Right) ),
              Count  => new Integer'(1) );
   end "+";

   function "+" (Right : Boolean) return Value is
   begin -- "+"
      return (Ada.Finalization.Controlled with
              Object => new Value_Info'(ID => JSON_Boolean, Boolean_Value => Right),
              Count  => new Integer'(1) );
   end "+";

   function "+" (Right : Object'Class) return Value is
   begin -- "+"
      return (Ada.Finalization.Controlled with
              Object => new Value_Info'(ID => JSON_Object, Object_Value => Object (Right) ),
              Count  => new Integer'(1) );
   end "+";

   procedure Append (To : in out Object; Key : in String; Item : in Value'Class) is
   begin -- Append
      To.Include (Key => Ada.Strings.Unbounded.To_Unbounded_String (Key), New_Item => Value (Item) );
   end Append;

   function Image (V : Value) return String;
   -- Returns a string representation of V.
   pragma Inline (Image);

   function Encode (S : String) return String;
   -- Return an encoded form of S suitable for string representation.
   pragma Inline (Encode);

   Quote : Character renames Ada.Characters.Latin_1.Quotation;

   function To_String (Item : Object) return String is
      procedure Append_One (Position : in Objects.Cursor);
      -- Appends the element at Position to the string result.

      use Ada.Strings.Unbounded;

      Result : Unbounded_String;

      procedure Append_One (Position : in Objects.Cursor) is
         Key     : constant Unbounded_String := Objects.Key (Position);
         Element : constant Value            := Objects.Element (Position);
      begin -- Append_One
         Append (Source => Result, New_Item => Quote & Encode (To_String (Key) ) & Quote);
         Append (Source => Result, New_Item => ": ");
         Append (Source => Result, New_Item => Image (Element) );
         Append (Source => Result, New_Item => ", ");
      end Append_One;
   begin -- To_String
      Append (Source => Result, New_Item => '{');
      Item.Iterate (Process => Append_One'Access);
      Overwrite (Source => Result, Position => Length (Result) - 1, New_Item => String'(1 => '}') );
      Delete (Source => Result, From => Length (Result), Through => Length (Result) );

      return To_String (Result);
   end To_String;

   function "+" (Right : List'Class) return Value is
   begin -- "+"
      return (Ada.Finalization.Controlled with
              Object => new Value_Info'(ID => JSON_List, List_Value => List (Right) ),
              Count  => new Integer'(1) );
   end "+";

   procedure Append (To : in out List; Item : in Value'Class) is
   begin -- Append
      To.Append (New_Item => Value (Item) );
   end Append;

   function "&" (Left : List; Right : Value'Class) return List is
      New_List : List := Left;
   begin -- "&"
      Append (To => New_List, Item => Value (Right) );

      return New_List;
   end "&";

   function To_String (Item : List) return String is
      use Ada.Strings.Unbounded;

      Result : Unbounded_String;
   begin -- To_String
      Append (Source => Result, New_Item => '[');

      All_Values : for I in Item.First_Index .. Item.Last_Index loop
         Append (Source => Result, New_Item => Image (Item.Element (I) ) );

         exit All_Values when I >= Item.Last_Index;

         Append (Source => Result, New_Item => ", ");
      end loop All_Values;

      Append (Source => Result, New_Item => ']');

      return To_String (Result);
   end To_String;

   function Image (V : Value) return String is
      function Image (N : Number) return String;
      -- Returns a string representation of N.
      pragma Inline (Image);

      function Image (N : Number) return String is
         Integer_Value : constant Integer := Integer (N);

         function Image is new PragmARC.Images.Float_Image (Number);
      begin -- Image
         if N = Number (Integer_Value) then -- N contains a whole number.
            return PragmARC.Images.Image (Integer_Value);
         else
            return Image (N, Fore => 1); -- Output as a decimal.
         end if;
      end Image;
   begin -- Image
      case V.Object.ID is
         when JSON_String =>
            return Quote & Encode (Ada.Strings.Unbounded.To_String (V.Object.String_Value) ) & Quote;
         when JSON_Number =>
            return Image (V.Object.Number_Value);
         when JSON_Object =>
            return To_String (V.Object.Object_Value);
         when JSON_List =>
            return To_String (V.Object.List_Value);
         when JSON_Boolean =>
            if V.Object.Boolean_Value then
               return "true";
            else
               return "false";
            end if;
         when JSON_Null =>
            return "null";
      end case;
   end Image;

   function Encode (S : String) return String is
      use Ada.Characters.Latin_1;
      Must_Escape : constant String := (Quotation,
                                        Reverse_Solidus,
                                        BS,  -- Backspace
                                        FF,  -- Form feed
                                        LF,  -- New line
                                        CR,  -- Carriage return
                                        HT); -- Horizontal tab
                                        -- Solidus was intentionally left out.

      type Escape_Set is array (Must_Escape'Range) of String (1 .. 2);

      Escaped_Result : constant Escape_Set := (1 => (Reverse_Solidus & Quotation),
                                               2 => (Reverse_Solidus & Reverse_Solidus),
                                               3 => (Reverse_Solidus & LC_B),
                                               4 => (Reverse_Solidus & LC_F),
                                               5 => (Reverse_Solidus & LC_N),
                                               6 => (Reverse_Solidus & LC_R),
                                               7 => (Reverse_Solidus & LC_T) );
   begin -- Encode
      if S'Length = 0 then
         return "";
      end if;

      Each_Escape : for I in Must_Escape'Range loop
         if S (S'First) = Must_Escape (I) then
            return Escaped_Result (I) & Encode (S (S'First + 1 .. S'Last) );
         end if;
      end loop Each_Escape;

      return S (S'First) & Encode (S (S'First + 1 .. S'Last) );
   end Encode;

   overriding procedure Initialize (Object : in out Value) is
   begin -- Initialize
      Object.Object := new Value_Info'(ID => JSON_Null);
      Object.Count  := new Integer'(1);
   end Initialize;

   overriding procedure Adjust (Object : in out Value) is
   begin -- Adjust
      Object.Count.all := Object.Count.all + 1;
   end Adjust;

   overriding procedure Finalize (Object : in out Value) is
      procedure Free is new Ada.Unchecked_Deallocation (Value_Info, Value_Handle);
      procedure Free is new Ada.Unchecked_Deallocation (Integer, Integer_Handle);
   begin -- Finalize
      Object.Count.all := Object.Count.all - 1;

      if Object.Count.all <= 0 then
         Free (Object.Object);
         Free (Object.Count);
      end if;
   end Finalize;
end Solid.Formats.JSON;
