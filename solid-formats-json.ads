private with Ada.Containers.Hashed_Maps;
private with Ada.Containers.Vectors;
private with Ada.Finalization;
private with Ada.Strings.Unbounded.Hash;

package Solid.Formats.JSON is
   type Value is tagged private;
   -- A JSON datum.

   function "=" (Left : Value; Right : Value) return Boolean;
   -- Returns True if Left and Right are equal.

   Null_Value : constant Value;

   function "+" (Right : String) return Value;
   function "+" (Right : Integer) return Value;
   function "+" (Right : Float) return Value;
   function "+" (Right : Boolean) return Value;
   -- The above convert standard types to a JSON value.

   type Object is tagged private;

   function "+" (Right : Object'Class) return Value;
   -- Returns the object as a value which can be appended to a list or object.

   procedure Append (To : in out Object; Key : in String; Item : in Value'Class);
   -- Appends Value with Key to the object To.

   function To_String (Item : Object) return String;
   -- Returns a string representation of Object.

   type List is tagged private;

   function "+" (Right : List'Class) return Value;
   -- Returns the list as a value which can be appended to a list or object.

   procedure Append (To : in out List; Item : in Value'Class);
   -- Appends Item to the list To.

   function "&" (Left : List; Right : Value'Class) return List;
   -- Concatenates Right with Left by appending Right and returning the list.

   function To_String (Item : List) return String;
   -- Return a string representation of List.
private -- Solid.Formats.JSON
   type Value_Info;

   type Value_Handle is access all Value_Info;

   type Integer_Handle is access Integer;

   type Value is new Ada.Finalization.Controlled with record -- A reference-counted pointer.
      Object : Value_Handle;
      Count  : Integer_Handle;
   end record;

   overriding procedure Initialize (Object : in out Value);
   overriding procedure Adjust     (Object : in out Value);
   overriding procedure Finalize   (Object : in out Value);

   package Objects is new Ada.Containers.Hashed_Maps (Key_Type        => Ada.Strings.Unbounded.Unbounded_String,
                                                      Element_Type    => Value,
                                                      Hash            => Ada.Strings.Unbounded.Hash,
                                                      Equivalent_Keys => Ada.Strings.Unbounded. "=");
   package Lists is new Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Value);

   type Object is new Objects.Map with null record;
   type List is new Lists.Vector with null record;

   type Number is new Float;

   type Value_ID is (JSON_String, JSON_Number, JSON_Object, JSON_List, JSON_Boolean, JSON_Null);

   type Value_Info (ID : Value_ID := JSON_Null) is record
      case ID is
         when JSON_String =>
            String_Value : Ada.Strings.Unbounded.Unbounded_String;
         when JSON_Number =>
            Number_Value : Number;
         when JSON_Object =>
            Object_Value : Object;
         when JSON_List =>
            List_Value : List;
         when JSON_Boolean =>
            Boolean_Value : Boolean;
         when JSON_Null =>
            null;
      end case;
   end record;

   Null_Value : constant Value := (Ada.Finalization.Controlled with
                                   Object => new Value_Info'(ID => JSON_Null),
                                   Count  => new Integer'(1) );
end Solid.Formats.JSON;
