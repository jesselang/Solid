with Ada.Containers;

private with Ada.Containers.Hashed_Maps;
private with Ada.Containers.Vectors;

generic -- Solid.Data_Structures.Hashed_Multimaps
   type Map_Key is private;
   type Element is private;
   with function Hash (Key : Map_Key) return Ada.Containers.Hash_Type;
   with function Equivalent_Keys (Left, Right : Map_Key) return Boolean;
   with function "=" (Left, Right : Element) return Boolean is <>;
package Solid.Data_Structures.Hashed_Multimaps is
   type Map is tagged private;
   type Cursor is private;

   Empty_Map  : constant Map;
   No_Element : constant Cursor;

   procedure Append (Container : in out Map;
                     Key       : in     Map_Key;
                     New_Item  : in     Element);

   function Key (Position : Cursor) return Map_Key;

   generic -- Iterate
      with procedure Process (Position : in Cursor; Continue : in out Boolean);
   procedure Iterate (Container : in Map);

   generic -- Iterate_Values
      with procedure Process (Value : in Element; Continue : in out Boolean);
   procedure Iterate_Values (Container : in Map;
                             Position  : in Cursor);
private -- Solid.Data_Structures.Hashed_Multimaps
   package Element_Implementation is new Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Element);
   package Map_Implementation is new Ada.Containers.Hashed_Maps (Key_Type        => Map_Key,
                                                                 Element_Type    => Element_Implementation.Vector,
                                                                 Hash            => Hash,
                                                                 Equivalent_Keys => Equivalent_Keys,
                                                                 "="             => Element_Implementation."=");
   type Map is tagged record
      Handle : Map_Implementation.Map;
   end record;

   type Cursor is record
      Handle : Map_Implementation.Cursor;
   end record;

   Empty_Map  : constant Map    := (Handle => Map_Implementation.Empty_Map);
   No_Element : constant Cursor := (Handle => Map_Implementation.No_Element);
end Solid.Data_Structures.Hashed_Multimaps;
