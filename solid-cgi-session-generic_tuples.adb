with Solid.Strings;

package body Solid.CGI.Session.Generic_Tuples is
   function Get (Session : Data'Class; Key : String) return Tuple_Value is
      Item : Valued_Tuple;
   begin -- Get
      Item := Valued_Tuple (Get_Tuple (Session, Key => Key) );
      return Item.Value;
   exception -- Get
      when Constraint_Error =>
         raise Invalid_Value;
   end Get;

   procedure Set (Session : in out Data'Class; Key : in String; Value : in Tuple_Value) is
      use Solid.Strings;

      Item : constant Valued_Tuple := (Key => +Key, Value => Value);
   begin -- Set
      Set_Tuple (Session, Item => Item);
   end Set;
end Solid.CGI.Session.Generic_Tuples;
