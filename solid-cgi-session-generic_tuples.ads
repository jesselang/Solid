-- Extension of abstract tuple type, with operations to store and retrieve tuples from the session data.
-- Allows any type to be stored in the session data.
-- See Solid.CGI.Session.Tuples for commonly used instantiations.
generic -- Solid.CGI.Session.Generic_Tuples
   type Tuple_Value is private;
package Solid.CGI.Session.Generic_Tuples is
   Invalid_Value : exception;

   function Get (Session : Data; Key : String) return Tuple_Value;
   -- Raises Not_Found if not Exists (Session
   -- Raises Invalid_Value if the value stored is not a Tuple_Value.

   procedure Set (Session : in out Data; Key : in String; Value : in Tuple_Value);
private -- Solid.CGI.Session.Generic_Tuples
   type Valued_Tuple is new Tuple with record
      Value : Tuple_Value;
   end record;
end Solid.CGI.Session.Generic_Tuples;
