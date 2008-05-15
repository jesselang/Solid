with Solid.Web.Session.Generic_Tuples;
with Solid.Strings;

package Solid.Web.Session.Tuples is
   package Strings is new Session.Generic_Tuples (Tuple_Value => Solid.Strings.U_String);
   package Numbers is new Session.Generic_Tuples (Tuple_Value => Integer);
   package Logical is new Session.Generic_Tuples (Tuple_Value => Boolean);
   package Floats  is new Session.Generic_Tuples (Tuple_Value => Float);
end Solid.Web.Session.Tuples;
