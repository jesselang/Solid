with Ada.Strings.Unbounded;

package Solid.Strings is
   subtype U_String is Ada.Strings.Unbounded.Unbounded_String;

   Null_String : constant U_String := Ada.Strings.Unbounded.Null_Unbounded_String;

   function "+" (Right : String)   return U_String renames Ada.Strings.Unbounded.To_Unbounded_String;
   function "-" (Right : U_String) return String   renames Ada.Strings.Unbounded.To_String;
end Solid.Strings;
