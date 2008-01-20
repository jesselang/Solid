with Ada.Strings.Unbounded;

package Solid.Strings is
   subtype U_String is Ada.Strings.Unbounded.Unbounded_String;

   Null_String : constant U_String := Ada.Strings.Unbounded.Null_Unbounded_String;

   function "+" (Right : String)   return U_String renames Ada.Strings.Unbounded.To_Unbounded_String;
   function "-" (Right : U_String) return String   renames Ada.Strings.Unbounded.To_String;

   function "&" (Left : U_String; Right : U_String)  return U_String renames Ada.Strings.Unbounded."&";
   function "&" (Left : U_String; Right : String)    return U_String renames Ada.Strings.Unbounded."&";
   function "&" (Left : String; Right : U_String)    return U_String renames Ada.Strings.Unbounded."&";
   function "&" (Left : U_String; Right : Character) return U_String renames Ada.Strings.Unbounded."&";

   type String_Array is array (Positive range <>) of Strings.U_String;
end Solid.Strings;
