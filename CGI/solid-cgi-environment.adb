with Ada.Environment_Variables;

package body Solid.CGI.Environment is
   function Value (Name : in Variable) return String is
   begin -- Value
      return Value (Name => Variable'Image (Name) );
   end Value;

   function Value (Name : in String) return String is
      use Ada.Environment_Variables;
   begin -- Value
      if Exists (Name => Name) then
         return Value (Name => Name);
      else
         return "";
      end if;
   end Value;
end Solid.CGI.Environment;
