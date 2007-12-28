with Ada.Environment_Variables;

package body Solid.CGI.Standard.Environment is
   function Value (Object : Data; Name : CGI.Environment.Variable) return String is
   begin -- Value
      return Value (Object => Object, Name => CGI.Environment.Variable'Image (Name) );
   end Value;

   function Value (Object : Data; Name : String) return String is
   begin -- Value
      if Ada.Environment_Variables.Exists (Name => Name) then
         return Ada.Environment_Variables.Value (Name => Name);
      else
         return "";
      end if;
   end Value;

   procedure Iterate (Object : in Data) is
      Continue : Boolean := True;

      procedure Iteration_Wrapper (Name : in String; Value : in String) is
      begin -- Iteration_Wrapper
         if not Continue then
            return;
         end if;

         Process (Name => Name, Value => Value, Continue => Continue);
      end Iteration_Wrapper;
   begin -- Iterate
      Ada.Environment_Variables.Iterate (Process => Iteration_Wrapper'Access);
   end Iterate;
end Solid.CGI.Standard.Environment;
