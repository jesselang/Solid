with Ada.Environment_Variables;

package body Solid.Web.Standard.Environment is
   function Value (Object : Data; Name : Web.Environment.Variable) return String is
   begin -- Value
      return Value (Object => Object, Name => Web.Environment.Variable'Image (Name) );
   end Value;

   function Value (Object : Data; Name : String) return String is
   begin -- Value
      if Ada.Environment_Variables.Exists (Name => Name) then
         return Ada.Environment_Variables.Value (Name => Name);
      else
         return "";
      end if;
   end Value;

   procedure Iterate_Process (Object : in Data; Process : Web.Environment.Callback) is
      procedure Iterate is new Environment.Iterate (Process => Process.all);
   begin -- Iterate_Process
      Iterate (Object => Object);
   end Iterate_Process;

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


end Solid.Web.Standard.Environment;
