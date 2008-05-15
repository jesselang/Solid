package body Solid.Web.Environment is
   function Value (Object : Handle; Name : Variable) return String is
   begin -- Value
      return Value (Object.all, Name => Variable'Image (Name) );
   end Value;

   function Value (Object : Handle; Name : String) return String is
   begin -- Value
      return Value (Object.all, Name => Name);
   end Value;

   procedure Iterate (Object : Handle) is
      procedure Iteration_Wrapper (Name : in String; Value : in String; Continue : in out Boolean) is
      begin -- Iteration_Wrapper
         Process (Name => Name, Value => Value, Continue => Continue);
      end Iteration_Wrapper;
   begin -- Iterate
      Iterate_Process (Object => Object.all, Process => Iteration_Wrapper'Unrestricted_Access);
   end Iterate;
end Solid.Web.Environment;
