package body Solid.CGI.Environment is
   function Value (Object : Handle; Name : Variable) return String is
   begin -- Value
      return Value (Object.all, Name => Variable'Image (Name) );
   end Value;

   function Value (Object : Handle; Name : String) return String is
   begin -- Value
      return Value (Object.all, Name => Name);
   end Value;
end Solid.CGI.Environment;
