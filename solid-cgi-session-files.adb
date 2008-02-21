package body Solid.CGI.Session.Files is
   function Create (Name : String := "Session") return Data is
      Result : Data;
   begin -- Create
      Result.Set_Name (To => Name);
      Result.New_Identity;
      return Result;
   end Create;

   procedure Read (Session : in out Data) is
   begin -- Read
      null;
   end Read;

   procedure Write (Session : in Data) is
   begin -- Write
      null;
   end Write;
end Solid.CGI.Session.Files;
