package body Solid.CGI.Headers is
   procedure Add (To : in out List; Name : in String; Value : in String) is
   begin -- Add
      Implementation.Append (Container => To.Handle,
                             Key       => Ada.Strings.Unbounded.To_Unbounded_String (Name),
                             New_Item  => Ada.Strings.Unbounded.To_Unbounded_String (Value) );
   end Add;

   procedure Iterate (Item : in List) is
      procedure Each_Name (Name : in String; Continue : in out Boolean) is
      begin -- Each_Name
         null;
      end Each_Name;
   begin -- Iterate
      null;
   end Iterate;
end Solid.CGI.Headers;
