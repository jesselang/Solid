with Ada.Text_IO;


package body Solid.CGI.Headers is
   procedure Read (Request : in out List) is
   begin -- Read
      null;
   end Read;

   procedure Add (To : in out List; Name : in String; Value : in String) is
   begin -- Add
      Implementation.Append (Container => To.Handle,
                             Key       => Ada.Strings.Unbounded.To_Unbounded_String (Name),
                             New_Item  => Ada.Strings.Unbounded.To_Unbounded_String (Value) );
   end Add;

   procedure Write (Response : in out List) is
   begin -- Write
      null;
   end Write;
end Solid.CGI.Headers;
