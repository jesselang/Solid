with Solid.CGI.Environment;

package Solid.CGI.Standard.Environment is
   Current : constant CGI.Environment.Handle;

   type Data is new CGI.Environment.Data with null record;

   overriding function Value (Object : Data; Name : String) return String;
   -- Get the CGI environment variable with Name.
   -- Returns "" (null string) if not found.

   overriding procedure Iterate_Process (Object : in Data; Process : CGI.Environment.Callback);

   generic -- Iterate
      with procedure Process (Name : in String; Value : in String; Continue : in out Boolean);
   procedure Iterate (Object : in Data);
   -- Iterates over CGI environment variables.
   -- Stops iterating when Continue is set to False.
private -- Solid.CGI.Standard.Environment
   Current : constant CGI.Environment.Handle := new Data;
end Solid.CGI.Standard.Environment;
