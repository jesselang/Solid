-- Implementation of the "standard" CGI environment.  Used in Solid.CGI.Standard.Program to read environment variables.

with Solid.CGI.Environment;

package Solid.CGI.Standard.Environment is
   Current : constant CGI.Environment.Handle;
   -- This constant should be used in environment operations.

   type Data is new CGI.Environment.Data with null record;

   generic -- Iterate
      with procedure Process (Name : in String; Value : in String; Continue : in out Boolean);
   procedure Iterate (Object : in Data);
   -- Iterates over CGI environment variables.
   -- Returns immediately when Continue is set to False.

   -- Implementations of abstract operations.
   overriding
   function Value (Object : Data; Name : String) return String;
   -- Get the CGI environment variable with Name.
   -- Returns "" (null string) if not found.

   overriding
   procedure Iterate_Process (Object : in Data; Process : CGI.Environment.Callback);
private -- Solid.CGI.Standard.Environment
   Current : constant CGI.Environment.Handle := new Data;
end Solid.CGI.Standard.Environment;
