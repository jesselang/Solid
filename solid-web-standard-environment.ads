-- Implementation of the "standard" CGI environment.  Used in Solid.Web.Standard.Program to read environment variables.

with Solid.Web.Environment;

package Solid.Web.Standard.Environment is
   Current : constant Web.Environment.Handle;
   -- This constant should be used in environment operations.

   type Data is new Web.Environment.Data with null record;

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
   procedure Iterate_Process (Object : in Data; Process : Web.Environment.Callback);
private -- Solid.Web.Standard.Environment
   Current : constant Web.Environment.Handle := new Data;
end Solid.Web.Standard.Environment;
