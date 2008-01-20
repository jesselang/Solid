with Solid.Strings;
with Solid.Text_Streams;

use Solid.Strings;

package body Solid.CGI.Request.Set is
   procedure Environment (Object : in out Data; Environment : in CGI.Environment.Handle) is
   begin -- Environment
      Object.Environment := Environment;
   end Environment;

   procedure Post_Query (Object : in out Data; Post_Query : in String) is
   begin -- Post_Query
      Object.Post_Query := +Post_Query;
   end Post_Query;

   procedure Headers (Object : in out Data; Headers : in CGI.Headers.List) is
   begin -- Headers
      Object.Headers := Headers;
   end Headers;

   procedure Parameters (Object : in out Data; Parameters : in CGI.Parameters.List) is
   begin -- Parameters
      Object.Parameters := Parameters;
   end Parameters;

   procedure Append_Payload (Object : in out Data; Payload : in Ada.Streams.Stream_Element_Array) is
   begin -- Payload
      Object.Payload := Object.Payload & (+Text_Streams.To_String (Payload) );
   end Append_Payload;
end Solid.CGI.Request.Set;
