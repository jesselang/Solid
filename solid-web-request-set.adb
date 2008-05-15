with Solid.Strings;
with Solid.Text_Streams;

use Solid.Strings;

package body Solid.Web.Request.Set is
   procedure Environment (Object : in out Data; Environment : in Web.Environment.Handle) is
   begin -- Environment
      Object.Environment := Environment;
   end Environment;

   procedure Post_Query (Object : in out Data; Post_Query : in String) is
   begin -- Post_Query
      Object.Post_Query := +Post_Query;
   end Post_Query;

   procedure Cookies (Object : in out Data; Cookies : in Web.Cookies.List) is
   begin -- Cookies
      Object.Cookies := Cookies;
   end Cookies;

   procedure Headers (Object : in out Data; Headers : in Web.Headers.List) is
   begin -- Headers
      Object.Headers := Headers;
   end Headers;

   procedure Parameters (Object : in out Data; Parameters : in Web.Parameters.List) is
   begin -- Parameters
      Object.Parameters := Parameters;
   end Parameters;

   procedure Append_Payload (Object : in out Data; Payload : in Ada.Streams.Stream_Element_Array) is
   begin -- Payload
      Object.Payload := Object.Payload & (+Text_Streams.To_String (Payload) );
   end Append_Payload;

   procedure Session_Context (Object : in out Data; Settings : Web.Session.Storage.Context_Handle) is
   begin -- Session_Context
      Object.Session_Context := Settings;
   end Session_Context;
end Solid.Web.Request.Set;
