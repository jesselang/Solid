-- Operations to set various components of a request object.
-- For use when creating CGI protocol implementations.
with Ada.Streams;
with Solid.CGI.Cookies;
with Solid.CGI.Headers;
with Solid.CGI.Parameters;
with Solid.CGI.Session;

package Solid.CGI.Request.Set is
   procedure Environment (Object : in out Data; Environment : in CGI.Environment.Handle);

   procedure Post_Query (Object : in out Data; Post_Query : in String);

   procedure Cookies (Object : in out Data; Cookies : in CGI.Cookies.List);

   procedure Headers (Object : in out Data; Headers : in CGI.Headers.List);

   procedure Parameters (Object : in out Data; Parameters : in CGI.Parameters.List);

   procedure Append_Payload (Object : in out Data; Payload : in Ada.Streams.Stream_Element_Array);

   procedure Session_Context (Object : in out Data; Settings : CGI.Session.Storage.Context_Handle);
end Solid.CGI.Request.Set;
