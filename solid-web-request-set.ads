-- Operations to set various components of a request object.
-- For use when creating gateway protocol implementations.
with Ada.Streams;
with Solid.Web.Cookies;
with Solid.Web.Headers;
with Solid.Web.Parameters;
with Solid.Web.Session;

package Solid.Web.Request.Set is
   procedure Environment (Object : in out Data; Environment : in Web.Environment.Handle);

   procedure Post_Query (Object : in out Data; Post_Query : in String);

   procedure Cookies (Object : in out Data; Cookies : in Web.Cookies.List);

   procedure Headers (Object : in out Data; Headers : in Web.Headers.List);

   procedure Parameters (Object : in out Data; Parameters : in Web.Parameters.List);

   procedure Append_Payload (Object : in out Data; Payload : in Ada.Streams.Stream_Element_Array);

   procedure Session_Context (Object : in out Data; Settings : Web.Session.Storage.Context_Handle);
end Solid.Web.Request.Set;
