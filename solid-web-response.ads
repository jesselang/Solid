-- Type and operations that represent a HTTP response.
private with Solid.Strings;
with Ada.Streams;
with Solid.Web.Headers;
with Solid.Web.Request;

package Solid.Web.Response is
   type Data is private;

   function Test (Client : Request.Data) return Data;
   pragma Inline (Test);

   function Build (Content_Type : String;
                   Message_Body : String;
                   Headers      : Web.Headers.List := Web.Headers.No_Headers)
   return Data;

   function URL (Location : String) return Data;
   -- Redirect to Location.  This is a temporary redirect.

   -- The following functions are designed to be used by the library, not by client applications.
   function Headers (Object : Data) return Web.Headers.List;

   function Payload (Object : Data) return Ada.Streams.Stream_Element_Array;
private -- Solid.Web.Response
   type Data is record
      Headers : Web.Headers.List;
      Payload : Strings.U_String;
   end record;

   pragma Assert (Ada.Streams.Stream_Element'Size = 8); -- -gnata has to be enabled for this assertion to work.
end Solid.Web.Response;
