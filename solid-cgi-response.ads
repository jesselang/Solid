-- Type and operations that represent a HTTP response.
private with Solid.Strings;
with Ada.Streams;
with Solid.CGI.Headers;
with Solid.CGI.Request;

package Solid.CGI.Response is
   type Data is private;

   function Test (Client : Request.Data) return Data;
   pragma Inline (Test);

   function Build (Content_Type : String;
                   Message_Body : String;
                   Headers      : CGI.Headers.List := CGI.Headers.No_Headers)
   return Data;

   -- The following functions are designed to be used by the library, not by client applications.
   function Headers (Object : Data) return CGI.Headers.List;

   function Payload (Object : Data) return Ada.Streams.Stream_Element_Array;
private -- Solid.CGI.Response
   type Data is record
      Headers : CGI.Headers.List;
      Payload : Strings.U_String;
   end record;

   pragma Assert (Ada.Streams.Stream_Element'Size = 8);
end Solid.CGI.Response;
