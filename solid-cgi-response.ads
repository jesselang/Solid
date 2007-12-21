private with Ada.Strings.Unbounded;

with Solid.CGI.Headers;
with Solid.CGI.Request;

package Solid.CGI.Response is
   type Data is private;

   function Test return Data;
   pragma Inline (Test);

   function Build (Content_Type : String; Message_Body : String) return Data;


   function Headers (Object : Data) return CGI.Headers.List;

   function Payload (Object : Data) return String;
private -- Solid.CGI.Response
   type Data is record
      Message_Headers : CGI.Headers.List;
      Message_Body    : Ada.Strings.Unbounded.Unbounded_String;
   end record;
end Solid.CGI.Response;
