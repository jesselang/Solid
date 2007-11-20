private with Ada.Strings.Unbounded;
private with Solid.CGI.Headers;

with Solid.CGI.Request;

package Solid.CGI.Response is
   type Data is private;

   type Callback is access function (Request : Request.Data) return Data;

   function Build (Content_Type : String; Message_Body : String) return Data;
private -- Solid.CGI.Response
   type Data is record
      Message_Headers : Headers.List;
      Message_Body    : Ada.Strings.Unbounded.Unbounded_String;
   end record;
end Solid.CGI.Response;
