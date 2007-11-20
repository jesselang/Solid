private with Ada.Strings.Unbounded;
private with Solid.CGI.Headers;

package Solid.CGI.Request is
   type Request_Method is (Get, Post, Head);

   type Data is private;

   function Method (Request : Data) return Request_Method;
private -- Solid.CGI.Request
   type Data is record
      Method          : Request_Method;
      URI             : Ada.Strings.Unbounded.Unbounded_String;
      Message_Headers : Headers.List;
      Message_Body    : Ada.Strings.Unbounded.Unbounded_String;
   end record;
end Solid.CGI.Request;
