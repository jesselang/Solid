private with Ada.Strings.Unbounded;
private with Solid.CGI.Headers;

package Solid.CGI.Response is
   type Data is private;

private -- Solid.CGI.Response
   type Data is record
      Message_Header  : Headers.List;
      Message_Body    : Ada.Strings.Unbounded.Unbounded_String;
   end record;
end Solid.CGI.Response;
