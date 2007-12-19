private with Solid.Strings;

with Ada.Calendar;
with Ada.Finalization;
with Solid.CGI.Headers;
with Solid.CGI.Parameters;

package Solid.CGI.Request is
   type Request_Method is (Get, Post);

   type Data is new Ada.Finalization.Controlled with private;

   function Method (Request : Data) return Request_Method;

   function URI (Request : Data) return String;

   function Headers (Request : Data) return CGI.Headers.List;

   function Parameters (Request : Data) return CGI.Parameters.List;
private -- Solid.CGI.Request
   type Data is new Ada.Finalization.Controlled with record
      Created            : Ada.Calendar.Time;
      Method             : Request_Method;
      URI                : Strings.U_String;
      Message_Headers    : CGI.Headers.List;
      Message_Parameters : CGI.Parameters.List;
      Message_Body       : Strings.U_String;
   end record;

   overriding procedure Initialize (Object : in out Data);
end Solid.CGI.Request;
