with Solid.CGI.Request;
with Solid.CGI.Response;

package Solid.CGI.Program is

   function Request return Request.Data;

   procedure Send (Answer : Response.Data);

   pragma Elaborate_Body;
   -- Request
   -- Response
   -- Parameters
   -- Dispatchers

end Solid.CGI.Program;
