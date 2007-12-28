private with Ada.Streams;
private with Solid.Strings;

with Ada.Calendar;
with Ada.Finalization;
with Solid.CGI.Environment;
with Solid.CGI.Headers;
with Solid.CGI.Parameters;

package Solid.CGI.Request is
   type Request_Method is (Get, Post);

   type Data is new Ada.Finalization.Controlled with private;

   --~ function Method (Object : Data) return Request_Method;

   --~ function Path (Object : Data) return String;

   function Headers (Object : Data) return CGI.Headers.List;

   function Parameters (Object : Data) return CGI.Parameters.List;
private -- Solid.CGI.Request
   type Data is new Ada.Finalization.Controlled with record
      Created            : Ada.Calendar.Time;
      --~ Method             : Request_Method;
      --~ Path               : Strings.U_String;
      Environment        : CGI.Environment.Handle;
      Headers            : CGI.Headers.List;
      Parameters         : CGI.Parameters.List;
      Payload            : Strings.U_String;
   end record;

   pragma Assert (Ada.Streams.Stream_Element'Size = 8);

   overriding procedure Initialize (Object : in out Data);
end Solid.CGI.Request;
