with Ada.Calendar;
with Solid.Strings;

use Solid.Strings;

package body Solid.CGI.Request is
   --~ function Method (Object : Data) return Request_Method is
   --~ begin -- Method
      --~ return Object.Method;
   --~ end Method;

   --~ function Path (Object : Data) return String is
   --~ begin -- Path
      --~ return -Object.Path;
   --~ end Path;

   function Headers (Object : Data) return CGI.Headers.List is
   begin -- Headers
      return Object.Headers;
   end Headers;

   function Parameters (Object : Data) return CGI.Parameters.List is
   begin -- Parameters
      return Object.Parameters;
   end Parameters;

   overriding procedure Initialize (Object : in out Data) is
   begin -- Initialize
      Object.Created := Ada.Calendar.Clock;
   end Initialize;
end Solid.CGI.Request;
