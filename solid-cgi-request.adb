with Solid.Strings;

use Solid.Strings;

package body Solid.CGI.Request is
   function Method (Request : Data) return Request_Method is
   begin -- Method
      return Request.Method;
   end Method;

   function URI (Request : Data) return String is
   begin -- URI
      return -Request.URI;
   end URI;

   function Headers (Request : Data) return CGI.Headers.List is
   begin -- Headers
      return Request.Message_Headers;
   end Headers;

   function Parameters (Request : Data) return CGI.Parameters.List is
   begin -- Parameters
      return Request.Message_Parameters;
   end Parameters;

   overriding procedure Initialize (Object : in out Data) is
   begin -- Initialize
      null;
   end Initialize;
end Solid.CGI.Request;
