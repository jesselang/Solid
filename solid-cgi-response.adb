with Solid.Strings;
use Solid.Strings;

package body Solid.CGI.Response is
   function Test return Data is
   begin -- Test
      return Build (Content_Type => "text/plain", Message_Body => "This is a test response.");
   end Test;

   function Build (Content_Type : String; Message_Body : String) return Data is
      Result : Data;
   begin -- Build
      Result.Message_Headers.Add (Name => "Content-type", Value => Content_Type);
      Result.Message_Body := +Message_Body;

      return Result;
   end Build;

   function Headers (Object : Data) return CGI.Headers.List is
   begin -- Headers
      return Object.Message_Headers;
   end Headers;

   function Payload (Object : Data) return String is
   begin -- Payload
      return -Object.Message_Body;
   end Payload;
end Solid.CGI.Response;
