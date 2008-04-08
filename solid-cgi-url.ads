-- Utility URL functions.
package Solid.CGI.URL is
   function Query (URL : String) return String;
   -- Returns query portion of the URL.

   -- Encode/Decode
   function Decode (S : String) return String;
end Solid.CGI.URL;