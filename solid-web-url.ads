-- Utility URL functions.
package Solid.Web.URL is
   function Query (URL : String) return String;
   -- Returns query portion of the URL.

   -- Encode/Decode
   function Decode (S : String) return String;
end Solid.Web.URL;
