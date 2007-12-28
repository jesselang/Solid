-- Support for Netstrings - http://cr.yp.to/proto/netstrings.txt

package Solid.Strings.Net is
   Invalid : exception;

   type Net_String is new String;

   function Encode (S : String) return Net_String;
   -- Encodes S as a Net_String.
   -- Raises Invalid if S could not be encoded.

   function Decode (N : Net_String) return String;
   -- Decodes N as a String.
   -- Raises Invalid if N could not be decoded.
end Solid.Strings.Net;
