with Ada.Streams;

package Solid.Web.MIME.Encodings.Base64 is
   function Encode (Item : Ada.Streams.Stream_Element_Array) return Ada.Streams.Stream_Element_Array;
   -- Encodes S using Base64.
   -- Raises Encoding_Error if Item could not be encoded.

   function Decode (Item : Ada.Streams.Stream_Element_Array) return Ada.Streams.Stream_Element_Array;
   -- Decodes S using Base64.
   -- Raises Invalid_Length if Item is the wrong length.
   -- Raises Encoding_Error if Item contains invalid data.
end Solid.Web.MIME.Encodings.Base64;
