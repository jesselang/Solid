with Ada.Streams;

package Solid.Web.Response.Client is
   Invalid : exception;

   function Read (Stream : access Ada.Streams.Root_Stream_Type'Class) return Web.Response.Data;
   -- Reads Stream and parses into a response.
   -- Raises Invalid if the parsing failed.
end Solid.Web.Response.Client;
