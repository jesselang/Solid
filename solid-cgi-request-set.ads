with Ada.Streams;
with Solid.CGI.Headers;
with Solid.CGI.Parameters;

package Solid.CGI.Request.Set is
   --~ procedure Method (Object : in out Data; Method : in Request_Method);

   --~ procedure Path (Object : in out Data; Path : in String);

   procedure Environment (Object : in out Data; Environment : in CGI.Environment.Handle);

   procedure Headers (Object : in out Data; Headers : in CGI.Headers.List);

   procedure Parameters (Object : in out Data; Parameters : in CGI.Parameters.List);

   procedure Payload (Object : in out Data; Payload : in Ada.Streams.Stream_Element_Array);
end Solid.CGI.Request.Set;
