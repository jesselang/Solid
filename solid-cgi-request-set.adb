with Solid.Strings;
with Solid.Text_Streams;

use Solid.Strings;

package body Solid.CGI.Request.Set is
   --~ procedure Method (Object : in out Data; Method : in Request_Method) is
   --~ begin -- Method
      --~ Object.Method := Method;
   --~ end Method;

   --~ procedure Path (Object : in out Data; Path : in String) is
   --~ begin -- Path
      --~ Object.Path := +Path;
   --~ end Path;

   procedure Environment (Object : in out Data; Environment : in CGI.Environment.Handle) is
   begin -- Environment
      Object.Environment := Environment;
   end Environment;

   procedure Headers (Object : in out Data; Headers : in CGI.Headers.List) is
   begin -- Headers
      Object.Headers := Headers;
   end Headers;

   procedure Parameters (Object : in out Data; Parameters : in CGI.Parameters.List) is
   begin -- Parameters
      Object.Parameters := Parameters;
   end Parameters;

   procedure Payload (Object : in out Data; Payload : in Ada.Streams.Stream_Element_Array) is
   begin -- Payload
      Object.Payload := +Text_Streams.To_String (Payload);
   end Payload;
end Solid.CGI.Request.Set;
