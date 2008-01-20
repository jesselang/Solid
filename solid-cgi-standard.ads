with Solid.CGI.Request;
with Solid.CGI.Response;

package Solid.CGI.Standard is
   type Buffer_Size is new Positive;

   generic -- Program
      with function Process (Client : Request.Data) return Response.Data;
      Payload_Read : Buffer_Size := 1024;
   procedure Program;
end Solid.CGI.Standard;
