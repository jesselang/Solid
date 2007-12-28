with Solid.CGI.Request;
with Solid.CGI.Response;

package Solid.CGI.Standard is
   generic -- Program
      with function Process (Client : Request.Data) return Response.Data;
   procedure Program;
end Solid.CGI.Standard;
