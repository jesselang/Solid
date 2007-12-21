with Solid.CGI.Request;
with Solid.CGI.Response;

generic
   with function Process (Client : Request.Data) return Response.Data;
procedure Solid.CGI.Program;
