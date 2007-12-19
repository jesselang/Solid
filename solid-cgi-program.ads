with Solid.CGI.Request;
with Solid.CGI.Response;

generic
   with procedure Process (Request : Request.Data) return Response.Data;
procedure Solid.CGI.Program;
