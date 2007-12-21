with Solid.CGI.Program;
with Solid.CGI.Request;
with Solid.CGI.Response;

use Solid.CGI;

procedure CGI_Test is
   function Nothing (Client : Request.Data) return Response.Data is
      Result : constant Response.Data := Response.Test;
   begin -- Nothing
      return Result;
   end Nothing;

   procedure Do_Nothing is new Program (Process => Nothing);
begin -- CGI_Test
   Do_Nothing;
end CGI_Test;
