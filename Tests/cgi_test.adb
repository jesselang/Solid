with Solid.CGI.Standard;
--~ with Solid.CGI.Request;
with Solid.CGI.Response;

--~ use Solid.CGI;

procedure CGI_Test is
   --~ function Nothing (Client : Request.Data) return Response.Data is
      --~ Result : constant Response.Data := Response.Test;
   --~ begin -- Nothing
      --~ return Result;
   --~ end Nothing;

   procedure Test is new Solid.CGI.Standard.Program (Process => Solid.CGI.Response.Test);
begin -- CGI_Test
   Test;
end CGI_Test;
