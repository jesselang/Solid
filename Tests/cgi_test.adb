with Solid.CGI.Cookies;
with Solid.CGI.Headers;
with Solid.CGI.Standard;
with Solid.CGI.Request;
with Solid.CGI.Response;
use Solid.CGI;

procedure CGI_Test is
   --~ function Nothing (Client : Request.Data) return Response.Data is
      --~ Result : constant Response.Data := Response.Test;
   --~ begin -- Nothing
      --~ return Result;
   --~ end Nothing;

   function Cookie_Test (Client : Request.Data) return Response.Data is
      Result  : Response.Data;
      Headers : Solid.CGI.Headers.List;
   begin -- Cookie_Test
      Cookies.Set (Headers => Headers,
                   Name    => "Solid",
                   Value   => "Jesse");

      return Response.Build ("text/plain", Message_Body => "Set a cookie.", Headers => Headers);
   end Cookie_Test;

--   procedure Test is new Solid.CGI.Standard.Program (Process => Solid.CGI.Response.Test);

   procedure Cookie is new Solid.CGI.Standard.Program (Process => Cookie_Test);
   --Query : Solid.CGI.Parameters.List := Solid.CGI.Parameters.Parse_URL_Encoding ("some=thing+is%20screwy here&else=do");
begin -- CGI_Test
   -- null;
   --Test;

   Cookie;
end CGI_Test;
