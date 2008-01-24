with Solid.CGI.Cookies;
with Solid.CGI.Headers;
with Solid.CGI.Standard;
with Solid.CGI.Request;
with Solid.CGI.Response;
with Solid.Strings;

with Solid.CGI.Environment;
with Solid.CGI.Standard.Environment;

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
      Cookies : Solid.CGI.Cookies.List;
   begin -- Cookie_Test
      Cookies := Client.Cookies;
      if Cookies.Exists (Name => "Solid") then
         return Response.Build ("text/plain", Message_Body => "The cookie has been set with the value """ &
                                                              Cookies.Get (Name => "Solid") & """");
         --~ return Response.Build ("text/plain", Message_Body => "Cookies """ & Solid.CGI.Environment.Value
                                                                             --~ (Object => Solid.CGI.Standard.Environment.Current,
                                                                              --~ Name   => Solid.CGI.Environment.HTTP_Cookie)
                                                              --~ & """");
      else

         Solid.CGI.Cookies.Set (Headers => Headers,
                                Name    => "Solid",
                                Value   => "Jesse");

         return Response.Build ("text/plain", Message_Body => "Set a cookie " & Solid.CGI.Environment.Value
                                                                             (Object => Solid.CGI.Standard.Environment.Current,
                                                                              Name   => Solid.CGI.Environment.HTTP_Cookie)
                                                                              , Headers => Headers);
      end if;
   end Cookie_Test;

   procedure Test is new Solid.CGI.Standard.Program (Process => Solid.CGI.Response.Test);

   procedure Cookie is new Solid.CGI.Standard.Program (Process => Cookie_Test);
   --Query : Solid.CGI.Parameters.List := Solid.CGI.Parameters.Parse_URL_Encoding ("some=thing+is%20screwy here&else=do");
begin -- CGI_Test
   -- null;
   -- Test;

   Cookie;
end CGI_Test;
