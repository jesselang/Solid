with Solid.Web.Cookies;
with Solid.Web.Headers;
with Solid.Web.Session.Files;
with Solid.Web.Standard;
with Solid.Web.Request;
with Solid.Web.Response;
with Solid.Strings;

with Solid.Web.Environment;
with Solid.Web.Standard.Environment;

use Solid.Web;

procedure Web_Test is
   function Cookie_Test (Client : Request.Data) return Response.Data is
      Result  : Response.Data;
      Headers : Solid.Web.Headers.List;
      Cookies : Solid.Web.Cookies.List;
   begin -- Cookie_Test
      Cookies := Request.Cookies (Client);
      if Cookies.Exists (Name => "Solid") then
         return Response.Build ("text/plain", Message_Body => "The cookie has been set with the value """ &
                                                              Cookies.Get (Name => "Solid") & """");
      else

         Solid.Web.Cookies.Set (Headers => Headers, Name => "Solid", Value => "Jesse");

         return Response.Build ("text/plain", Message_Body => "Set a cookie " & Solid.Web.Environment.Value
                                                                             (Object  => Solid.Web.Standard.Environment.Current,
                                                                              Name    => Solid.Web.Environment.HTTP_Cookie),
                                                                              Headers => Headers);
      end if;
   end Cookie_Test;

   function Session_Test (Client : Request.Data) return Response.Data is
      Session : Solid.Web.Session.Handle := Request.Session (Client);
      Headers : Solid.Web.Headers.List;

      use type Solid.Web.Session.Handle;
   begin -- Session_Test
      if Session = Solid.Web.Session.No_Session then
         Session := new Solid.Web.Session.Data;
         Request.New_Session (Client, Session => Session.all, Headers => Headers);
         return Response.Build ("text/plain", Message_Body => "No session exists, so one was created.", Headers => Headers);
      else
         --return Response.Build ("text/plain", Message_Body => "A session exists.");
         return Response.URL ("/post_test.xml");
      end if;

      --~ return Response.URL ("/post_test.xml");
   end Session_Test;

   Context : Solid.Web.Session.Storage.Context_Handle := Solid.Web.Session.Files.Initialize (Path => "/tmp/solid-sessions");
   procedure Test is new Solid.Web.Standard.Program (Process => Solid.Web.Response.Test, Session_Context => Context);
   procedure Sessions is new Solid.Web.Standard.Program (Process => Session_Test, Session_Context => Context);

   procedure Cookies is new Solid.Web.Standard.Program (Process => Cookie_Test, Session_Context => Context);
begin -- Web_Test
   --Test;
   Sessions;

   --Cookies;
end Web_Test;
