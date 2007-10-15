package Solid.CGI.Request is
   type Request_Method is (Get, Post, Head);

   type Data is private;

   function Method (Query : Data) return Request_Method;
private -- Solid.CGI.Request
   type Data is record
   end record;
end Solid.CGI.Request;
