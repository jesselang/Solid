package body Solid.CGI.Request is
   function Method (Request : Data) return Request_Method is
   begin -- Method
      return Request.Method;
   end Method;
end Solid.CGI.Request;
