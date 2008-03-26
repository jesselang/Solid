-- "Standard" CGI support.
with Solid.CGI.Request;
with Solid.CGI.Response;
with Solid.CGI.Session;

package Solid.CGI.Standard is
   Invalid_Gateway : exception;

   type Buffer_Size is new Positive;

   generic -- Program
      with function Process (Client : Request.Data) return Response.Data;
      Session_Context : Session.Storage.Context_Handle := Session.Storage.No_Context;
      Payload_Read    : Buffer_Size                    := 1024;
   procedure Program;
   -- High-level abstraction for a standard CGI program.
   -- Program creates a request object based on the CGI standard.
   -- The request object is passed to Process, which should return a response object.
   -- Exceptions propagated out of Process will displayed to the client.
   -- To use sessions, a valid context must be given as Session_Context.
   -- See Solid.CGI.Session.Files for a basic storage implementation.
   -- Payload_Read is the size of the buffer used to read the request payload.
   -- The default should be fine in most cases.  This is probably bad design, in which
   -- case, hopefully it will disappear.
   -- Get and Post request methods are supported.
   -- If the request method is Post, only basic form data is supported.
   -- Raises Invalid_Gateway if basic sanity checks or program setup fails.
end Solid.CGI.Standard;
