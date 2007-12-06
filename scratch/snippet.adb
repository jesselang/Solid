
declare
   package Method is new Solid.CGI.Request.Discrete_Handler (Element   => Solid.CGI.Request.Request_Method,
                                                             Get_Value => Solid.CGI.Request.Method);

   procedure Get is new Method.Equal_To (Value   => Solid.CGI.Request.Get,
                                         Process => Get_Request);
   procedure Post is new Method.Equal_To (Value   => Solid.CGI.Request.Post,
                                          Process => Post_Request);

   Single_Request : Solid.CGI.Request.Data;
begin
   Request.Queue.Get (Item => Single_Request);
   Get (Item => Single_Request);
   -- What should be done if the request was handled above?  Right now, it would set Single_Request.Handled to True.
   -- Every generic operation would return immediately if Handled (Single_Request).
   Post (Item => Single_Request);

   -- At this point, one of the above operations should have handled the request.  We run a catch-all in case the request
   -- wasn't handled.  This operation puts a "request not handled/not found" response in the response bag.
   Unhandled_Request;

end;
