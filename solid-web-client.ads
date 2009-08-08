with Solid.Strings;
with Solid.Web.Headers;
with Solid.Web.Parameters;
with Solid.Web.Response;

package Solid.Web.Client is
   type Timeout_Value is new Duration;

   Forever : constant Timeout_Value;

   type Connection_Timeout is record
      Connect  : Timeout_Value;
      Send     : Timeout_Value;
      Receive  : Timeout_Value;
      Response : Timeout_Value;
   end record;

   No_Timeout : constant Connection_Timeout;

   type Authorization_Method is (Basic, Digest);

   type Authorization_Info is record
      Method   : Authorization_Method := Basic;
      Username : Strings.U_String;
      Password : Strings.U_String;
   end record;

   No_Authorization : constant Authorization_Info;

   subtype Headers_List is Headers.List;
   No_Headers : constant Headers_List := Headers.No_Headers;

   Host_Error : exception;
   Timed_Out  : exception;

   function Get (URL         : String;
                 Timeout     : Connection_Timeout := No_Timeout;
                 Credentials : Authorization_Info := No_Authorization;
                 Headers     : Headers_List       := No_Headers)
   return Response.Data;
   -- The username and password in the URL will be ignored.
   -- Raises Host_Error if the host for URL could not be found.
   -- Raises Timed_Out if Timeout is exceeded.


--   function Head

--   function Put

   subtype Parameters_List is Parameters.List;

   function Post (URL         : String;
                  Parameters  : Parameters_List;
                  Timeout     : Connection_Timeout := No_Timeout;
                  Credentials : Authorization_Info := No_Authorization;
                  Headers     : Headers_List       := No_Headers)
   return Response.Data;
   -- The username and password in the URL will be ignored.
   -- Raises Host_Error if the host for URL could not be found.
   -- Raises Timed_Out if Timeout is exceeded.
private -- Solid.Web.Client
   Forever : constant Timeout_Value := Timeout_Value'Last;

   No_Timeout : constant Connection_Timeout := (others => Forever);

   No_Authorization : constant Authorization_Info := (others => <>);
end Solid.Web.Client;
