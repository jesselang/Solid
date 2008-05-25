-- Representation of a HTTP request.
private with Solid.Strings;
private with Ada.Finalization;
with Ada.Calendar;
with Ada.Streams;
with Solid.Web.Cookies;
with Solid.Web.Environment;
with Solid.Web.Headers;
with Solid.Web.Parameters;
with Solid.Web.Session;

package Solid.Web.Request is
   No_Environment : exception;

   type Data is private;

   -- Request environment information.
   -- The following environment related functions may raise No_Environment if no environment is available for the request.
   type Request_Method is (Get, Post);

   function Method (Object : Data) return Request_Method;
   -- Returns the request method.

   function URI (Object : Data) return String;
   -- Returns the URI location.

   function Path (Object : Data) return String;
   -- Returns the path.

   function Translated_Path (Object : Data) return String;
   -- Returns the translated path.

   type Count is range -1 .. Integer'Last;

   Not_Set : constant Count := -1;

   function Content_Length (Object : Data) return Count;
   -- Returns the content length.

   function Content_Type (Object : Data) return String;
   -- Returns the content type.

   function Query (Object : Data) return String;
   -- Returns the query.

   function Program_Name (Object : Data) return String;
   -- Returns the program name.  It would be called Script_Name if this were a script. :^)

   function Document_Root (Object : Data) return String;
   -- Returns the document root.

   function User_Agent (Object : Data) return String;
   -- Returns the user agent.

   -- Referrer

   function Host (Object : Data) return String;
   -- Returns the host.

   function Server_Name (Object : Data) return String;
   -- Returns the server name.

   function Server_Admin (Object : Data) return String;
   -- Returns the server admin.

   function Server_Software (Object : Data) return String;
   -- Returns the server software.

   function Server_Protocol (Object : Data) return String;
   -- Returns the server protocol.

   function Server_Signature (Object : Data) return String;
   -- Returns the server signature.

   function Server_Address (Object : Data) return String;
   -- Returns the server address.

   type Port_Number is range 0 .. 65535;

   No_Port : constant Port_Number := 0;

   function Server_Port (Object : Data) return Port_Number;
   -- Returns the server port.

   function Remote_Address (Object : Data) return String;
   -- Returns the remote (client) address.

   function Remote_Port (Object : Data) return Port_Number;
   -- Returns the remote (client) port.

   -- Components of the request.
   function Environment (Object : Data) return Web.Environment.Handle;
   -- Returns the environment handle.  This could be used to get non-standard information from the environment.

   function Headers (Object : Data) return Web.Headers.List;
   -- Returns the list of headers.

   function Cookies (Object : Data) return Web.Cookies.List;
   -- Returns the list of cookies.  The cookie used for session data will not be in this list.

   function Parameters (Object : Data) return Web.Parameters.List;
   -- Returns the list of parameters.

   function Payload (Object : Data) return Ada.Streams.Stream_Element_Array;
   -- Returns the payload.

   function Session (Object : Data) return Web.Session.Handle;
   -- Returns the handle to the session information, if one exists.
   -- Returns Web.Session.No_Session if no session exists.

   procedure New_Session (Object : in Data; Session : out Web.Session.Data; Headers : in out Web.Headers.List);
   -- Creates a new session information object in Session, using the information in Object.
   -- Sets a cookie in Headers.  Headers must be passed with the response so the browser receives the cookie.
private -- Solid.Web.Request
   type Data is new Ada.Finalization.Controlled with record
      Created            : Ada.Calendar.Time;
      Environment        : Web.Environment.Handle;
      Post_Query         : Strings.U_String;
      Headers            : Web.Headers.List;
      Cookies            : Web.Cookies.List;
      Parameters         : Web.Parameters.List;
      Payload            : Strings.U_String;
      Session_Context    : Web.Session.Storage.Context_Handle;
   end record;

   overriding
   procedure Initialize (Object : in out Data);

   pragma Assert (Ada.Streams.Stream_Element'Size = 8);
end Solid.Web.Request;
