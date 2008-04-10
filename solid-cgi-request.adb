with Ada.Calendar;
with GNAT.String_Split;
with Solid.CGI.Containers.Tables;
with Solid.Strings;
with Solid.Text_Streams;

use Solid.Strings;

package body Solid.CGI.Request is
   procedure Validate_Environment (Object : in Data) is
      use type CGI.Environment.Handle;
   begin -- Validate_Environment
      if Object.Environment = null then
         raise No_Environment;
      end if;
   end Validate_Environment;

   function Method (Object : Data) return Request_Method is
   begin -- Method
      Validate_Environment (Object => Object);

      return Request_Method'Value (CGI.Environment.Value (Object.Environment, Name => CGI.Environment.Request_Method) );
   end Method;

   function URI (Object : Data) return String is
   begin -- URI
      Validate_Environment (Object => Object);

      return CGI.Environment.Value (Object.Environment, Name => CGI.Environment.Request_URI);
   end URI;

   function Path (Object : Data) return String is
   begin -- Path
      Validate_Environment (Object => Object);

      return CGI.Environment.Value (Object.Environment, Name => CGI.Environment.Path_Info);
   end Path;

   function Translated_Path (Object : Data) return String is
   begin -- Translated_Path
      Validate_Environment (Object => Object);

      return CGI.Environment.Value (Object.Environment, Name => CGI.Environment.Path_Translated);
   end Translated_Path;

   function Content_Length (Object : Data) return Count is
   begin -- Content_Length
      Validate_Environment (Object => Object);

      return Count'Value (CGI.Environment.Value (Object.Environment, Name => CGI.Environment.Content_Length) );
   exception -- Content_Length
      when Constraint_Error =>
         return Not_Set;
   end Content_Length;

   function Content_Type (Object : Data) return String is
   begin -- Content_Type
      Validate_Environment (Object => Object);

      return CGI.Environment.Value (Object.Environment, Name => CGI.Environment.Content_Type);
   end Content_Type;

   function Query (Object : Data) return String is
   begin -- Query
      Validate_Environment (Object => Object);
      -- Method?
      return CGI.Environment.Value (Object.Environment, Name => CGI.Environment.Query_String);
   end Query;

   function Program_Name (Object : Data) return String is
   begin -- Program_Name
      Validate_Environment (Object => Object);

      return CGI.Environment.Value (Object.Environment, Name => CGI.Environment.Script_Name);
   end Program_Name;

   function Document_Root (Object : Data) return String is
   begin -- Document_Root
      Validate_Environment (Object => Object);

      return CGI.Environment.Value (Object.Environment, Name => CGI.Environment.Document_Root);
   end Document_Root;

   function User_Agent (Object : Data) return String is
   begin -- User_Agent
      Validate_Environment (Object => Object);

      return CGI.Environment.Value (Object.Environment, Name => CGI.Environment.HTTP_User_Agent);
   end User_Agent;

   function Host (Object : Data) return String is
   begin -- Host
      Validate_Environment (Object => Object);

      return CGI.Environment.Value (Object.Environment, Name => CGI.Environment.HTTP_Host);
   end Host;

   function Server_Name (Object : Data) return String is
   begin -- Server_Name
      Validate_Environment (Object => Object);

      return CGI.Environment.Value (Object.Environment, Name => CGI.Environment.Server_Name);
   end Server_Name;

   function Server_Admin (Object : Data) return String is
   begin -- Server_Admin
      Validate_Environment (Object => Object);

      return CGI.Environment.Value (Object.Environment, Name => CGI.Environment.Server_Admin);
   end Server_Admin;

   function Server_Software (Object : Data) return String is
   begin -- Server_Software
      Validate_Environment (Object => Object);

      return CGI.Environment.Value (Object.Environment, Name => CGI.Environment.Server_Software);
   end Server_Software;

   function Server_Protocol (Object : Data) return String is
   begin -- Server_Protocol
      Validate_Environment (Object => Object);

      return CGI.Environment.Value (Object.Environment, Name => CGI.Environment.Server_Protocol);
   end Server_Protocol;

   function Server_Signature (Object : Data) return String is
   begin -- Server_Signature
      Validate_Environment (Object => Object);

      return CGI.Environment.Value (Object.Environment, Name => CGI.Environment.Server_Signature);
   end Server_Signature;

   function Server_Address (Object : Data) return String is
   begin -- Server_Address
      Validate_Environment (Object => Object);

      return CGI.Environment.Value (Object.Environment, Name => CGI.Environment.Server_Addr);
   end Server_Address;

   function Server_Port (Object : Data) return Port_Number is
   begin -- Server_Port
      Validate_Environment (Object => Object);

      return Port_Number'Value (CGI.Environment.Value (Object.Environment, Name => CGI.Environment.Server_Port) );
   exception -- Server_Port
      when Constraint_Error =>
         return No_Port;
   end Server_Port;

   function Remote_Address (Object : Data) return String is
   begin -- Remote_Address
      Validate_Environment (Object => Object);

      return CGI.Environment.Value (Object.Environment, Name => CGI.Environment.Remote_Addr);
   end Remote_Address;

   function Remote_Port (Object : Data) return Port_Number is
   begin -- Remote_Port
      Validate_Environment (Object => Object);

      return Port_Number'Value (CGI.Environment.Value (Object.Environment, Name => CGI.Environment.Server_Port) );
   exception -- Remote_Port
      when Constraint_Error =>
         return No_Port;
   end Remote_Port;

   function Environment (Object : Data) return CGI.Environment.Handle is
   begin -- Environment
      return Object.Environment;
   end Environment;

   function Headers (Object : Data) return CGI.Headers.List is
   begin -- Headers
      return Object.Headers;
   end Headers;

   function Cookies (Object : Data) return CGI.Cookies.List is
   begin -- Cookies
      return Object.Cookies;
   end Cookies;

   function Parameters (Object : Data) return CGI.Parameters.List is
   begin -- Parameters
      return Object.Parameters;
   end Parameters;

   function Payload (Object : Data) return Ada.Streams.Stream_Element_Array is
   begin -- Payload
      return Text_Streams.To_Stream (+Object.Payload);
   end Payload;

   function Session (Object : Data) return Boolean is
      Cookies : constant CGI.Cookies.List := Request.Cookies (Object);

      use type CGI.Session.Storage.Context_Handle;
   begin -- Session
      if Object.Session_Context = CGI.Session.Storage.No_Context then
         raise CGI.Session.Invalid_Context with "Session: No session context.";
      end if;

      declare
         Session_Name : constant String := Object.Session_Context.Name;
      begin
         return Cookies.Exists (Session_Name);
      end;
   end Session;

   function Session (Object : Data) return CGI.Session.Data is
      Cookies    : constant CGI.Cookies.List := Request.Cookies (Object);
   begin -- Session
      if not Session (Object) then
         return No_Session : CGI.Session.Data do
            null; -- An invalid session.
         end return;
      end if;

      declare
         Session_Name : constant String := Object.Session_Context.Name;
      begin
         return CGI.Session.Read (From => Object.Session_Context, Identity => Cookies.Get (Name => Session_Name) );
      end;
   end Session;

   procedure New_Session (Object : in Data; Session : out CGI.Session.Data; Headers : in out CGI.Headers.List) is
   begin -- New_Session
      if Request.Session (Object) then
         raise CGI.Session.Invalid_Context with "New_Session: Session already exists.";
      end if;

      declare
         Session_Name : constant String := Object.Session_Context.Name;
      begin
         CGI.Session.Create (Settings => Object.Session_Context, Session => Session);
         CGI.Cookies.Set (Headers, Name => Session_Name, Value => CGI.Session.Identity (Session) );
      end;
   end New_Session;

   procedure Initialize (Object : in out Data) is
   begin -- Initialize
      Object.Created := Ada.Calendar.Clock;
   end Initialize;
end Solid.CGI.Request;
