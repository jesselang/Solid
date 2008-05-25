with Ada.Calendar;
with GNAT.String_Split;
with Solid.Web.Containers.Tables;
with Solid.Strings;
with Solid.Text_Streams;

use Solid.Strings;

with Ada.Text_IO;

package body Solid.Web.Request is
   procedure Validate_Environment (Object : in Data) is
      use type Web.Environment.Handle;
   begin -- Validate_Environment
      if Object.Environment = null then
         raise No_Environment;
      end if;
   end Validate_Environment;

   function Method (Object : Data) return Request_Method is
   begin -- Method
      Validate_Environment (Object => Object);

      return Request_Method'Value (Web.Environment.Value (Object.Environment, Name => Web.Environment.Request_Method) );
   end Method;

   function URI (Object : Data) return String is
   begin -- URI
      Validate_Environment (Object => Object);

      return Web.Environment.Value (Object.Environment, Name => Web.Environment.Request_URI);
   end URI;

   function Path (Object : Data) return String is
   begin -- Path
      Validate_Environment (Object => Object);

      return Web.Environment.Value (Object.Environment, Name => Web.Environment.Path_Info);
   end Path;

   function Translated_Path (Object : Data) return String is
   begin -- Translated_Path
      Validate_Environment (Object => Object);

      return Web.Environment.Value (Object.Environment, Name => Web.Environment.Path_Translated);
   end Translated_Path;

   function Content_Length (Object : Data) return Count is
   begin -- Content_Length
      Validate_Environment (Object => Object);

      return Count'Value (Web.Environment.Value (Object.Environment, Name => Web.Environment.Content_Length) );
   exception -- Content_Length
      when Constraint_Error =>
         return Not_Set;
   end Content_Length;

   function Content_Type (Object : Data) return String is
   begin -- Content_Type
      Validate_Environment (Object => Object);

      return Web.Environment.Value (Object.Environment, Name => Web.Environment.Content_Type);
   end Content_Type;

   function Query (Object : Data) return String is
   begin -- Query
      Validate_Environment (Object => Object);
      -- Method?
      return Web.Environment.Value (Object.Environment, Name => Web.Environment.Query_String);
   end Query;

   function Program_Name (Object : Data) return String is
   begin -- Program_Name
      Validate_Environment (Object => Object);

      return Web.Environment.Value (Object.Environment, Name => Web.Environment.Script_Name);
   end Program_Name;

   function Document_Root (Object : Data) return String is
   begin -- Document_Root
      Validate_Environment (Object => Object);

      return Web.Environment.Value (Object.Environment, Name => Web.Environment.Document_Root);
   end Document_Root;

   function User_Agent (Object : Data) return String is
   begin -- User_Agent
      Validate_Environment (Object => Object);

      return Web.Environment.Value (Object.Environment, Name => Web.Environment.HTTP_User_Agent);
   end User_Agent;

   function Host (Object : Data) return String is
   begin -- Host
      Validate_Environment (Object => Object);

      return Web.Environment.Value (Object.Environment, Name => Web.Environment.HTTP_Host);
   end Host;

   function Server_Name (Object : Data) return String is
   begin -- Server_Name
      Validate_Environment (Object => Object);

      return Web.Environment.Value (Object.Environment, Name => Web.Environment.Server_Name);
   end Server_Name;

   function Server_Admin (Object : Data) return String is
   begin -- Server_Admin
      Validate_Environment (Object => Object);

      return Web.Environment.Value (Object.Environment, Name => Web.Environment.Server_Admin);
   end Server_Admin;

   function Server_Software (Object : Data) return String is
   begin -- Server_Software
      Validate_Environment (Object => Object);

      return Web.Environment.Value (Object.Environment, Name => Web.Environment.Server_Software);
   end Server_Software;

   function Server_Protocol (Object : Data) return String is
   begin -- Server_Protocol
      Validate_Environment (Object => Object);

      return Web.Environment.Value (Object.Environment, Name => Web.Environment.Server_Protocol);
   end Server_Protocol;

   function Server_Signature (Object : Data) return String is
   begin -- Server_Signature
      Validate_Environment (Object => Object);

      return Web.Environment.Value (Object.Environment, Name => Web.Environment.Server_Signature);
   end Server_Signature;

   function Server_Address (Object : Data) return String is
   begin -- Server_Address
      Validate_Environment (Object => Object);

      return Web.Environment.Value (Object.Environment, Name => Web.Environment.Server_Addr);
   end Server_Address;

   function Server_Port (Object : Data) return Port_Number is
   begin -- Server_Port
      Validate_Environment (Object => Object);

      return Port_Number'Value (Web.Environment.Value (Object.Environment, Name => Web.Environment.Server_Port) );
   exception -- Server_Port
      when Constraint_Error =>
         return No_Port;
   end Server_Port;

   function Remote_Address (Object : Data) return String is
   begin -- Remote_Address
      Validate_Environment (Object => Object);

      return Web.Environment.Value (Object.Environment, Name => Web.Environment.Remote_Addr);
   end Remote_Address;

   function Remote_Port (Object : Data) return Port_Number is
   begin -- Remote_Port
      Validate_Environment (Object => Object);

      return Port_Number'Value (Web.Environment.Value (Object.Environment, Name => Web.Environment.Server_Port) );
   exception -- Remote_Port
      when Constraint_Error =>
         return No_Port;
   end Remote_Port;

   function Environment (Object : Data) return Web.Environment.Handle is
   begin -- Environment
      return Object.Environment;
   end Environment;

   function Headers (Object : Data) return Web.Headers.List is
   begin -- Headers
      return Object.Headers;
   end Headers;

   function Cookies (Object : Data) return Web.Cookies.List is
   begin -- Cookies
      return Object.Cookies;
   end Cookies;

   function Parameters (Object : Data) return Web.Parameters.List is
   begin -- Parameters
      return Object.Parameters;
   end Parameters;

   function Payload (Object : Data) return Ada.Streams.Stream_Element_Array is
   begin -- Payload
      return Text_Streams.To_Stream (+Object.Payload);
   end Payload;

   function Session (Object : Data) return Boolean is
      Cookies : constant Web.Cookies.List := Request.Cookies (Object);

      use type Web.Session.Storage.Context_Handle;
   begin -- Session
      if Object.Session_Context = Web.Session.Storage.No_Context then
         raise Web.Session.Invalid_Context with "Session: No session context.";
      end if;

      declare
         Session_Name : constant String := Object.Session_Context.Name;
      begin
         return Cookies.Exists (Session_Name);
      end;
   end Session;

   function Session (Object : Data) return Web.Session.Handle is
   begin -- Session
      if not Session (Object) then
         Ada.Text_IO.Put_Line (Ada.Text_IO.Current_Error, "not Session (Object)");
         return Web.Session.No_Session;
         -------------------------------------------
         -- Once FSF GNAT supports extended return statements, Web.Session.Data can be directly returned.
         --~ return No_Session : Web.Session.Data do
            --~ null; -- An invalid session.
         --~ end return;
      end if;

      Read_Session : declare
         Session_Name : constant String           := Object.Session_Context.Name;
         Cookies      : constant Web.Cookies.List := Request.Cookies (Object);
      begin
         return Web.Session.Read (From => Object.Session_Context, Identity => Cookies.Get (Name => Session_Name) );
      exception -- Read_Session
         when Web.Session.Not_Found =>
            Ada.Text_IO.Put_Line (Ada.Text_IO.Current_Error, "Not_Found");
            -- In the case where a session cookie is found, but no data is found.
            -- If this were a procedure, we could remove the cookie from the data,
            -- so an appropriate check could be made in New_Session.

            return Web.Session.No_Session;
            -------------------------------------------
            -- Once FSF GNAT supports extended return statements, Web.Session.Data can be directly returned.
            --~ return No_Session : Web.Session.Data do
               --~ null; -- An invalid session.
            --~ end return;
      end Read_Session;
   end Session;

   procedure New_Session (Object : in Data; Session : out Web.Session.Data; Headers : in out Web.Headers.List) is
   begin -- New_Session
      -- This would be an appropriate check mentioned above.
      --~ if Request.Session (Object) then
         --~ raise Web.Session.Invalid_Context with "New_Session: Session already exists.";
      --~ end if;

      declare
         Session_Name : constant String := Object.Session_Context.Name;
      begin
         Web.Session.Create (Settings => Object.Session_Context, Session => Session);
         Web.Cookies.Set (Headers, Name => Session_Name, Value => Web.Session.Identity (Session) );
      end;
   end New_Session;

   procedure Initialize (Object : in out Data) is
   begin -- Initialize
      Object.Created := Ada.Calendar.Clock;
   end Initialize;
end Solid.Web.Request;
