with Ada.Calendar;
with Solid.Strings;

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

   function Query (Object : Data) return String is
   begin -- Query
      Validate_Environment (Object => Object);

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

   function Parameters (Object : Data) return CGI.Parameters.List is
   begin -- Parameters
      return Object.Parameters;
   end Parameters;

   overriding procedure Initialize (Object : in out Data) is
   begin -- Initialize
      Object.Created := Ada.Calendar.Clock;
   end Initialize;
end Solid.CGI.Request;
