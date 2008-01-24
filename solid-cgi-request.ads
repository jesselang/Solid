private with Solid.Strings;

with Ada.Calendar;
with Ada.Finalization;
with Ada.Streams;
with Solid.CGI.Cookies;
with Solid.CGI.Environment;
with Solid.CGI.Headers;
with Solid.CGI.Parameters;

package Solid.CGI.Request is
   No_Environment : exception;

   type Data is new Ada.Finalization.Controlled with private;

   type Request_Method is (Get, Post);

   function Method (Object : Data) return Request_Method;

   function URI (Object : Data) return String;

   function Path (Object : Data) return String;

   function Translated_Path (Object : Data) return String;

   type Count is range -1 .. Integer'Last;

   Not_Set : constant Count := -1;

   function Content_Length (Object : Data) return Count;

   function Content_Type (Object : Data) return String;

   function Query (Object : Data) return String;

   function Program_Name (Object : Data) return String;

   function Script_Name (Object : Data) return String renames Program_Name;

   function Document_Root (Object : Data) return String;

   function User_Agent (Object : Data) return String;

   -- Referrer

   -- Cookies

   function Host (Object : Data) return String;

   function Server_Name (Object : Data) return String;

   function Server_Admin (Object : Data) return String;

   function Server_Software (Object : Data) return String;

   function Server_Protocol (Object : Data) return String;

   function Server_Signature (Object : Data) return String;

   function Server_Address (Object : Data) return String;

   type Port_Number is range 0 .. 65535;

   No_Port : constant Port_Number := 0;

   function Server_Port (Object : Data) return Port_Number;

   function Remote_Address (Object : Data) return String;

   function Remote_Port (Object : Data) return Port_Number;

   function Environment (Object : Data) return CGI.Environment.Handle;

   function Headers (Object : Data) return CGI.Headers.List;

   function Cookies (Object : Data) return CGI.Cookies.List;

   function Parameters (Object : Data) return CGI.Parameters.List;

   function Payload (Object : Data) return Ada.Streams.Stream_Element_Array;
private -- Solid.CGI.Request
   type Data is new Ada.Finalization.Controlled with record
      Created            : Ada.Calendar.Time;
      Environment        : CGI.Environment.Handle;
      Post_Query         : Strings.U_String;
      Headers            : CGI.Headers.List;
      Cookies            : CGI.Cookies.List;
      Parameters         : CGI.Parameters.List;
      Payload            : Strings.U_String;
   end record;

   pragma Assert (Ada.Streams.Stream_Element'Size = 8);

   overriding procedure Initialize (Object : in out Data);
end Solid.CGI.Request;
