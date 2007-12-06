private with Ada.Calendar;
private with Ada.Strings.Unbounded;
private with Solid.CGI.Headers;

with Ada.Finalization;

package Solid.CGI.Request is
   type Request_Method is (Get, Post);

   type Data is new Ada.Finalization.Controlled with private;

   function Method (Request : Data) return Request_Method;

   -- To be used by the response package, not for external use.
--   type Request_Identifier is private;

--   procedure Release (Identifier : in Request_Identifier);
   -- Releases Identifier to be used for another request/response pair.
private -- Solid.CGI.Request
--   type Request_Identifier is new Integer;

   type Data is new Ada.Finalization.Controlled with record
--      Identifier      : Request_Identifier;
      Created         : Ada.Calendar.Time;
      Handled         : Boolean := False;
      Method          : Request_Method;
      URI             : Ada.Strings.Unbounded.Unbounded_String;
      Message_Headers : Headers.List;
      Message_Body    : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   overriding procedure Initialize (Object : in out Data);
end Solid.CGI.Request;
