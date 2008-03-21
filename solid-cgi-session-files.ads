-- Store session data in a file system.  Intended for standard CGI use.
private with Ada.Calendar;
--~ private with Ada.Containers.Ordered_Sets;
--~ private with Ada.Sequential_IO;
private with Solid.Calendar;

package Solid.CGI.Session.Files is
   type Context is new Session.Context with private;

   function Initialize (Path : String; Name : String := "Session"; Lifetime : Duration := Duration'Last) return Context_Handle;

   procedure Set_Path (Settings : in out Context'Class; To : in String);

   function Path (Settings : Context'Class) return String;
private -- Solid.CGI.Session.Files
   type Context is new Session.Context with record
      Path      : Strings.U_String;
   end record;

   overriding
   procedure Initialize (Settings : in out Context);

   overriding
   procedure Finalize (Settings : in out Context) is null;

   overriding
   function Exists (Settings : Context; Session : Data'Class) return Boolean;

   overriding
   procedure Create (Settings : in out Context; Session : in out Data'Class);

   overriding
   procedure Delete (Settings : in out Context; Session : in out Data'Class);

   overriding
   procedure Read (Settings : in out Context; Identity : in String; Session : out Data'Class);

   overriding
   procedure Write (Settings : in out Context; Session : in out Data'Class);

   overriding
   procedure Close (Settings : in out Context; Session : in out Data'Class);

   type Local_Context is access all Context;
end Solid.CGI.Session.Files;
