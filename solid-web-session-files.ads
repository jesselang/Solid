-- A basic storage method suitable for "standard" low-volume web applications.
-- Stores session data in the file system.  Intended for standard CGI use.
private with Ada.Calendar;
private with Solid.Calendar;

package Solid.Web.Session.Files is
   type Context is new Storage.Context with private;

   function Initialize (Path     : String;
                        Name     : String                   := "Session";
                        Lifetime : Storage.Storage_Lifetime := Storage.Storage_Lifetime'Last)
   return Storage.Context_Handle;

   procedure Set_Path (Settings : in out Context'Class; To : in String);

   function Path (Settings : Context'Class) return String;
private -- Solid.Web.Session.Files
   type Context is new Storage.Context with record
      Path      : Strings.U_String;
   end record;

   overriding
   procedure Initialize (Settings : in out Context);

   overriding
   procedure Finalize (Settings : in out Context) is null;

   overriding
   function Exists (Settings : Context; Session : Data) return Boolean;

   overriding
   procedure Create (Settings : in out Context; Session : in out Data);

   overriding
   procedure Delete (Settings : in out Context; Session : in out Data);

   overriding
   procedure Read (Settings : in out Context; Identity : in String; Session : out Data);

   overriding
   procedure Write (Settings : in out Context; Session : in out Data);

   overriding
   procedure Close (Settings : in out Context; Session : in out Data);
end Solid.Web.Session.Files;
