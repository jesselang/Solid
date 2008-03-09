-- Store session data in a file system.  Intended for standard CGI use.
private with Ada.Calendar;
private with Ada.Containers.Ordered_Sets;
private with Ada.Sequential_IO;
private with Solid.Calendar;

package Solid.CGI.Session.Files is
   type Context is new Session.Context with private;

   function Initialize (Path : String; Name : String := "Session"; Lifetime : Duration := Duration'Last) return Context_Handle;

   procedure Set_Path (Settings : in out Context'Class; To : in String);

   overriding
   procedure Initialize_Implementation (Settings : in out Context);

   function Path (Settings : Context'Class) return String;

   type Data is new Session.Data with private;

   overriding
   function New_Session (Settings : Context) return Session.Data'Class;

   overriding
   procedure Create_Implementation (Session : in out Data);

   overriding
   procedure Delete_Implementation (Session : in out Data);

   overriding
   procedure Read_Implementation (From : in out Context; Identity : in String; To : out Session.Data'Class);
   -- Reads Session from storage.
   -- Raises Not_Found if Session was not found in storage.

   overriding
   procedure Write_Implementation (Session : in out Data);

private -- Solid.CGI.Session.Files
   type Index_Entry is record
      Identity : Session_Identity  := No_Identity;
      Expires  : Ada.Calendar.Time := Calendar.No_Time;
      In_Use   : Boolean           := False;
   end record;

   package Index_IO is new Ada.Sequential_IO (Index_Entry);

   function "<" (Left : Index_Entry; Right : Index_Entry) return Boolean;
   function "=" (Left : Index_Entry; Right : Index_Entry) return Boolean;

   package Index_Sets is new Ada.Containers.Ordered_Sets (Element_Type => Index_Entry);

   type Context is new Session.Context with record
      Path      : Strings.U_String;
      Index     : Index_IO.File_Type;
      Index_Set : Index_Sets.Set;
   end record;

   type Local_Context is access all Context;

   type Data is new Session.Data with null record;
end Solid.CGI.Session.Files;
