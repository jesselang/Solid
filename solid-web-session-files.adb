with Ada.Directories;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with GNAT.Lock_Files;
with Solid.Strings;
use Solid.Strings;

package body Solid.Web.Session.Files is
   function Initialize (Path     : String;
                        Name     : String                   := "Session";
                        Lifetime : Storage.Storage_Lifetime := Storage.Storage_Lifetime'Last)
   return Storage.Context_Handle is
      Result : Storage.Context_Handle := new Context;
   begin -- Initialize
      Set_Path (Settings => Context (Result.all), To => Path);
      Storage.Initialize (Settings => Result, Name => Name, Lifetime => Lifetime);

      return Result;
   end Initialize;

   procedure Set_Path (Settings : in out Context'Class; To : in String) is
   begin -- Set_Path
      Settings.Path := +To;
   end Set_Path;

   function Path (Settings : Context'Class) return String is
   begin -- Path
      return +Settings.Path;
   end Path;

   procedure Initialize (Settings : in out Context) is
   begin -- Initialize
      Ada.Directories.Create_Path (New_Directory => +Settings.Path);
   exception -- Initialize
      when Ada.Directories.Name_Error | Ada.Directories.Use_Error =>
         raise Invalid_Context with "could not create path.";
   end Initialize;

   function Data_File (Settings : Context; Identity : String) return String;
   -- Returns the data file name for Session.

   function Lock_File (Settings : Context; Identity : String) return String;
   -- Returns the lock file name for Session.

   function Exists (Settings : Context; Session : Data) return Boolean is
   begin -- Exists
      return Ada.Directories.Exists (Data_File (Settings, Identity => Session.Identity) );
   end Exists;

   procedure Create (Settings : in out Context; Session : in out Data) is
      File   : Ada.Streams.Stream_IO.File_Type;
      Stream : Ada.Streams.Stream_IO.Stream_Access;

      use Ada.Streams;
   begin -- Create
      -- If the session file exists, generate a new identity.
      -- Write the session that we have, which is an empty one.
      GNAT.Lock_Files.Lock_File (Lock_File_Name => Lock_File (Settings, Identity => Session.Identity), Wait => 0.3);
      Stream_IO.Create (File => File, Name => Data_File (Settings, Identity => Session.Identity) );
      Stream := Stream_IO.Stream (File);
      Output (Stream => Stream, Item => Session);
      Stream_IO.Close (File => File);
   end Create;

   procedure Delete (Settings : in out Context; Session : in out Data) is
   begin -- Delete
      Ada.Directories.Delete_File (Name => Data_File (Settings, Identity => Session.Identity) );
      GNAT.Lock_Files.Unlock_File (Lock_File_Name => Lock_File (Settings, Identity => Session.Identity) );
      -- Set Session to some uninitialized value in the parent package?
   end Delete;

   procedure Read (Settings : in out Context; Identity : in String; Session : out Data) is
      File   : Ada.Streams.Stream_IO.File_Type;
      Stream : Ada.Streams.Stream_IO.Stream_Access;

      use Ada.Streams;
   begin -- Read
      GNAT.Lock_Files.Lock_File (Lock_File_Name => Lock_File (Settings, Identity => Identity), Wait => 0.3);
      Stream_IO.Open (File => File, Mode => Stream_IO.In_File, Name => Data_File (Settings, Identity => Identity) );
      Stream := Stream_IO.Stream (File);
      Input (Stream, Session);
      Stream_IO.Close (File => File);
   exception -- Read
      when Stream_IO.Name_Error | Stream_IO.Use_Error =>
         GNAT.Lock_Files.Unlock_File (Lock_File_Name => Lock_File (Settings, Identity => Identity) );
         raise Not_Found;
   end Read;

   procedure Write (Settings : in out Context; Session : in out Data) is
      File   : Ada.Streams.Stream_IO.File_Type;
      Stream : Ada.Streams.Stream_IO.Stream_Access;

      use Ada.Streams;
   begin -- Write
      Stream_IO.Open (File => File, Mode => Stream_IO.Out_File, Name => Data_File (Settings, Identity => Session.Identity) );
      Stream := Stream_IO.Stream (File);
      Output (Stream => Stream, Item => Session);
      Stream_IO.Close (File => File);
   end Write;

   procedure Close (Settings : in out Context; Session : in out Data) is
   begin -- Close
      GNAT.Lock_Files.Unlock_File (Lock_File_Name => Lock_File (Settings, Identity => Session.Identity) );
   end Close;

   function Data_File (Settings : Context; Identity : String) return String is
   begin -- Data_File
      return +Settings.Path & '/' & Identity;
   end Data_File;

   function Lock_File (Settings : Context; Identity : String) return String is
   begin -- Lock_File
      return Data_File (Settings, Identity) & ".lock";
   end Lock_File;
end Solid.Web.Session.Files;
