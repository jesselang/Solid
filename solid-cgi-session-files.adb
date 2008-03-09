with Ada.Directories;
with Solid.CGI.Session.Files.Index;
with Solid.CGI.Session.Files.Storage;
with Solid.Strings;
use Solid.Strings;

package body Solid.CGI.Session.Files is
   function Initialize (Path : String; Name : String := "Session"; Lifetime : Duration := Duration'Last) return Context_Handle is
      Result : Context_Handle := new Context;
   begin -- Initialize
      Set_Path (Settings => Context (Result.all), To => Path);
      Initialize (Settings => Result, Name => Name, Lifetime => Lifetime);

      return Result;
   end Initialize;

   procedure Set_Path (Settings : in out Context'Class; To : in String) is
   begin -- Set_Path
      Settings.Path := +To;
   end Set_Path;

   procedure Initialize_Implementation (Settings : in out Context) is
   begin -- Initialize_Implementation
      Ada.Directories.Create_Path (New_Directory => +Settings.Path);
   exception -- Initialize_Implementation
      when Ada.Directories.Name_Error | Ada.Directories.Use_Error =>
         raise Invalid_Context with "could not create path.";
   end Initialize_Implementation;

   function Path (Settings : Context'Class) return String is
   begin -- Path
      return +Settings.Path;
   end Path;

   function New_Session (Settings : Context) return Session.Data'Class is
      Result : Session.Data'Class := Data'(Session.Data with null record);
   begin -- New_Session
      return Result;
   end New_Session;

   procedure Create_Implementation (Session : in out Data) is
      Settings : Local_Context := Local_Context (Session.Settings);
   begin -- Create_Implementation
      if not Valid (Session.Settings.all) then
         raise Invalid_Context;
      end if;

      Unique_Identity : loop
         exit Unique_Identity when not Index.Exists (Session);

         Session.New_Identity;
      end loop Unique_Identity;

      Index.Add (Session => Session);
   end Create_Implementation;

   procedure Delete_Implementation (Session : in out Data) is
      Settings : Local_Context := Local_Context (Session.Settings);
   begin -- Delete_Implementation
      if not Valid (Session.Settings.all) then
         raise Invalid_Context;
      end if;

      if not Index.Exists (Session) then
         raise Not_Found;
      end if;

      Index.Delete (Session => Session);
      Storage.Delete (Session => Session);
   end Delete_Implementation;

   procedure Read_Implementation (From : in out Context; Identity : in String; To : out Session.Data'Class) is
   begin -- Read_Implementation
      if not Valid (From) then
         raise Invalid_Context;
      end if;

      --~ if not Exists_In_Index (Session) then
         --~ raise Not_Found;
      --~ end if;


      -- If session not in index then raise Not_Found.
      -- If session is in use, raise In_Use.
      -- If data file isn't readable, then delete from index, and raise Not_Found.
      -- Read data file.
   end Read_Implementation;

   procedure Write_Implementation (Session : in out Data) is
   begin -- Write_Implementation
      if not Valid (Session.Settings.all) then
         raise Invalid_Context;
      end if;

      if not Index.Exists (Session) then
         raise Not_Found;
      end if;

      Storage.Write (Session);

      -- If session is not in the index, raise Not_Found.
      -- Check in use?
      -- Write data file.
      --~ Release_Lock (Session => Session);
   end Write_Implementation;


   function "<" (Left : Index_Entry; Right : Index_Entry) return Boolean is
   begin -- "<"
      return Left.Identity < Right.Identity;
   end "<";

   function "=" (Left : Index_Entry; Right : Index_Entry) return Boolean is
   begin -- "="
      return Left.Identity = Right.Identity;
   end "=";
end Solid.CGI.Session.Files;
