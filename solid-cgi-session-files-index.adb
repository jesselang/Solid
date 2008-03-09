with GNAT.Lock_Files;
with Solid.Strings;
use Solid.Strings;

package body Solid.CGI.Session.Files.Index is
   procedure Open (Settings : in out Context; Mode : in Index_IO.File_Mode := Index_IO.In_File);

   procedure Read (Settings : in out Context);

   procedure Close (Settings : in out Context);

   function Exists (Session : Data) return Boolean is
      Settings : Local_Context := Local_Context (Session.Settings);
      Result   : Boolean;

      use type Index_Sets.Cursor;
   begin -- Exists
      if not Valid (Session.Settings.all) then
         raise Invalid_Context;
      end if;

      Open (Settings => Settings.all);
      Read (Settings => Settings.all);
      Close (Settings => Settings.all);
      Result := Settings.Index_Set.Find (Index_Entry'(Identity => Session.Identity, others => <>) ) /= Index_Sets.No_Element;
      Settings.Index_Set.Clear;

      return Result;
   end Exists;

   procedure Add (Session : in Data) is
      Settings  : Local_Context := Local_Context (Session.Settings);
      New_Entry : Index_Entry   := (Identity => Session.Identity, Expires => Session.Expires, In_Use => True);
   begin -- Add
      Open (Settings => Settings.all, Mode => Index_IO.Append_File);
      Index_IO.Write (File => Settings.Index, Item => New_Entry);
      Close (Settings => Settings.all);
   end Add;

   --~ function Session_File_Name (Session : Data) return String is
      --~ Settings  : Local_Context := Local_Context (Session.Settings);
   --~ begin -- Session_File_Name
      --~ if not Valid (Settings.all) then
         --~ raise Invalid_Context;
      --~ end if;

      --~ return +Settings.Path & '/' & Session.Identity;
   --~ end Session_File_Name;

   procedure Truncate (Settings : in out Context);

   procedure Write (Settings : in out Context);

   procedure Delete (Session : in out Data) is
      Settings : Local_Context := Local_Context (Session.Settings);

      use type Index_Sets.Cursor;
   begin -- Delete
      if not Valid (Session.Settings.all) then
         raise Invalid_Context;
      end if;

      Open (Settings => Settings.all);
      Read (Settings => Settings.all);
      -- Handle exception if item not in set?
      Settings.Index_Set.Delete (Item => Index_Entry'(Identity => Session.Identity, others => <>) );
      Truncate (Settings => Settings.all);
      Write (Settings => Settings.all);
      Close (Settings => Settings.all);
   end Delete;

   function In_Use (Session : Data) return Boolean is
   begin -- In_Use
      return True;
   end In_Use;

   function Index_File_Name (Settings : Context) return String;
   pragma Inline (Index_File_Name);

   function Index_Lock_File_Name (Settings : Context) return String;
   pragma Inline (Index_Lock_File_Name);

   function Index_File_Name (Settings : Context) return String is
   begin -- Index_File_Name
      if not Valid (Settings) then
         raise Invalid_Context;
      end if;

      return +Settings.Path & "/index";
   end Index_File_Name;

   function Index_Lock_File_Name (Settings : Context) return String is
   begin -- Index_Lock_File_Name
      return Index_File_Name (Settings) & ".lock";
   end Index_Lock_File_Name;

   procedure Open (Settings : in out Context; Mode : in Index_IO.File_Mode := Index_IO.In_File) is
   begin -- Open
      if not Valid (Settings) then
         raise Invalid_Context;
      end if;

      GNAT.Lock_Files.Lock_File (Lock_File_Name => Index_Lock_File_Name (Settings), Wait => 0.5, Retries => 3);

      declare
         File_Name : constant String := Index_File_Name (Settings);
      begin
         Index_IO.Open (File => Settings.Index, Mode => Mode, Name => File_Name);
      exception
         when Index_IO.Name_Error =>
            Index_IO.Create (File => Settings.Index, Mode => Mode, Name => File_Name);
      end;
   exception -- Open
      when Index_IO.Status_Error | Index_IO.Name_Error | Index_IO.Use_Error =>
         GNAT.Lock_Files.Unlock_File (Lock_File_Name => Index_Lock_File_Name (Settings) );
         raise Invalid_Context with "unable to open/create index.";
      when GNAT.Lock_Files.Lock_Error =>
         raise Invalid_Context with "unable to get lock for index.";
   end Open;

   procedure Close (Settings : in out Context) is
   begin -- Close
      if not Valid (Settings) then
         raise Invalid_Context;
      end if;

      Index_IO.Close (File => Settings.Index);
      GNAT.Lock_Files.Unlock_File (Lock_File_Name => Index_Lock_File_Name (Settings) );
   end Close;

   procedure Read (Settings : in out Context) is
      Single_Entry : Index_Entry;
   begin -- Read
      Read_All_Entries : loop
         exit Read_All_Entries when Index_IO.End_Of_File (Settings.Index);

         Index_IO.Read (File => Settings.Index, Item => Single_Entry);
         Settings.Index_Set.Insert (New_Item => Single_Entry);
      end loop Read_All_Entries;
   end Read;

   procedure Write (Settings : in out Context) is
      procedure Write_Entry (Position : Index_Sets.Cursor);

      procedure Write_Entry (Position : Index_Sets.Cursor) is
      begin -- Write_Entry
         Index_IO.Write (File => Settings.Index, Item => Index_Sets.Element (Position) );
      end Write_Entry;
   begin -- Write
      Settings.Index_Set.Iterate (Process => Write_Entry'Access);
   end Write;

   procedure Truncate (Settings : in out Context) is
   begin -- Truncate
      Index_IO.Delete (File => Settings.Index);
      Open (Settings => Settings, Mode => Index_IO.Out_File);
   end Truncate;
end Solid.CGI.Session.Files.Index;
