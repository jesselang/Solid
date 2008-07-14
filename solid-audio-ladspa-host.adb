with Ada.Containers.Ordered_Maps;
with Ada.Directories;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with Interfaces.C.Strings;
with Solid.Audio.Ladspa.Thin;
with Solid.Interfaces.Libraries;
with Solid.Strings;

with Ada.Text_IO;

package body Solid.Audio.Ladspa.Host is
   -- Plugin indexes:
   -- Library/index -> Plugin descriptor handle
   -- Plugin ID     -> Plugin descriptor handle
   -- Plugin Label  -> Plugin descriptor handle

   type Library_Key is record
      Path  : Strings.U_String;
      Name  : Strings.U_String;
      Index : Plugin_Index;
   end record;

   function "<" (Left : Library_Key; Right : Library_Key) return Boolean;

   use type Thin.LADSPA_Descriptor_Handle;
   package Library_Maps is new Ada.Containers.Ordered_Maps (Key_Type     => Library_Key,
                                                            Element_Type => Thin.LADSPA_Descriptor_Handle);
   package ID_Maps is new Ada.Containers.Ordered_Maps      (Key_Type     => Plugin_ID,
                                                            Element_Type => Thin.LADSPA_Descriptor_Handle);
   package Label_Maps is new Ada.Containers.Ordered_Maps   (Key_Type     => Strings.U_String,
                                                            Element_Type => Thin.LADSPA_Descriptor_Handle,
                                                            "<"          => Ada.Strings.Unbounded."<");
   Library_Map : Library_Maps.Map;
   ID_Map      : ID_Maps.Map;
   Label_Map   : Label_Maps.Map;

   Initialized : Boolean := False;

   type String_Array_Handle is access Strings.String_Array;

   Plugin_Search_Path : String_Array_Handle;

   procedure Initialize (Plugin_Path : in Strings.String_Array := Default_Plugin_Path; Refresh : Boolean := False) is
      generic -- Libraries
         with procedure Process (Library : in String);
      procedure Iterate_Libraries (Search_Path : in Strings.String_Array);
      -- Runs Process with each library found in Search_Path.

      procedure Iterate_Libraries (Search_Path : in Strings.String_Array) is
         Library_Suffix : constant String := ".so";

         Search_Error : exception;

         procedure Search (Path : in String);
         -- Runs Process with each library file found in Path.
         -- Runs recursively on Path.
         -- Raises Search_Error if Path is not a directory.

         procedure Search (Path : in String) is
            Kind             : Ada.Directories.File_Kind := Ada.Directories.Kind (Path);
            Directory_Search : Ada.Directories.Search_Type;
            Directory_Entry  : Ada.Directories.Directory_Entry_Type;
            Recurse_List     : String_Array_Handle;

            procedure Free is new Ada.Unchecked_Deallocation (Strings.String_Array, String_Array_Handle);

            use type Ada.Directories.File_Kind;
            use Solid.Strings;
         begin -- Search
            if Ada.Directories.Kind (Path) /= Ada.Directories.Directory then
               raise Search_Error;
            end if;

            Ada.Directories.Start_Search (Search    => Directory_Search,
                                          Directory => Path,
                                          Pattern   => "",
                                          Filter    => (Ada.Directories.Directory     => True,
                                                        Ada.Directories.Ordinary_File => True,
                                                        Ada.Directories.Special_File  => False) );

            Each_Search_Entry : loop
               exit Each_Search_Entry when not Ada.Directories.More_Entries (Directory_Search);

               Ada.Directories.Get_Next_Entry (Search => Directory_Search, Directory_Entry => Directory_Entry);
               Kind := Ada.Directories.Kind (Directory_Entry);

               declare
                  Name        : constant String := Ada.Directories.Full_Name (Directory_Entry);
                  Simple_Name : constant String := Ada.Directories.Simple_Name (Directory_Entry);
                  Old_List : String_Array_Handle;

                  use type Strings.String_Array;
               begin
                  if Kind = Ada.Directories.Directory then
                     if Simple_Name /= "." and Simple_Name /= ".." then
                        if Recurse_List = null then
                           Recurse_List := new Strings.String_Array'(1 => +Name);
                        else
                           Old_List := Recurse_List;
                           Recurse_List := new Strings.String_Array'(Old_List.all & (+Name) );
                           Free (Old_List);
                        end if;
                     end if;
                  elsif Name (Name'Last - Library_Suffix'Length + 1 .. Name'Last) = Library_Suffix then
                     Process (Library => Name);
                  else
                     null; -- Ordinary file, but not a library.
                  end if;
               end;
            end loop Each_Search_Entry;

            Ada.Directories.End_Search (Search => Directory_Search);

            if Recurse_List /= null then
               for Index in Recurse_List'Range loop
                  Search (Path => +Recurse_List (Index) );
               end loop;

               Free (Recurse_List);
            end if;
         end Search;

         use type Ada.Directories.File_Kind;
         use Solid.Strings;
      begin -- Iterate_Libraries
         for Index in Search_Path'Range loop
            if Ada.Directories.Exists (+Search_Path (Index) ) and then
               Ada.Directories.Kind (+Search_Path (Index) ) = Ada.Directories.Directory
            then
               Search (Path => +Search_Path (Index) );
            end if;
         end loop;
      end Iterate_Libraries;

      procedure Register_Plugins (Library : in String);
      -- Register all the plugins found in Library.

      procedure Register_Libraries is new Iterate_Libraries (Process => Register_Plugins);

      package C renames Standard.Interfaces.C;

      procedure Register_Plugins (Library : in String) is
         Handle            : Interfaces.Libraries.Handle;
         Ladspa_Descriptor : Thin.LADSPA_Descriptor_Function;
         Plugin            : Thin.LADSPA_Descriptor_Handle;

         function Find is new Interfaces.Libraries.Find (Thin.LADSPA_Descriptor_Function);

         use type Interfaces.Libraries.Handle;
      begin -- Register_Plugins
         Ada.Text_IO.Put_Line (Library);

         Handle := Interfaces.Libraries.Load (Library);

         if Handle = Interfaces.Libraries.No_Handle then
            return;
         end if;

         Ladspa_Descriptor := Find (Handle, Name => "ladspa_descriptor");

         All_Plugins : for Index in Plugin_Index'Range loop
            Plugin := Ladspa_Descriptor (Index);

            exit All_Plugins when Plugin = Thin.No_Descriptor;

            declare
               Key   : constant Library_Key := (Path  =>
                                                Name  =>
                                                Index => );
               ID    : constant Plugin_ID := Plugin.UniqueID;
               Label : constant String    := C.Strings.Value (Plugin.Label);
            begin
               ID_Map.Insert (Key => Plugin.UniqueID, New_Item => Plugin);
               Label_Map.Insert (Key => +C.Strings.Value (Plugin.Label), New_Item => Plugin);
            exception
               when Constraint_Error =>
                  null;
                  -- Duplicate plugin ID or label found.
                  --Ada.Text_IO.Put_Line (Plugin_ID'Image (ID) & " - " & Label);
            end;
         end loop All_Plugins;

         -- I've no real concept of where we ought to unload the libraries.
         Solid.Interfaces.Libraries.Unload (Handle);
      exception -- Register_Plugins
         when Interfaces.Libraries.Not_Found =>
            Solid.Interfaces.Libraries.Unload (Handle);
            -- Library is not a LADSPA file.
      end Register_Plugins;

      procedure Free is new Ada.Unchecked_Deallocation (Strings.String_Array, String_Array_Handle);
   begin -- Initialize
      if not Refresh and Initialized then
         return;
      elsif Refresh then
         if Plugin_Search_Path = null then
            raise Host_Error;
         end if;
      else
         if Plugin_Search_Path /= null then
            Free (Plugin_Search_Path);
         end if;

         Plugin_Search_Path := new Strings.String_Array'(Plugin_Path);
      end if;

      Register_Libraries (Search_Path => Plugin_Search_Path.all);

      Initialized := True;
   end Initialize;

   procedure Iterate (Order : in Plugin_Order := Library; Refresh : Boolean := False) is
   begin -- Iterate
      if not Initialized then
         Initialize;
      elsif Refresh then
         Initialize (Refresh => True);
      end if;

      null;
   end Iterate;

   function "<" (Left : Library_Key; Right : Library_Key) return Boolean is
      use type Ada.Strings.Unbounded.Unbounded_String;
   begin -- "<"
      if Left.Name = Right.Name then
         if Left.Path = Right.Path then
            return Left.Index < Right.Index;
         else
            return Left.Path < Right.Path;
         end if;
      else
         return Left.Name < Right.Name;
      end if;
   end "<";
end Solid.Audio.Ladspa.Host;
