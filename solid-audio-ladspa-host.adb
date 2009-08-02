with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Directories;
with Ada.Strings.Unbounded.Hash;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Interfaces.C.Strings;
with PragmARC.Images;
with Solid.Audio.Ladspa.Thin;
-- with Solid.Interfaces.Bitwise_Enumerations;
with Solid.Interfaces.Libraries;
with Solid.Strings;
with System;

with Ada.Text_IO;

package body Solid.Audio.Ladspa.Host is

   ------------------------
   -- Library Management --
   ------------------------

   type Library_Key is record
      Path  : Strings.U_String;
      Name  : Strings.U_String;
      Index : Plugin_Index;
   end record;
   -- Library_Key represents a specific plugin found at Index with a specific library file found at Path and Name.
   -- Plugins are loaded using this key, which is usually found by a map lookup on a unique identifier, such as
   -- the plugin's ID or label.

   type Library_Info is record
      Handle     : Solid.Interfaces.Libraries.Handle;
      Descriptor : Thin.LADSPA_Descriptor_Function;
   end record;
   -- Library_Info contains a LADSPA library file's resources.

   function Hash (Key : Library_Key) return Ada.Containers.Hash_Type;
   function Equivalent_Keys (Left : Library_Key; Right : Library_Key) return Boolean;
   use type Solid.Interfaces.Libraries.Handle;
   package Library_Maps is new Ada.Containers.Hashed_Maps (Key_Type        => Library_Key,
                                                           Element_Type    => Library_Info,
                                                           Hash            => Hash,
                                                           Equivalent_Keys => Equivalent_Keys);

   Library_Not_Found  : exception;
   Library_Not_LADSPA : exception;

   protected Library_Manager is
      procedure Open (Library : in Library_Key; Descriptor : out Thin.LADSPA_Descriptor_Handle);
      -- Sets Descriptor for the plugin identified by Library.  Opens the library file if needed.
      -- Raises Library_Not_Found if Library could not be found.
      -- Raises Library_Not_LADSPA if Library does not appear to be a LADSPA library.
      -- Descriptor will be Thin.No_Descriptor if no plugin descriptor is found at Library.Index.

      procedure Close (Library : in Library_Key);
      -- Closes the resources associated with Library.
   private -- Library_Manager
      Open_Libraries : Library_Maps.Map;
   end Library_Manager;

   -- Three plugin indices are kept.  The first two identify plugins using unique IDs and labels.
   -- The third index contains information about the plugins.
   -- Key              Element
   -- Plugin ID     -> Library_Key
   -- Plugin Label  -> Library_Key
   -- Library_Key   -> Plugin_Information

   function "<" (Left : Library_Key; Right : Library_Key) return Boolean;
   package ID_Maps is new Ada.Containers.Ordered_Maps      (Key_Type     => Plugin_ID,
                                                            Element_Type => Library_Key);
   package Label_Maps is new Ada.Containers.Ordered_Maps   (Key_Type     => Strings.U_String,
                                                            Element_Type => Library_Key,
                                                            "<"          => Ada.Strings.Unbounded."<");

   type Plugin_Details is record
      ID        : Plugin_ID;
      Label     : Solid.Strings.U_String;
      Name      : Solid.Strings.U_String;
      Maker     : Solid.Strings.U_String;
      Copyright : Solid.Strings.U_String;
   end record;

   package Plugin_Maps is new Ada.Containers.Ordered_Maps (Key_Type        => Library_Key,
                                                           Element_Type    => Plugin_Details);

   -- The plugin indices, as mentioned above.
   Plugin_IDs    : ID_Maps.Map;
   Plugin_Labels : Label_Maps.Map;
   Plugin_Info   : Plugin_Maps.Map;

   Initialized : Boolean := False; -- Stores whether the package was initialized.

   package String_List is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                      Element_Type => Strings.U_String,
                                                      "="          => Strings."=");

   Warning_List : String_List.Vector; -- If requested, Warnings are appended to this vector during initialization.

   package C renames Standard.Interfaces.C;

   procedure Initialize (Warnings : in out Boolean; Plugin_Path : in Strings.String_Array := Default_Plugin_Path) is
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

            type String_Array_Handle is access Strings.String_Array;

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
                  Name        : constant String := Ada.Directories.Full_Name   (Directory_Entry);
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
                     if Warnings then
                        Warning_List.Append (New_Item => +("Warning: " & Name & " does not appear to be a library.") );
                     end if;
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

      procedure Register_Plugins (Library : in String) is
         Key : Library_Key := (Path  => +Ada.Directories.Containing_Directory (Library),
                               Name  => +Ada.Directories.Simple_Name (Library),
                               Index => Plugin_Index'First);
         Plugin : Thin.LADSPA_Descriptor_Handle;

         use type Thin.LADSPA_Descriptor_Handle;
      begin -- Register_Plugins
         All_Plugins : for Index in Plugin_Index'Range loop
            Key.Index := Index;
            Library_Manager.Open (Library => Key, Descriptor => Plugin);

            exit All_Plugins when Plugin = Thin.No_Descriptor;

            Plugin_ID : declare
               function Image is new PragmARC.Images.Modular_Image (Ladspa.Plugin_ID);
            begin -- Plugin_ID
               Plugin_IDs.Insert (Key => Plugin.UniqueID, New_Item => Key);
            exception -- Plugin_ID
               when Constraint_Error =>
                  if Warnings then
                     Warning_List.Append (New_Item => +("Warning: duplicate plugin ID " & Image (Plugin.UniqueID) ) );
                  end if;
            end Plugin_ID;

            Plugin_Label : declare
               Label : constant String := C.Strings.Value (Plugin.Label);
            begin -- Plugin_Label
               Plugin_Labels.Insert (Key => +Label, New_Item => Key);

               Plugin_Information : declare
                  Name      : constant String := C.Strings.Value (Plugin.Name);
                  Maker     : constant String := C.Strings.Value (Plugin.Maker);
                  Copyright : constant String := C.Strings.Value (Plugin.Copyright);
               begin -- Plugin_Information
                  Plugin_Info.Insert (Key => Key, New_Item => (ID        => Plugin.UniqueID,
                                                               Label     => +Label,
                                                               Name      => +Name,
                                                               Maker     => +Maker,
                                                               Copyright => +Copyright) );
               end Plugin_Information;
            exception -- Plugin_Label
               when Constraint_Error =>
                  if Warnings then
                     Warning_List.Append (New_Item => +("Warning: duplicate plugin label " & Label) );
                  end if;
            end Plugin_Label;
         end loop All_Plugins;

         Library_Manager.Close (Library => Key);
      exception -- Register_Plugins
         when Library_Not_Found =>
            if Warnings then
               Warning_List.Append (New_Item => +("Warning: " & Library & " could not be found.") );
            end if;
         when Library_Not_LADSPA =>
            if Warnings then
               Warning_List.Append (New_Item => +("Warning: " & Library & " does not appear to be a LADSPA library file.") );
            end if;
      end Register_Plugins;

      use type Ada.Containers.Count_Type;
   begin -- Initialize
      Plugin_IDs.Clear;
      Plugin_Labels.Clear;
      Plugin_Info.Clear;

      Warning_List.Clear;

      Register_Libraries (Search_Path => Plugin_Path);

      Warnings := Warnings and then Warning_List.Length > 0;

      Initialized := True;
   end Initialize;

   procedure Warnings is
      function "+" (Left : Strings.U_String) return String renames Strings."+";
   begin -- Warnings
      for Index in String_List.First_Index (Warning_List) .. String_List.Last_Index (Warning_List) loop
         Process (Message => +Warning_List.Element (Index) );
      end loop;
   end Warnings;

   procedure Available_Plugins (Order : in Plugin_Order := Library) is
      procedure Plugin_Information (Item : in Plugin_Details);
      -- Calls Process with information from Item.

      procedure Plugin_Information (Position : in Plugin_Maps.Cursor);
      -- Calls Plugin_Information with the item in Position.

      procedure Plugin_Information (Position : in ID_Maps.Cursor);
      -- Calls Plugin_Information with the item in Position.

      procedure Plugin_Information (Position : in Label_Maps.Cursor);
      -- Calls Plugin_Information with the item in Position.

      Continue : Boolean := True;

      procedure Plugin_Information (Item : in Plugin_Details) is
         use Solid.Strings;
      begin -- Plugin_Information
         Process (ID        => Item.ID,
                  Label     => +Item.Label,
                  Name      => +Item.Name,
                  Maker     => +Item.Maker,
                  Copyright => +Item.Copyright,
                  Continue  => Continue);
      end Plugin_Information;

      procedure Plugin_Information (Position : in Plugin_Maps.Cursor) is
      begin -- Plugin_Information
         if Continue then
            Plugin_Information (Item => Plugin_Maps.Element (Position) );
         end if;
      end Plugin_Information;

      procedure Plugin_Information (Position : in ID_Maps.Cursor) is
         Info_Position : Plugin_Maps.Cursor;

         use type Plugin_Maps.Cursor;
      begin -- Plugin_Information
         if Continue then
            Info_Position := Plugin_Info.Find (ID_Maps.Element (Position) );

            if Info_Position /= Plugin_Maps.No_Element then
               Plugin_Information (Item => Plugin_Maps.Element (Info_Position) );
            end if;
         end if;
      end Plugin_Information;

      procedure Plugin_Information (Position : in Label_Maps.Cursor) is
         Info_Position : Plugin_Maps.Cursor;

         use type Plugin_Maps.Cursor;
      begin -- Plugin_Information
         if Continue then
            Info_Position := Plugin_Info.Find (Label_Maps.Element (Position) );

            if Info_Position /= Plugin_Maps.No_Element then
               Plugin_Information (Item => Plugin_Maps.Element (Info_Position) );
            end if;
         end if;
      end Plugin_Information;
   begin -- Available_Plugins
      if not Initialized then
         raise Not_Initialized;
      end if;

      case Order is
         when Library =>
            Plugin_Info.Iterate (Process => Plugin_Information'Access);
         when ID =>
            Plugin_IDs.Iterate (Process => Plugin_Information'Access);
         when Label =>
            Plugin_Labels.Iterate (Process => Plugin_Information'Access);
      end case;
   end Available_Plugins;

   procedure Instantiate (P : in out Plugin; Rate : in Sample_Rate);

   procedure Create_Ports (P : in out Plugin);

   procedure Create (P : in out Plugin; Rate : in Sample_Rate; ID : in Plugin_ID) is
      Position : ID_Maps.Cursor;

      use type ID_Maps.Cursor;
      use type Thin.LADSPA_Descriptor_Handle;
   begin -- Create
      if not Initialized then
         raise Not_Initialized;
      end if;

      if P.State in Instantiated .. Deactivated then
         raise Invalid_State;
      end if;

      Position := Plugin_IDs.Find (ID);

      if Position = ID_Maps.No_Element then
         raise Plugin_Not_Found;
      end if;

      Library_Manager.Open (Library => ID_Maps.Element (Position), Descriptor => P.Descriptor);

      if P.Descriptor = Thin.No_Descriptor then
         raise Plugin_Not_Found;
      end if;

      Instantiate (P, Rate => Rate);
      P.Rate  := Rate;
      P.State := Instantiated;
      Create_Ports (P);
      P.State := Connected;
   exception -- Create
      when Library_Not_Found | Library_Not_LADSPA =>
         raise Plugin_Not_Found;
   end Create;

   procedure Create (P : in out Plugin; Rate : in Sample_Rate; Label : in String) is
      Position : Label_Maps.Cursor;

      use type Label_Maps.Cursor;
      use type Thin.LADSPA_Descriptor_Handle;
   begin -- Create
      if not Initialized then
         raise Not_Initialized;
      end if;

      if P.State in Instantiated .. Deactivated then
         raise Invalid_State;
      end if;

      Position := Plugin_Labels.Find (+Label);

      if Position = Label_Maps.No_Element then
         raise Plugin_Not_Found;
      end if;

      Library_Manager.Open (Library => Label_Maps.Element (Position), Descriptor => P.Descriptor);

      if P.Descriptor = Thin.No_Descriptor then
         raise Plugin_Not_Found;
      end if;

      Instantiate (P, Rate => Rate);
      P.Rate  := Rate;
      P.State := Instantiated;
      Create_Ports (P);
      P.State := Connected;
   exception -- Create
      when Library_Not_Found | Library_Not_LADSPA =>
         raise Plugin_Not_Found;
   end Create;

   function Have_Property (P : Plugin; Property : Plugin_Property) return Boolean is
   begin -- Have_Property
      if not Initialized then
         raise Not_Initialized;
      end if;

      -- Check state.

      -- Do something useful.

      return False;
   end Have_Property;

   procedure Activate (P : in out Plugin) is
      use type Thin.Instance_Procedure;
   begin -- Activate
      if not Initialized then
         raise Not_Initialized;
      end if;

      -- Check state.

      if P.Descriptor.Activate /= null then
         P.Descriptor.Activate (Instance => P.Instance);
      end if;

      P.State := Activated;
   end Activate;

   procedure Deactivate (P : in out Plugin) is
      use type Thin.Instance_Procedure;
   begin -- Deactivate
      if not Initialized then
         raise Not_Initialized;
      end if;

      -- Check state.

      if P.Descriptor.Deactivate /= null then
         P.Descriptor.Deactivate (Instance => P.Instance);
      end if;

      P.State := Deactivated;
   end Deactivate;

   procedure Run (P : in out Plugin; Sample_Count : in Buffer_Size) is
      Count : constant C.unsigned_long := C.unsigned_long (Sample_Count);
   begin -- Run
      if not Initialized then
         raise Not_Initialized;
      end if;

      -- Check state.

      P.Descriptor.Run (Instance => P.Instance, SampleCount => Count);
      P.State := Running;
   end Run;

   procedure Cleanup (P : in out Plugin) is
   begin -- Cleanup
      if not Initialized then
         raise Not_Initialized;
      end if;

      -- Deactivate if needed.

      -- Cleanup plugin.

      -- Deallocate the ports.
      P.State := Finalized;
   end Cleanup;

   function Name (Port : Plugin_Port) return String is
      function "+" (Left : Strings.U_String) return String renames Strings."+";
   begin -- Name
      if not Initialized then
         raise Not_Initialized;
      end if;

      return +Port.Name;
   end Name;

   function Direction (Port : Plugin_Port) return Port_Direction is
   begin -- Direction
      if not Initialized then
         raise Not_Initialized;
      end if;

      return Port.Direction;
   end Direction;

   function Ports (P : Plugin) return Port_Array is
   begin -- Ports
      if not Initialized then
         raise Not_Initialized;
      end if;

      if P.State not in Connected .. Deactivated then
         return (1 .. 0 => <>);
      end if;

      return P.Ports.all;
   end Ports;

   procedure Connect (P : in out Plugin; Port : in out Audio_Port; Buffer : in out Sample_Buffer) is
      use type Thin.Connect_Port_Procedure;
      use Solid.Strings;
   begin -- Connect
      if not Initialized then
         raise Not_Initialized;
      end if;

      if P.Descriptor.Connect_Port = null then
         raise Program_Error;
      end if;

      if P.Instance = null then
         raise Program_Error;
      end if;

      P.Descriptor.Connect_Port (Instance => P.Instance, Port => Port.Index, DataLocation => Buffer (Buffer'First) );
      Port.Connected := True;
   end Connect;

   function Take_Hint (Control : Control_Port; Hint : Port_Hint) return Boolean is
   begin -- Take_Hint
      if not Initialized then
         raise Not_Initialized;
      end if;

      return Control.Hints (Hint);
   end Take_Hint;

   procedure Set_Default (Control : in out Control_Port) is
   begin -- Set_Default
      if not Initialized then
         raise Not_Initialized;
      end if;

      -- Check bounds?

      Control.Value := Control.Default;
   end Set_Default;

   function Get (Control : Control_Port) return Control_Value is
   begin -- Get
      if not Initialized then
         raise Not_Initialized;
      end if;

      return Control.Value;
   end Get;

   procedure Set (Control : in out Normal_Control; Value : in Control_Value) is
   begin -- Set
      if not Initialized then
         raise Not_Initialized;
      end if;

      if Value not in Control.Lower .. Control.Upper then
         raise Range_Error;
      end if;

      Control.Value := Value;
   end Set;

   function Enabled (Control : Toggle_Control) return Boolean is
   begin -- Enabled
      if not Initialized then
         raise Not_Initialized;
      end if;

      return Control.Value > 0.0;
   end Enabled;

   procedure Set (Control : in out Toggle_Control; Enable : in Boolean) is
   begin -- Set
      if not Initialized then
         raise Not_Initialized;
      end if;

      if Enable then
         Control.Value := 1.0;
      else
         Control.Value := 0.0;
      end if;
   end Set;

   function Get (Control : Sample_Rate_Control) return Audio.Sample_Rate is
   begin -- Get
      if not Initialized then
         raise Not_Initialized;
      end if;

      return Audio.Sample_Rate (Control.Value);
   end Get;

   procedure Set (Control : in out Sample_Rate_Control; Value : in Audio.Sample_Rate) is
      Control_Rate : constant Control_Value := Control_Value (Control.Rate);
      New_Value    : constant Control_Value := Control_Value (Value);
   begin -- Set
      if not Initialized then
         raise Not_Initialized;
      end if;

      if New_Value not in Control.Lower * Control_Rate .. Control.Upper * Control_Rate then
         raise Range_Error;
      end if;

      Control.Value := New_Value;
   end Set;

   function Get (Control : Integer_Control) return Integer is
   begin -- Get
      if not Initialized then
         raise Not_Initialized;
      end if;

      return Integer (Control.Value);
   end Get;

   procedure Set (Control : in out Integer_Control; Value : in Integer) is
      New_Value : constant Control_Value := Control_Value (Value);
   begin -- Set
      if not Initialized then
         raise Not_Initialized;
      end if;

      if New_Value not in Control.Lower .. Control.Upper then
         raise Range_Error;
      end if;

      Control.Value := New_Value;
   end Set;

   protected body Library_Manager is
      function Library_Path (Library : Library_Key) return String;

      procedure Open (Library : in Library_Key; Descriptor : out Thin.LADSPA_Descriptor_Handle) is
         Path : constant String := Library_Path (Library);

         Position : Library_Maps.Cursor;
         Info     : Library_Info;

         function Find is new Interfaces.Libraries.Find (Thin.LADSPA_Descriptor_Function);

         use type Library_Maps.Cursor;
         use type Interfaces.Libraries.Handle;
         use type Thin.LADSPA_Descriptor_Handle;
      begin -- Open
         Position := Open_Libraries.Find (Library);

         if Position = Library_Maps.No_Element then
            Info.Handle := Interfaces.Libraries.Load (Path);

            if Info.Handle = Interfaces.Libraries.No_Handle then
               raise Library_Not_Found;
            end if;

            Info.Descriptor := Find (Info.Handle, Name => "ladspa_descriptor");

            Open_Libraries.Insert (Key => Library, New_Item => Info);
         else
            Info := Library_Maps.Element (Position);
         end if;

         Descriptor := Info.Descriptor (Library.Index);
      exception -- Open
         when Interfaces.Libraries.Not_Found =>
            Solid.Interfaces.Libraries.Unload (Info.Handle);

            raise Library_Not_LADSPA;
      end Open;

      procedure Close (Library : in Library_Key) is
         Position : Library_Maps.Cursor := Open_Libraries.Find (Library);
         Info     : Library_Info;

         use type Library_Maps.Cursor;
      begin -- Close
         if Position /= Library_Maps.No_Element then
            Info := Library_Maps.Element (Position);

            Solid.Interfaces.Libraries.Unload (Info.Handle);

            Open_Libraries.Delete (Position => Position);
         end if;
      end Close;

      function Library_Path (Library : Library_Key) return String is
         use Solid.Strings;
      begin -- Library_Path
         return +Library.Path & '/' & (+Library.Name);
      end Library_Path;
   end Library_Manager;

   function Hash (Key : Library_Key) return Ada.Containers.Hash_Type is
      use type Ada.Strings.Unbounded.Unbounded_String;
   begin -- Hash
      return Ada.Strings.Unbounded.Hash (Key.Path & Key.Name);
   end Hash;

   function Equivalent_Keys (Left : Library_Key; Right : Library_Key) return Boolean is
      use type Ada.Strings.Unbounded.Unbounded_String;
   begin -- Equivalent_Keys
      return Left.Path = Right.Path and then Left.Name = Right.Name;
   end Equivalent_Keys;

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

   procedure Instantiate (P : in out Plugin; Rate : in Sample_Rate) is
      use type Thin.LADSPA_Descriptor_Handle;
   begin -- Instantiate
      -- Check state?

      if P.Descriptor = Thin.No_Descriptor then
         null; -- Raise something.
      end if;

      if P.Instance /= null then
         null; -- Raise something.
      end if;

      P.Instance := P.Descriptor.instantiate (P.Descriptor, SampleRate => C.unsigned_long (Rate) );

      if P.Instance = null then
         raise Program_Error;
      end if;
   end Instantiate;

   procedure Create_Ports (P : in out Plugin) is
      function Direction (D : Thin.LADSPA_PortDescriptor) return Port_Direction;
      -- Returns the direction of the port described by D.
      -- Raises Program_Error if an error occurs.

      function Direction (D : Thin.LADSPA_PortDescriptor) return Port_Direction is
         use type Thin.LADSPA_PortDescriptor;
      begin -- Direction
         case D and (Thin.Input or Thin.Output) is
            when Thin.Input =>
               return Input;
            when Thin.Output =>
               return Output;
            when others =>
               raise Program_Error with "Port direction wasn't described.";
         end case;
      end Direction;

      Port_Count   : constant C.ptrdiff_t := C.ptrdiff_t (P.Descriptor.PortCount);
      Descriptors  : constant Thin.PortDescriptor_List := Thin.PortDescriptor_Lists.Value
                                                                           (P.Descriptor.PortDescriptors, Length => Port_Count);
      Names        : constant Thin.PortName_List       := Thin.PortName_Lists.Value (P.Descriptor.PortNames, Length => Port_Count);
      Port_Hints   : constant Thin.PortRangeHint_List  := Thin.PortRangeHint_Lists.Value
                                                                           (P.Descriptor.PortRangeHints, Length => Port_Count);

      Port_Index : Thin.Port_Index;

      type Control_Handle is access all Control_Port'Class;

      Control : Control_Handle;

      use type Thin.LADSPA_PortDescriptor;
      use type Thin.LADSPA_PortRangeHintDescriptor;
      use type Thin.Connect_Port_Procedure;
   begin -- Create_Ports
      -- Check state?

      if P.Ports /= null then
         null; -- Raise something.
      end if;

      P.Ports := new Port_Array (1 .. Natural (P.Descriptor.PortCount) );

      All_Ports : for Index in P.Ports'Range loop
         Port_Index := Thin.Port_Index (Index - 1);

         if (Descriptors (Port_Index) and Thin.Audio) > 0 then
            P.Ports (Index) := new Audio_Port;
         elsif (Descriptors (Port_Index) and Thin.Control) > 0 then
            if (Port_Hints (Port_Index).HintDescriptor and Thin.Toggled) > 0 then
               P.Ports (Index) := new Toggle_Control;
            elsif (Port_Hints (Port_Index).HintDescriptor and Thin.Sample_Rate) > 0 then
               P.Ports (Index) := new Sample_Rate_Control'(Rate => P.Rate, others => <>); -- Rate required for accurate bounding.
            elsif (Port_Hints (Port_Index).HintDescriptor and Thin.Integer) > 0 then
               P.Ports (Index) := new Integer_Control;
            else
               P.Ports (Index) := new Normal_Control;
            end if;

            -- Create a control port view of the port.
            Control := Control_Handle (P.Ports (Index) );

            if P.Descriptor.Connect_Port = null then
               raise Program_Error;
            end if;

            -- Connect control port.
            P.Descriptor.Connect_Port (Instance => P.Instance, Port => Port_Index, DataLocation => Control.Value);

            -- Set the remaining port hints.
            if (Port_Hints (Port_Index).HintDescriptor and Thin.Logarithmic) > 0 then
               Control.Hints (Logarithmic) := True;
            end if;

            -- Set bounds.
            if (Port_Hints (Port_Index).HintDescriptor and Thin.Bounded_Below) > 0 then
               Control.Lower := Port_Hints (Port_Index).LowerBound;
            end if;

            if (Port_Hints (Port_Index).HintDescriptor and Thin.Bounded_Above) > 0 then
               Control.Upper := Port_Hints (Port_Index).UpperBound;
            end if;

            -- Set a default value.
            case (Port_Hints (Port_Index).HintDescriptor and Thin.Default_Mask) is
               when Thin.Default_None =>
                  Control.Default := 0.0;
               when Thin.Default_Minimum =>
                  Control.Default := Control.Lower;
               when Thin.Default_Low =>
                  -- For ports with LADSPA_HINT_LOGARITHMIC, this should be exp(log(lower) * 0.75 + log(upper) * 0.25).
                  -- Otherwise, this should be (lower * 0.75 + upper * 0.25). */

                  -- TODO --
                  null;
               when Thin.Default_Middle =>
                  -- For ports with LADSPA_HINT_LOGARITHMIC, this should be exp(log(lower) * 0.5 + log(upper) * 0.5).
                  -- Otherwise, this should be (lower * 0.5 + upper * 0.5).

                  -- TODO --
                  null;
               when Thin.Default_High =>
                  -- For ports with LADSPA_HINT_LOGARITHMIC, this should be exp(log(lower) * 0.25 + log(upper) * 0.75).
                  -- Otherwise, this should be (lower * 0.25 + upper * 0.75).

                  -- TODO --
                  null;
               when Thin.Default_Maximum =>
                  Control.Default := Control.Upper;
               when Thin.Default_0 =>
                  Control.Default := 0.0;
               when Thin.Default_1 =>
                  Control.Default := 1.0;
               when Thin.Default_100 =>
                  Control.Default := 100.0;
               when Thin.Default_440 =>
                  Control.Default := 440.0;
               when others =>
                  raise Program_Error with "Port default was not described."; -- This should never happen.
            end case;

            Set_Default (Control.all);
         else
            raise Program_Error with "Port type was not described."; -- This should never happen.
         end if;

         P.Ports (Index).Index := Port_Index;
         P.Ports (Index).Direction := Direction (Descriptors (Port_Index) );
         P.Ports (Index).Name := +C.Strings.Value (Names (Port_Index) );
      end loop All_Ports;
   end Create_Ports;
end Solid.Audio.Ladspa.Host;
