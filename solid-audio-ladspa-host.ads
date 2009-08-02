private with Solid.Audio.Ladspa.Thin;
with Solid.Strings;


package Solid.Audio.Ladspa.Host is
   Default_Plugin_Path : constant Strings.String_Array;

   procedure Initialize (Warnings : in out Boolean; Plugin_Path : in Strings.String_Array := Default_Plugin_Path);
   -- Initializes the LADSPA host, enumerating plugins found in Plugin_Path.
   -- If Warnings is True when Initialize is called, any warnings will be stored and made available by using the
   -- Warnings function below.  If any warnings are encountered, Warnings will be set True.

   generic -- Warnings
      with procedure Process (Message : in String);
   procedure Warnings;
   -- Calls Process with each warning message encountered during initialization.

   Not_Initialized : exception;

   -- The following operations raise Not_Initialized if the package has not been initialized.

   type Plugin_Order is (Library, -- Libraries in alphabetical order, plugins ordered within the library.
                         ID,      -- Ordered by plugin unique ID.
                         Label);  -- Ordered by plugin unique label.

   generic -- Iterate
      with procedure Process (ID        : in     Plugin_ID;
                              Label     : in     String;
                              Name      : in     String;
                              Maker     : in     String;
                              Copyright : in     String;
                              Continue  : in out Boolean);
   procedure Available_Plugins (Order : in Plugin_Order := Library);
   -- Calls Process with information about each registered plugin.

   type Plugin is limited private;
   -- A plugin instance.

   Plugin_Not_Found : exception;
   Invalid_State    : exception;

   procedure Create (P : in out Plugin; Rate : in Sample_Rate; ID    : in Plugin_ID);
   procedure Create (P : in out Plugin; Rate : in Sample_Rate; Label : in String);
   -- Create an instance of a plugin using the ID or Label.
   -- Raises Plugin_Not_Found if the plugin could not be instantiated.
   -- Raises Invalid_State if the plugin was already created and has not been finalized.

   type Plugin_Property is (Realtime, Inplace_Broken, Hard_RT_Capable);

   function Have_Property (P : Plugin; Property : Plugin_Property) return Boolean;

   procedure Activate (P : in out Plugin);
   -- Activate the plugin.

   procedure Deactivate (P : in out Plugin);
   -- Deactivate the plugin.

   procedure Run (P : in out Plugin; Sample_Count : in Buffer_Size);
   -- Process Sample_Count samples with the plugin.

   procedure Cleanup (P : in out Plugin);
   -- Finalizes the plugin.

   -----------
   -- Ports --
   -----------

   type Plugin_Port is abstract tagged limited private;

   function Name (Port : Plugin_Port) return String;

   type Port_Direction is (Input, Output);

   function Direction (Port : Plugin_Port) return Port_Direction;


   type Port_Handle is access Plugin_Port'Class;

   type Port_Array is array (Positive range <>) of Port_Handle;


   function Ports (P : Plugin) return Port_Array;


   type Audio_Port is new Plugin_Port with private;

   procedure Connect (P : in out Plugin; Port : in out Audio_Port; Buffer : in out Sample_Buffer);

   type Control_Port is abstract new Plugin_Port with private;

   type Port_Hint is (Logarithmic);

   function Take_Hint (Control : Control_Port; Hint : Port_Hint) return Boolean;

   Range_Error : exception;

   procedure Set_Default (Control : in out Control_Port);

   function Get (Control : Control_Port) return Control_Value;

   type Normal_Control is new Control_Port with private;

   procedure Set (Control : in out Normal_Control; Value : in Control_Value);


   type Toggle_Control is new Control_Port with private;

   function Enabled (Control : Toggle_Control) return Boolean;

   procedure Set (Control : in out Toggle_Control; Enable : in Boolean);


   type Sample_Rate_Control is new Control_Port with private;

   function Get (Control : Sample_Rate_Control) return Audio.Sample_Rate;

   procedure Set (Control : in out Sample_Rate_Control; Value : in Audio.Sample_Rate);

   type Integer_Control is new Control_Port with private;

   function Get (Control : Integer_Control) return Integer;

   procedure Set (Control : in out Integer_Control; Value : in Integer);







   --~ function Low_Bound (Control : Plugin_Control) return Control_Value;

   --~ function High_Bound (Control : Plugin_Control) return Control_Value;

   --procedure Process_Audio (P : in out Plugin; Sample_Count : in Buffer_Size);

private -- Solid.Audio.Ladspa.Host
   function "+" (Left : String) return Strings.U_String renames Strings."+";

   Default_Plugin_Path : constant Strings.String_Array := (+"/usr/lib/ladspa",
                                                           +"/usr/local/lib/ladspa");

   type Plugin_Port is abstract tagged limited record
      Index     : Thin.Port_Index;
      Direction : Port_Direction;
      Name      : Strings.U_String;
   end record;

   type Port_Array_Handle is access Port_Array;

   type Plugin_State is (Uninitialized, Instantiated, Connected, Activated, Running, Deactivated, Finalized);

   type Plugin is limited record
      Instance   : Plugin_Handle;
      Descriptor : Thin.LADSPA_Descriptor_Handle;
      Ports      : Port_Array_Handle;
      State      : Plugin_State := Uninitialized;
      Rate       : Audio.Sample_Rate;
   end record;

   type Audio_Port is new Plugin_Port with record
      Connected : Boolean := False;
   end record;

   type Hint_List is array (Port_Hint) of Boolean;

   type Control_Port is abstract new Plugin_Port with record
      Value   : aliased Control_Value := 0.0;
      Lower   : Control_Value         := Control_Value'First; -- Bound.
      Upper   : Control_Value         := Control_Value'Last;  -- Bound.
      Default : Control_Value         := 0.0;
      Hints   : Hint_List             := (others => False);
   end record;

   type Normal_Control is new Control_Port with null record;

   type Toggle_Control is new Control_Port with null record;

   type Sample_Rate_Control is new Control_Port with record
      Rate : Audio.Sample_Rate;
   end record;

   type Integer_Control is new Control_Port with null record;

end Solid.Audio.Ladspa.Host;
