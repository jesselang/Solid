private with Solid.Audio.Ladspa.Thin;
with Solid.Strings;


package Solid.Audio.Ladspa.Host is
   Default_Plugin_Path : constant Strings.String_Array;

   Host_Error : exception;

   procedure Initialize (Warnings : in out Boolean; Plugin_Path : in Strings.String_Array := Default_Plugin_Path);
   -- Initializes the LADSPA host, enumerating plugins found in Plugin_Path.
   -- If Warnings is True when Initialize is called, any warnings will be stored and made available by using the
   -- Warnings function below.  If any warnings are stored, Warnings will be set True.
   -- Raises Host_Error if

   function Warnings return Strings.String_Array;
   -- Returns any warnings stored during initialization.

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

   type Plugin is limited private;

   Plugin_Not_Found : exception;

   procedure Create (P : in out Plugin; Rate : in Sample_Rate; ID    : in Plugin_ID);
   procedure Create (P : in out Plugin; Rate : in Sample_Rate; Label : in String);
   -- Create an instance of a plugin using the ID or Label.
   -- Raises Plugin_Not_Found if the plugin could not be instantiated.

   procedure Activate (P : in out Plugin);
   procedure Deactivate (P : in out Plugin);

   procedure Run (P : in out Plugin; Samples : in Buffer_Size);

   type Plugin_Port is abstract tagged limited private;

   function Name (Port : Plugin_Port) return String;

   type Port_Direction is (Input, Output);

   function Direction (Port : Plugin_Port) return Port_Direction;


   type Port_Handle is access Plugin_Port'Class;

   type Port_Array is array (Positive range <>) of Port_Handle;


   function Ports (P : Plugin) return Port_Array;


   type Audio_Port is new Plugin_Port with private;

   procedure Connect (Port : in out Audio_Port; Buffer : in Buffer_Handle);



   type Control_Port is abstract new Plugin_Port with private;

   procedure Set_Default (Control : in out Control_Port);

   type Normal_Control is new Control_Port with private;

   function Get (Control : Normal_Control) return Control_Value;

   procedure Set (Control : in out Normal_Control; Value : in Control_Value);


   type Toggle_Control is new Control_Port with private;

   function Enabled (Control : Toggle_Control) return Boolean;

   procedure Set (Control : in out Toggle_Control; Enable : in Boolean);


   type Sample_Rate_Control is new Control_Port with private;

   function Get (Control : Sample_Rate_Control) return Sample_Rate;

   procedure Set (Control : in out Sample_Rate_Control; Rate : in Sample_Rate);


   type Logarithmic_Control is new Normal_Control with private;


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
   end record;

   type Audio_Port is new Plugin_Port with record
      Handle : Buffer_Handle;
   end record;

   type Control_Port is abstract new Plugin_Port with record
      Value : aliased Control_Value;
   end record;

   type Normal_Control is new Control_Port with null record;

   type Toggle_Control is new Control_Port with null record;

   type Sample_Rate_Control is new Control_Port with null record;

   type Logarithmic_Control is new Normal_Control with null record;

   type Integer_Control is new Control_Port with null record;

end Solid.Audio.Ladspa.Host;
