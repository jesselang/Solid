with Interfaces.C.Extensions;
with Interfaces.C.Pointers;
with Interfaces.C.Strings;
with System;

package Solid.Audio.Ladspa.Thin is
   --subtype LADSPA_Audio_Data is Audio.Sample;

   package C renames Interfaces.C;

   subtype LADSPA_Control_Data is Control_Value;
   --~ type LADSPA_Control_Handle is access all LADSPA_Control_Data;
   --~ pragma Convention (C, LADSPA_Control_Handle);

   -- The following are C.int actually, but we need a modular type for bitwise operations.
   type LADSPA_Properties is new C.unsigned;

   -- Properties.
   Realtime        : constant LADSPA_Properties := 1;
   Inplace_Broken  : constant LADSPA_Properties := 2;
   Hard_RT_Capable : constant LADSPA_Properties := 4;

   type LADSPA_PortDescriptor is new C.unsigned;

   -- Port Descriptors.
   Input   : constant LADSPA_PortDescriptor := 1;
   Output  : constant LADSPA_PortDescriptor := 2;
   Control : constant LADSPA_PortDescriptor := 4;
   Audio   : constant LADSPA_PortDescriptor := 8;

   type LADSPA_PortRangeHintDescriptor is new C.unsigned;

   -- Port Range Hints.

   Bounded_Below   : constant LADSPA_PortRangeHintDescriptor := 16#001#;
   Bounded_Above   : constant LADSPA_PortRangeHintDescriptor := 16#002#;

   Toggled         : constant LADSPA_PortRangeHintDescriptor := 16#004#;
   Sample_Rate     : constant LADSPA_PortRangeHintDescriptor := 16#008#;
   Logarithmic     : constant LADSPA_PortRangeHintDescriptor := 16#010#;
   Integer         : constant LADSPA_PortRangeHintDescriptor := 16#020#;

   Default_Mask    : constant LADSPA_PortRangeHintDescriptor := 16#3c0#;
   Default_None    : constant LADSPA_PortRangeHintDescriptor := 16#000#;

   Default_Minimum : constant LADSPA_PortRangeHintDescriptor := 16#040#;
   Default_Low     : constant LADSPA_PortRangeHintDescriptor := 16#080#;
   Default_Middle  : constant LADSPA_PortRangeHintDescriptor := 16#0c0#;
   Default_High    : constant LADSPA_PortRangeHintDescriptor := 16#100#;
   Default_Maximum : constant LADSPA_PortRangeHintDescriptor := 16#140#;
   Default_0       : constant LADSPA_PortRangeHintDescriptor := 16#200#;
   Default_1       : constant LADSPA_PortRangeHintDescriptor := 16#240#;
   Default_100     : constant LADSPA_PortRangeHintDescriptor := 16#280#;
   Default_440     : constant LADSPA_PortRangeHintDescriptor := 16#2c0#;

   type LADSPA_PortRangeHint is record
      HintDescriptor : LADSPA_PortRangeHintDescriptor;
      LowerBound     : LADSPA_Control_Data;
      UpperBound     : LADSPA_Control_Data;
   end record;
   pragma Convention (C, LADSPA_PortRangeHint);

   No_PortRangeHint : constant LADSPA_PortRangeHint := (HintDescriptor => LADSPA_PortRangeHintDescriptor'First,
                                                        LowerBound     => LADSPA_Control_Data'Last,
                                                        UpperBound     => LADSPA_Control_Data'First);

   subtype LADSPA_Handle is Plugin_Handle;

   subtype chars_ptr is C.Strings.chars_ptr;

   type Port_Index is new C.unsigned_long;

   type PortDescriptor_List is array (Port_Index range <>) of aliased LADSPA_PortDescriptor;
   package PortDescriptor_Lists is new C.Pointers
      (Index              => Port_Index,
       Element            => LADSPA_PortDescriptor,
       Element_Array      => PortDescriptor_List,
       Default_Terminator => LADSPA_PortDescriptor'First);
       -- Do *not* use operations that use Default_Terminator.
       -- Use length-based operations only.
       -- This is not a valid terminator.

   type PortName_List is array (Port_Index range <>) of aliased chars_ptr;
   package PortName_Lists is new C.Pointers
      (Index              => Port_Index,
       Element            => chars_ptr,
       Element_Array      => PortName_List,
       Default_Terminator => C.Strings.Null_Ptr);
       -- Do *not* use operations that use Default_Terminator.
       -- Use length-based operations only.
       -- This is not a valid terminator.

   type PortRangeHint_List is array (Port_Index range <>) of aliased LADSPA_PortRangeHint;
   package PortRangeHint_Lists is new C.Pointers
      (Index              => Port_Index,
       Element            => LADSPA_PortRangeHint,
       Element_Array      => PortRangeHint_List,
       Default_Terminator => No_PortRangeHint);
       -- Do *not* use operations that use Default_Terminator.
       -- Use length-based operations only.
       -- This is not a valid terminator.

   type LADSPA_Descriptor;
   type LADSPA_Descriptor_Handle is access LADSPA_Descriptor;

   No_Descriptor : constant LADSPA_Descriptor_Handle := null;

   type Instantiate_Function is access function (Descriptor : LADSPA_Descriptor_Handle; SampleRate : C.unsigned_long)
   return LADSPA_Handle;
   pragma Convention (C, Instantiate_Function);

   type Instance_Procedure is access procedure (Instance : in LADSPA_Handle);
   pragma Convention (C, Instance_Procedure);

   -- Consider a port index/number/ID type.

   type Connect_Port_Procedure is access procedure (Instance     : in     LADSPA_Handle;
                                                    Port         : in     Port_Index;
                                                    DataLocation : in out LADSPA_Control_Data);
   pragma Convention (C, Connect_Port_Procedure);

   type Run_Procedure is access procedure (Instance : in LADSPA_Handle; SampleCount : in C.unsigned_long);
   pragma Convention (C, Run_Procedure);

   type Run_Gain_Procedure is access procedure (Instance : in LADSPA_Handle; Gain : in LADSPA_Control_Data);
   pragma Convention (C, Run_Gain_Procedure);

   type LADSPA_Descriptor is record
      UniqueID            : Ladspa.Plugin_ID;
      Label               : chars_ptr;
      Properties          : LADSPA_Properties;
      Name                : chars_ptr;
      Maker               : chars_ptr;
      Copyright           : chars_ptr;
      PortCount           : Port_Index;
      PortDescriptors     : PortDescriptor_Lists.Pointer;
      PortNames           : PortName_Lists.Pointer;
      PortRangeHints      : PortRangeHint_Lists.Pointer;
      ImplementationData  : C.Extensions.void_ptr;
      -- Operations.
      instantiate         : Instantiate_Function;
      connect_port        : Connect_Port_Procedure;
      activate            : Instance_Procedure;
      run                 : Run_Procedure;
      run_adding          : Run_Procedure;
      set_run_adding_gain : Run_Gain_Procedure;
      deactivate          : Instance_Procedure;
      cleanup             : Instance_Procedure;
   end record;
   pragma Convention (C, LADSPA_Descriptor);

   type LADSPA_Descriptor_Function is access function (Index : Ladspa.Plugin_Index) return LADSPA_Descriptor_Handle;
   pragma Convention (C, LADSPA_Descriptor_Function);
end Solid.Audio.Ladspa.Thin;
