with Interfaces.C.Extensions;
with Interfaces.C.Pointers;
with Interfaces.C.Strings;

package Solid.Audio.Ladspa.Thin is
   subtype LADSPA_Audio_Data is Audio.Sample;

   package C renames Interfaces.C;

   subtype LADSPA_Control_Data is Control_Value;
   type LADSPA_Control_Handle is access all LADSPA_Control_Data;

   -- The following are C.int actually, but we need a modular type for bitwise operations.
   type LADSPA_Properties is new C.unsigned;
   type LADSPA_PortDescriptor is new C.unsigned;
   type LADSPA_PortRangeHintDescriptor is new C.unsigned;

   type LADSPA_PortRangeHint is record
      HintDescriptor : LADSPA_PortRangeHintDescriptor;
      LowerBound     : LADSPA_Control_Data;
      UpperBound     : LADSPA_Control_Data;
   end record;

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

   type Instance_Procedure is access procedure (Instance : in out LADSPA_Handle);
   --pragma Convention (C, Instance_Procedure);

   -- Consider a port index/number/ID type.

   type Connect_Port_Procedure is access procedure (Instance     : in out LADSPA_Handle;
                                                    Port         : in     Port_Index;
                                                    DataLocation : in     LADSPA_Control_Handle);

   type Run_Procedure is access procedure (Instance : in out LADSPA_Handle; SampleCount : in C.unsigned_long);

   type Run_Gain_Procedure is access procedure (Instance : in out LADSPA_Handle; Gain : in LADSPA_Control_Data);

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

   type LADSPA_Descriptor_Function is access function (Index : Ladspa.Plugin_Index) return LADSPA_Descriptor_Handle;

   -- Enumerations.
   type LADSPA_Properties_List is (Realtime, Inplace_Broken, Hard_RT_Capable);
   for LADSPA_Properties_List use (Realtime => 1, Inplace_Broken => 2, Hard_RT_Capable => 4);

   type LADSPA_PortDescriptor_List is (Input, Output, Control, Audio);
   for LADSPA_PortDescriptor_List use (Input => 1, Output => 2, Control => 4, Audio => 8);

   type LADSPA_PortRangeHintDescriptor_List is
      (Default_None,
       Bounded_Low,
       Bounded_High,
       Toggled,
       Sample_Rate,
       Logarithmic,
       Integer,
       Default_Minimum,
       Default_Low,
       Default_Middle,
       Default_High,
       Default_Maximum,
       Default_0,
       Default_1,
       Default_100,
       Default_440,
       Default_Mask);
   for LADSPA_PortRangeHintDescriptor_List use
      (Default_None    => 0,
       Bounded_Low     => 1,
       Bounded_High    => 2,
       Toggled         => 4,
       Sample_Rate     => 8,
       Logarithmic     => 16#10#,
       Integer         => 16#20#,
       Default_Minimum => 16#40#,
       Default_Low     => 16#80#,
       Default_Middle  => 16#c0#,
       Default_High    => 16#100#,
       Default_Maximum => 16#140#,
       Default_0       => 16#200#,
       Default_1       => 16#240#,
       Default_100     => 16#280#,
       Default_440     => 16#2c0#,
       Default_Mask    => 16#3c0#);
end Solid.Audio.Ladspa.Thin;
