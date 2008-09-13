with System.Restrictions;

package body Solid.Audio.Jack.Thread is
   procedure adainit;

   procedure Initialize (Argument : in Interfaces.C.Extensions.void_ptr) is
   begin -- Initialize
      adainit;
   end Initialize;

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

      procedure adainit is
      E017 : Boolean; pragma Import (Ada, E017, "system__secondary_stack_E");
      E013 : Boolean; pragma Import (Ada, E013, "system__soft_links_E");
      E023 : Boolean; pragma Import (Ada, E023, "system__exception_table_E");
      E006 : Boolean; pragma Import (Ada, E006, "ada__calendar_E");
      E051 : Boolean; pragma Import (Ada, E051, "ada__calendar__delays_E");
      E080 : Boolean; pragma Import (Ada, E080, "ada__io_exceptions_E");
      E058 : Boolean; pragma Import (Ada, E058, "ada__tags_E");
      E056 : Boolean; pragma Import (Ada, E056, "ada__streams_E");
      E089 : Boolean; pragma Import (Ada, E089, "interfaces__c_E");
      E092 : Boolean; pragma Import (Ada, E092, "interfaces__c__strings_E");
      E073 : Boolean; pragma Import (Ada, E073, "system__finalization_root_E");
      E075 : Boolean; pragma Import (Ada, E075, "system__finalization_implementation_E");
      E071 : Boolean; pragma Import (Ada, E071, "ada__finalization_E");
      E083 : Boolean; pragma Import (Ada, E083, "ada__finalization__list_controller_E");
      E081 : Boolean; pragma Import (Ada, E081, "system__file_control_block_E");
      E069 : Boolean; pragma Import (Ada, E069, "system__file_io_E");
      E055 : Boolean; pragma Import (Ada, E055, "ada__text_io_E");
      E087 : Boolean; pragma Import (Ada, E087, "solid__audio_E");
--      E085 : Boolean; pragma Import (Ada, E085, "jack_process_E");
      E094 : Boolean; pragma Import (Ada, E094, "solid__audio__jack_E");
      E100 : Boolean; pragma Import (Ada, E100, "solid__audio__jack__thread_E");
      E098 : Boolean; pragma Import (Ada, E098, "solid__interfaces__bitwise_enumerations_E");
      E095 : Boolean; pragma Import (Ada, E095, "solid__audio__jack__thin_E");

      Main_Priority : Integer;
      pragma Import (C, Main_Priority, "__gl_main_priority");
      Time_Slice_Value : Integer;
      pragma Import (C, Time_Slice_Value, "__gl_time_slice_val");
      WC_Encoding : Character;
      pragma Import (C, WC_Encoding, "__gl_wc_encoding");
      Locking_Policy : Character;
      pragma Import (C, Locking_Policy, "__gl_locking_policy");
      Queuing_Policy : Character;
      pragma Import (C, Queuing_Policy, "__gl_queuing_policy");
      Task_Dispatching_Policy : Character;
      pragma Import (C, Task_Dispatching_Policy, "__gl_task_dispatching_policy");
      -- FSF GNAT doesn't seem to like the following commented lines.
      --~ Priority_Specific_Dispatching : System.Address;
      --~ pragma Import (C, Priority_Specific_Dispatching, "__gl_priority_specific_dispatching");
      --~ Num_Specific_Dispatching : Integer;
      --~ pragma Import (C, Num_Specific_Dispatching, "__gl_num_specific_dispatching");
      Interrupt_States : System.Address;
      pragma Import (C, Interrupt_States, "__gl_interrupt_states");
      Num_Interrupt_States : Integer;
      pragma Import (C, Num_Interrupt_States, "__gl_num_interrupt_states");
      Unreserve_All_Interrupts : Integer;
      pragma Import (C, Unreserve_All_Interrupts, "__gl_unreserve_all_interrupts");
      Zero_Cost_Exceptions : Integer;
      pragma Import (C, Zero_Cost_Exceptions, "__gl_zero_cost_exceptions");
      Detect_Blocking : Integer;
      pragma Import (C, Detect_Blocking, "__gl_detect_blocking");
      Default_Stack_Size : Integer;
      pragma Import (C, Default_Stack_Size, "__gl_default_stack_size");
      -- FSF GNAT doesn't seem to like the following commented lines.
      --~ Leap_Seconds_Support : Integer;
      --~ pragma Import (C, Leap_Seconds_Support, "__gl_leap_seconds_support");

      procedure Install_Handler;
      pragma Import (C, Install_Handler, "__gnat_install_handler");

      Handler_Installed : Integer;
      pragma Import (C, Handler_Installed, "__gnat_handler_installed");
   begin
      Main_Priority := -1;
      Time_Slice_Value := -1;
      WC_Encoding := 'b';
      Locking_Policy := ' ';
      Queuing_Policy := ' ';
      Task_Dispatching_Policy := ' ';
      -- If this is needed, it will need to be tweaked for FSF GNAT.
      --~ System.Restrictions.Run_Time_Restrictions :=
        --~ (Set =>
          --~ (False, False, False, False, False, False, False, False,
           --~ False, False, False, False, False, False, False, False,
           --~ False, False, False, False, False, False, False, False,
           --~ False, False, False, False, False, False, False, False,
           --~ False, False, False, False, False, False, False, False,
           --~ False, False, False, False, False, False, False, False,
           --~ False, False, False, False, False, False, False, False,
           --~ False, False, False, False, False, False, False, False,
           --~ False, False),
         --~ Value => (0, 0, 0, 0, 0, 0, 0),
         --~ Violated =>
          --~ (False, False, True, True, False, True, True, True,
           --~ True, True, False, False, True, False, False, True,
           --~ True, False, True, True, True, True, True, True,
           --~ False, False, True, False, True, False, True, False,
           --~ False, True, False, False, False, True, False, True,
           --~ False, False, False, False, False, False, False, True,
           --~ True, True, False, False, False, True, True, False,
           --~ True, True, True, False, False, False, False, False,
           --~ False, False),
         --~ Count => (0, 0, 0, 0, 0, 0, 0),
         --~ Unknown => (False, False, False, False, False, False, False));

      -- FSF GNAT doesn't seem to like the following commented lines.
      --~ Priority_Specific_Dispatching :=
        --~ Local_Priority_Specific_Dispatching'Address;
      --~ Num_Specific_Dispatching := 0;
      Interrupt_States := Local_Interrupt_States'Address;
      Num_Interrupt_States := 0;
      Unreserve_All_Interrupts := 0;
      Zero_Cost_Exceptions := 1;
      Detect_Blocking := 0;
      Default_Stack_Size := -1;
      -- FSF GNAT doesn't seem to like the following commented lines.
      --~ Leap_Seconds_Support := 0;

      -- This doesn't seem to be required, but we'll leave it in for now.
      if Handler_Installed = 0 then
         Install_Handler;
      end if;

      ----------------------------------
      -- These are required.
      System.Soft_Links'Elab_Body;
      E013 := True;
      System.Secondary_Stack'Elab_Body;
      E017 := True;
      ------------------------------------
      -- Not exactly sure about these, but we'll leave them in for now.
      System.Exception_Table'Elab_Body;
      E023 := True;
      Ada.Calendar'Elab_Spec;
      Ada.Calendar'Elab_Body;
      E006 := True;
      Ada.Io_Exceptions'Elab_Spec;
      E080 := True;
      Ada.Tags'Elab_Spec;
      Ada.Streams'Elab_Spec;
      E056 := True;
      Interfaces.C'Elab_Spec;
      E089 := True;
      Interfaces.C.Strings'Elab_Spec;
      E092 := True;
      System.Finalization_Root'Elab_Spec;
      E073 := True;
      Ada.Calendar.Delays'Elab_Body;
      E051 := True;
      System.Finalization_Implementation'Elab_Spec;
      System.Finalization_Implementation'Elab_Body;
      E075 := True;
      Ada.Finalization'Elab_Spec;
      E071 := True;
      Ada.Finalization.List_Controller'Elab_Spec;
      E083 := True;
      -----------------------------------------------------------------
      -- The following commented lines cause blocking on finalization.
      --~ System.File_Control_Block'Elab_Spec;
      --~ E081 := True;
      --~ System.File_Io'Elab_Body;
      --~ E069 := True;
      ------------------------------------------------------------------
      -- Not exactly sure about these, but we'll leave them in for now.
      Ada.Tags'Elab_Body;
      E058 := True;
      ------------------------------------------------------------------
      -- The following commented lines cause blocking on finalization.
      --~ Ada.Text_Io'Elab_Spec;
      --~ Ada.Text_Io'Elab_Body;
      --~ E055 := True;
      ------------------------------------------------------------------
      -- These are required.
      Solid.Audio'Elab_Spec;
      E087 := True;
      Solid.Audio.Jack'Elab_Spec;
--      E085 := True;
      E100 := True;
      E098 := True;
      Solid.Audio.Jack.Thin'Elab_Spec;
      E095 := True;
      E094 := True;
   end adainit;
end Solid.Audio.Jack.Thread;
