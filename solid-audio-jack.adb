with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Interfaces.C.Strings;
with Solid.Audio.Jack.Thin;
--with Solid.Audio.Jack.Thread;
with System;

package body Solid.Audio.Jack is
   function New_String (Str : String) return Interfaces.C.Strings.chars_ptr renames Interfaces.C.Strings.New_String;

   function Process_Audio (nframes : Thin.jack_nframes_t; Context : Thin.void_ptr) return Thin.int;
   -- Callback handed to Jack for audio processing for clients.
   pragma Convention (C, Process_Audio); -- Jack needs to call this as a callback, so it must be declared with a C convention.

   procedure Open (Connection : out Client; Name : in String; Server : in String := "") is
      Options : Thin.Bitwise_Options.Value_List (1 .. 3) := (Thin.JackNoStartServer,
                                                             Thin.JackUseExactName,
                                                             Thin.JackNullOption);
      Status  : aliased Thin.jack_status_t;
      Result  : Thin.int;

      use type Thin.int;
   begin -- Open
      if Server /= "" then
         Options (Options'Last) := Thin.JackServerName;
      end if;

      Connection.Handle := Thin.jack_client_open (New_String (Name),
                                                  Options     => Thin.Bitwise_Options.Bits (Options),
                                                  Status      => Status'Access,
                                                  Server_Name => New_String (Server) );

      if Connection.Handle = null then
         raise Client_Error with "Couldn't connect to jack: " & Thin.JackStatus'Image (Thin.Bitwise_Status.Values (Status) (1) );
      end if;

      -- Required for the stack to remain sane.
      --~ Result := Thin.jack_set_thread_init_callback (Connection.Handle,
                                                    --~ thread_init_callback => Thread.Initialize'Access,
                                                    --~ arg                  => System.Null_Address);

      --~ if Result /= 0 then
         --~ raise Client_Error with "Couldn't set thread init callback for jack.";
      --~ end if;

      -- Set the internal callback, and activate.

      Result := Thin.jack_set_process_callback (Connection.Handle,
                                                process_callback => Process_Audio'Access,
                                                arg              => Process_Conversion.To_Address (Connection.Processes) );

      if Result /= 0 then
         raise Client_Error with "Couldn't set process callback for jack.";
      end if;
   exception -- Open
      when E : others =>
         Ada.Text_IO.Put_Line ("Open - " & Ada.Exceptions.Exception_Information (E) );
         raise;
   end Open;

   procedure Close (Connection : in out Client) is
      Result : Thin.int;

      use type Thin.int;
   begin -- Close
      if Connection.Handle = null then
         return;
      end if;

      if Connection.State = Activated then
         Result := Thin.jack_deactivate (Connection.Handle);
         Connection.State := Deactivated;
      end if;

      Result := Thin.jack_client_close (Connection.Handle);

      if Result /= 0 then
         raise Client_Error with "Couldn't disconnect from jack";
      end if;
   exception -- Close
      when E : others =>
         Ada.Text_IO.Put_Line ("Close - " & Ada.Exceptions.Exception_Information (E) );
         raise;
   end Close;

   function Max_Buffer_Size (Connection : Client) return Solid.Audio.Buffer_Size is
   begin -- Max_Buffer_Size
      if Connection.State > Initialized then
         raise Client_Error with "Client already activated.";
      end if;

      return Thin.jack_get_buffer_size (Connection.Handle);
   end Max_Buffer_Size;

   procedure Register (Connection : in out Client; Input : in String; Output : in String; Process : not null Audio_Processor) is
      New_Process : Audio_Process;
      Result      : Thin.int;

      use type Thin.int;
      use type Process_Handle;
   begin -- Register
      if Connection.Handle = null then
         raise Client_Error with "Client not initialized.";
      end if;

      if Connection.Processes = null then
         raise Client_Error with "No processes.";
      elsif Connection.Processes (1) /= No_Process and Connection.Processes (2) /= No_Process then
         raise Client_Error with "Client has both channels registered.";
      else
         null;
      end if;

      New_Process.Input := Thin.jack_port_register (Connection.Handle,
                                                    port_name   => New_String (Input),
                                                    port_type   => New_String (Thin.JACK_DEFAULT_AUDIO_TYPE),
                                                    flags       => Thin.Bitwise_Flags.Bits (Thin.JackPortIsInput),
                                                    buffer_size => 0);

      if New_Process.Input = null then
         raise Client_Error with "Could not register input port.";
      end if;

      New_Process.Output := Thin.jack_port_register (Connection.Handle,
                                                     port_name   => New_String (Output),
                                                     port_type   => New_String (Thin.JACK_DEFAULT_AUDIO_TYPE),
                                                     flags       => Thin.Bitwise_Flags.Bits (Thin.JackPortIsOutput),
                                                     buffer_size => 0);

      if New_Process.Output = null then
         raise Client_Error with "Could not register output port.";
      end if;

      New_Process.Process := Process;

      if Connection.Processes (1) = No_Process then
         Connection.Processes (1) := New_Process;
      elsif Connection.Processes (2) = No_Process then
         Connection.Processes (2) := New_Process;
      else
         raise Client_Error with "No more processes available.";
      end if;

      if Connection.State < Activated then
         Result := Thin.jack_activate (Connection.Handle);

         if Result /= 0 then
            raise Client_Error with "Couldn't activate client.";
         end if;

         Connection.State := Activated;
      end if;
   exception -- Register
      when E : others =>
         Ada.Text_IO.Put_Line ("Register - " & Ada.Exceptions.Exception_Information (E) );
         raise;
   end Register;

   procedure Connect_To_Physical_Ports (Connection : in out Client) is
      procedure Free is new Ada.Unchecked_Deallocation (Thin.chars_ptr, Thin.port_array_t);

      Ports  : Thin.port_array_t;
      Result : Thin.int;

      use type Thin.int;
      use type Thin.port_array_t;
   begin -- Connect_To_Physical_Ports
      Ports := Thin.jack_get_ports (Connection.Handle,
                                    port_name_pattern => Interfaces.C.Strings.Null_Ptr,
                                    type_name_pattern => Interfaces.C.Strings.Null_Ptr,
                                    flags             => Thin.Bitwise_Flags.Bits ( (Thin.JackPortIsOutput,
                                                                                    Thin.JackPortIsPhysical) ) );

      if Ports = null then
         raise Client_Error with "Couldn't get an output port.";
      end if;

      Result := Thin.jack_connect (Connection.Handle,
                                   source_port      => Ports.all,
                                   destination_port => Thin.jack_port_name (Connection.Processes (1).Input) );
      Free (Ports);

      if Result /= 0 then
         raise Client_Error with "Could not connect input port.";
      end if;

      Ports := Thin.jack_get_ports (Connection.Handle,
                                    port_name_pattern => Interfaces.C.Strings.Null_Ptr,
                                    type_name_pattern => Interfaces.C.Strings.Null_Ptr,
                                    flags             => Thin.Bitwise_Flags.Bits ( (Thin.JackPortIsInput,
                                                                                    Thin.JackPortIsPhysical) ) );

      if Ports = null then
         raise Client_Error with "Couldn't get an input port.";
      end if;

      Result := Thin.jack_connect (Connection.Handle,
                                   source_port      => Thin.jack_port_name (Connection.Processes (1).Output),
                                   destination_port => Ports.all);
      Free (Ports);

      if Result /= 0 then
         raise Client_Error with "Could not connect output port.";
      end if;
   exception -- Connect_To_Physical_Ports
      when E : others =>
         Ada.Text_IO.Put_Line ("Connect_To_Physical_Ports - " & Ada.Exceptions.Exception_Information (E) );
         raise;
   end Connect_To_Physical_Ports;

   procedure Silence (Buffer : out Sample_Buffer) is
      Silent_Sample : constant Sample := 0.0;
   begin -- Silence
      Buffer := (others => Silent_Sample);
   end Silence;

   function Process_Audio (nframes : Thin.jack_nframes_t; Context : Thin.void_ptr) return Thin.int is
      procedure Write (Buffer : in Sample_Buffer; To : in Buffer_Pointer);
      -- Writes Buffer to the handle in To.

      procedure Write (Buffer : in Sample_Buffer; To : in Buffer_Pointer) is
         Location : Buffer_Pointer := To;
      begin -- Write
         for Index in Buffer'Range loop
            Location.all := Buffer (Index);
            if Index < Buffer'Last then
               C_Buffers.Increment (Location);
            end if;
         end loop;
      end Write;

      Processes     : constant Process_Handle         := Process_Conversion.To_Pointer (Context);
      Buffer_Length : constant Interfaces.C.ptrdiff_t := Interfaces.C.ptrdiff_t (nframes);

      Input_Handle  : Buffer_Pointer;
      Output_Handle : Buffer_Pointer;
      Output_Buffer : Sample_Buffer (1 .. nframes);

      use type Process_Handle;
      use type Interfaces.C.ptrdiff_t;
   begin -- Process_Audio
      if Processes /= null then
         for Index in Processes'Range loop
            if Processes (Index) /= No_Process then
               Input_Handle := Thin.jack_port_get_buffer (port => Processes (Index).Input, nframes => nframes);
               Output_Handle := Thin.jack_port_get_buffer (port => Processes (Index).Output, nframes => nframes);

               Output_Buffer := C_Buffers.Value (Input_Handle, Length => Buffer_Length);
               -- Does this copy the input buffer to the output buffer by default?
               -- Should it be silence instead?

               Processes (Index).Process (Input  => C_Buffers.Value (Input_Handle, Length => Buffer_Length),
                                          Output => Output_Buffer);
               Write (Buffer => Output_Buffer, To => Output_Handle);
            end if;
         end loop;
      end if;

      return 0;
   exception -- Process_Audio
      when E : others =>
         Ada.Text_IO.Put_Line ("Process_Audio - " & Thin.jack_nframes_t'Image (nframes) & " " &
                               Ada.Exceptions.Exception_Information (E) );

         return 1;
   end Process_Audio;
end Solid.Audio.Jack;
