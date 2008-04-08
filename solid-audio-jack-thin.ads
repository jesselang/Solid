with Interfaces.C.Extensions;
with Interfaces.C.Pointers;
with Interfaces.C.Strings;
with Solid.Interfaces.Bitwise_Enumerations;

private package Solid.Audio.Jack.Thin is
   pragma Linker_Options ("-ljack");
   pragma Linker_Options ("-lpthread");
   pragma Linker_Options ("-lrt");

   JACK_DEFAULT_AUDIO_TYPE : constant String := "32 bit float mono audio";

   package C renames Standard.Interfaces.C;
   subtype jack_nframes_t is Solid.Audio.Buffer_Size;
   subtype void_ptr is C.Extensions.void_ptr;
   subtype int is C.int;

   type JackProcessCallback is access function (nframes : jack_nframes_t; arg : void_ptr) return int;
   -- typedef int  (*JackProcessCallback)(jack_nframes_t nframes, void *arg);
   pragma Convention (C, JackProcessCallback);

   subtype chars_ptr is C.Strings.chars_ptr;
   type jack_options_t is new C.unsigned_long;
   type jack_status_t is new C.unsigned_long;

   subtype jack_client_t is Client_Handle;

   function jack_client_open (client_name : chars_ptr;
                              options     : jack_options_t;
                              status      : access jack_status_t;
                              server_name : chars_ptr)
   return jack_client_t; -- status is an "out" pointer
   pragma Import (C, jack_client_open);
   --~ jack_client_t *jack_client_open (const char *client_name,
            --~ jack_options_t options,
            --~ jack_status_t *status, ...);

   function jack_client_close (client : jack_client_t) return int;
   pragma Import (C, jack_client_close);
   --~ int jack_client_close (jack_client_t *client);

   function jack_set_process_callback (client : jack_client_t; process_callback : JackProcessCallback; arg : void_ptr) return int;
   pragma Import (C, jack_set_process_callback);
   --~ int jack_set_process_callback (jack_client_t *client,
               --~ JackProcessCallback process_callback,
               --~ void *arg);

   type jack_flags_t is new C.unsigned_long;
   subtype jack_port_t is Port_Handle;

   function jack_port_register (client      : jack_client_t;
                                port_name   : chars_ptr;
                                port_type   : chars_ptr;
                                flags       : jack_flags_t;
                                buffer_size : C.unsigned_long) -- Just pass 0 (zero)
   return jack_port_t;
   pragma Import (C, jack_port_register);
   --~ jack_port_t *jack_port_register (jack_client_t *client,
                                 --~ const char *port_name,
                                 --~ const char *port_type,
                                 --~ unsigned long flags,
                                 --~ unsigned long buffer_size);

   function jack_activate (client : jack_client_t) return int;
   pragma Import (C, jack_activate);
   --~ int jack_activate (jack_client_t *client);

   function jack_deactivate (client : jack_client_t) return int;
   pragma Import (C, jack_deactivate);
   --~ int jack_deactivate (jack_client_t *client);

   type port_array is array (C.size_t range <>) of aliased chars_ptr;
   package port_array_pointers is new C.Pointers (Index => C.size_t,
                                                  Element => chars_ptr,
                                                  Element_Array => port_array,
                                                  Default_Terminator => C.Strings.Null_Ptr);
   subtype port_array_t is port_array_pointers.Pointer;



   function jack_get_ports (client            : jack_client_t;
                            port_name_pattern : chars_ptr;
                            type_name_pattern : chars_ptr;
                            flags             : jack_flags_t)
   return port_array_t;
   pragma Import (C, jack_get_ports);
   --~ const char **jack_get_ports (jack_client_t *,
            --~ const char *port_name_pattern,
			   --~ const char *type_name_pattern,
			   --~ unsigned long flags);

   function jack_connect (client           : jack_client_t;
                          source_port      : chars_ptr;
                          destination_port : chars_ptr)
   return int;
   pragma Import (C, jack_connect);
   --~ int jack_connect (jack_client_t *,
		  --~ const char *source_port,
		  --~ const char *destination_port);

   function jack_port_name (port : jack_port_t) return chars_ptr;
   pragma Import (C, jack_port_name);
   --~ const char *jack_port_name (const jack_port_t *port);

   function jack_port_get_buffer (port : jack_port_t; nframes : jack_nframes_t) return Buffer_Handle;
   pragma Import (C, jack_port_get_buffer);
      -- void *jack_port_get_buffer (jack_port_t *, jack_nframes_t);

   -- Jack client options.
   type JackOptions is (JackNullOption, JackNoStartServer, JackUseExactName, JackServerName, JackLoadName, JackLoadInit);

   for JackOptions use
      (JackNullOption    =>  0,
       JackNoStartServer =>  1,
       JackUseExactName  =>  2,
       JackServerName    =>  4,
       JackLoadName      =>  8,
       JackLoadInit      => 16);

   package Bitwise_Options is new Solid.Interfaces.Bitwise_Enumerations (JackOptions, jack_options_t);

   JackOpenOptions : constant Bitwise_Options.Value_List := (JackServerName, JackNoStartServer, JackUseExactName);

   -- Server status.
   type JackStatus is (JackFailure,
                       JackNameNotUnique,
                       JackServerStarted,
                       JackServerFailed,
                       JackServerError,
                       JackNoSuchClient,
                       JackLoadFailure,
                       JackInitFailure,
                       JackShmFailure,
                       JackVersionError);

   for JackStatus use
      (JackFailure       =>   1,
       JackNameNotUnique =>   2,
       JackServerStarted =>   4,
       JackServerFailed  =>   8,
       JackServerError   =>  16,
       JackNoSuchClient  =>  32,
       JackLoadFailure   =>  64,
       JackInitFailure   => 128,
       JackShmFailure    => 256,
       JackVersionError  => 512);

   package Bitwise_Status is new Solid.Interfaces.Bitwise_Enumerations (JackStatus, jack_status_t);

   -- Port flags.
   type JackPortFlags is (JackPortIsInput, JackPortIsOutput, JackPortIsPhysical, JackPortCanMonitor, JackPortIsTerminal);

   for JackPortFlags use
      (JackPortIsInput    =>  1,
       JackPortIsOutput   =>  2,
       JackPortIsPhysical =>  4,
       JackPortCanMonitor =>  8,
       JackPortIsTerminal => 16);

   package Bitwise_Flags is new Solid.Interfaces.Bitwise_Enumerations (JackPortFlags, jack_flags_t);
end Solid.Audio.Jack.Thin;