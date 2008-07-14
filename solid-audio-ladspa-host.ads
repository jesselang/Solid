with Solid.Strings;

package Solid.Audio.Ladspa.Host is
   Default_Plugin_Path : constant Strings.String_Array;

   Host_Error : exception;

   procedure Initialize (Plugin_Path : in Strings.String_Array := Default_Plugin_Path; Refresh : Boolean := False);
   -- Initializes the LADSPA host, enumerating plugins found in Plugin_Path.
   -- Raises Host_Error if
   -- The following operations will call Initialize if it has not yet been called.

   type Plugin_Order is (Library, -- Libraries in alphabetical order, plugins ordered within the library.
                         ID,      -- Ordered by plugin unique ID.
                         Label);  -- Ordered by plugin unique label.

   generic -- Iterate
      with procedure Process (Library   : in String;
                              Index     : in Plugin_Index;
                              ID        : in Plugin_ID;
                              Label     : in String;
                              Name      : in String;
                              Copyright : in String);
   procedure Iterate (Order : in Plugin_Order := Library; Refresh : Boolean := False);
private -- Solid.Audio.Ladspa.Host
   function "+" (Left : String) return Strings.U_String renames Strings."+";

   Default_Plugin_Path : constant Strings.String_Array := (+"/usr/lib/ladspa",
                                                           +"/usr/local/lib/ladspa");
end Solid.Audio.Ladspa.Host;
