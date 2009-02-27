package Solid.Audio.Ladspa is
   type Plugin_Index is new Interfaces.C.unsigned_long;
   type Plugin_ID is new Interfaces.C.unsigned_long;

   type Plugin_Handle is new Interfaces.C.Extensions.opaque_structure_def_ptr;

   subtype Control_Value is Sample; -- This is required for connecting audio ports.  Otherwise GNAT gives a bug box.
end Solid.Audio.Ladspa;
