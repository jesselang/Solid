package Solid.Audio.Ladspa is
   type Plugin_Index is new Interfaces.C.unsigned_long;
   type Plugin_ID is new Interfaces.C.unsigned_long;

   type Plugin_Handle is new Interfaces.C.Extensions.opaque_structure_def_ptr;
end Solid.Audio.Ladspa;
