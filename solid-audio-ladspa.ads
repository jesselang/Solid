package Solid.Audio.Ladspa is
   type Plugin_Index is new Interfaces.C.unsigned_long;
   type Plugin_ID is new Interfaces.C.unsigned_long;

   type Plugin_Handle is new Interfaces.C.Extensions.opaque_structure_def_ptr;

   type Control_Value is new Interfaces.C.C_Float;
end Solid.Audio.Ladspa;
