with Interfaces.C.Extensions;

private package Solid.Audio.Jack.Thread is
   procedure Initialize (Argument : in Interfaces.C.Extensions.void_ptr);
   pragma Convention (C, Initialize);

   --~ procedure Finalize (Argument : in Interfaces.C.Extensions.void_ptr);
   --~ pragma Convention (C, Finalize);
end Solid.Audio.Jack.Thread;
