with Interfaces.C.Extensions;
with Interfaces.C.Pointers;

package Solid.Audio is
   type Sample is new Interfaces.C.C_Float;
   type Buffer_Size is new Interfaces.C.Extensions.Unsigned_32;
   type Sample_Buffer is array (Buffer_Size range <>) of aliased Sample;
   pragma Convention (C, Sample_Buffer); -- Not sure if this is needed to pass buffers around.
   type Buffer_Handle is access Sample_Buffer;
   package C_Buffers is new Interfaces.C.Pointers (Index              => Buffer_Size,
                                                   Element            => Sample,
                                                   Element_Array      => Sample_Buffer,
                                                   Default_Terminator => Sample'First);
                                                   -- Do *not* use operations that use Default_Terminator.
                                                   -- Use length-based operations only.
                                                   -- This is not a valid terminator.

   subtype Buffer_Pointer is C_Buffers.Pointer; -- This is an access type for Sample.
   subtype Sample_Handle  is C_Buffers.Pointer;

   type Sample_Rate is range 8000 .. 48000;
end Solid.Audio;
