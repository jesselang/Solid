with Interfaces.C.Extensions;
with Interfaces.C.Pointers;

package Solid.Audio is
   type Sample is new Interfaces.C.C_Float;
   type Buffer_Size is new Interfaces.C.Extensions.Unsigned_32;
   type Sample_Buffer is array (Buffer_Size range <>) of aliased Sample;
   package Buffers is new Interfaces.C.Pointers (Index              => Buffer_Size,
                                                 Element            => Sample,
                                                 Element_Array      => Sample_Buffer,
                                                 Default_Terminator => Sample'First);
                                                 -- Do *not* use operations that use Default_Terminator.
                                                 -- Use length-based operations only.
                                                 -- This is not a valid terminator.
   subtype Buffer_Handle is Buffers.Pointer;

   type Sample_Rate is range 8000 .. 48000;
end Solid.Audio;
