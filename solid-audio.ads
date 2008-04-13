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
   subtype Buffer_Handle is Buffers.Pointer;
end Solid.Audio;
