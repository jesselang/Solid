package Solid.Audio.Energy is
   type Frequency is new Float; -- range 55.0 .. 8000.0;

   function Energy_Of (Data : Sample_Buffer) return Float;
   -- Returns total audio energy of samples in Data.

   function Energy_Of (Target : Frequency; Samples_Per_Second : Sample_Rate; Data : Sample_Buffer) return Float;
   -- Returns relative audio energy of frequency Target within samples in Data, using the Goertzel
   -- algorithm.
end Solid.Audio.Energy;

-- Originally written by Chip Richards.
