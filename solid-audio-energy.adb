with Ada.Numerics.Elementary_Functions;
use  Ada.Numerics.Elementary_Functions;

with Ada.Text_IO;

package body Solid.Audio.Energy is
   function Energy_Of (Data : Sample_Buffer) return Float is
      Energy : Float;
   begin  -- Energy_Of
      Energy := 0.0;

      for Index in Data'Range loop
         Energy := Energy + Float (Data (Index) ** 2);
      end loop;

      return Energy;
   end Energy_Of;

   function Energy_Of (Target : Frequency; Samples_Per_Second : Sample_Rate; Data : Sample_Buffer) return Float is
      Coeff : Float;
      Q1    : Float;
      Q2    : Float;
      K     : Float;
      Omega : Float;

      function Get_Energy return Float is
      begin  -- Get_Energy
         return Q1 * Q1 + Q2 * Q2 - Q1 * Q2 * Coeff;
      end Get_Energy;

      procedure Process_Sample (S : in Sample) is
         Q0 : Float;
      begin  -- Process_Sample
         Q0 := Coeff * Q1 - Q2 + Float (S);
         Q2 := Q1;
         Q1 := Q0;
      end Process_Sample;
   begin  -- Energy_Of
      K := 0.5 + ( (Float (Data'Length) * Float (Target) ) / Float (Samples_Per_Second) );
      Omega := (2.0 * Ada.Numerics.Pi * K ) / Float (Data'Length);
      Coeff := 2.0 * Cos (Omega);
      Q1 := 0.0;
      Q2 := 0.0;

      for Index in Data'Range loop
         Process_Sample (Data (Index) );
      end loop;

      return Get_Energy;
   end Energy_Of;
end Solid.Audio.Energy;
