with Solid.Audio.Jack;

package body Jack_Process is
   procedure Foo (Input : in Solid.Audio.Sample_Buffer; Output : out Solid.Audio.Sample_Buffer) is
   begin -- Foo
      --Solid.Audio.Jack.Silence (Buffer => Output);
      --Output := (others => Solid.Audio.Sample'Last);
      --Output := Input;
      null;
   end Foo;
end Jack_Process;
