with Solid.Debug.Analyzer;

procedure Analyzer_Test is
   package Analyzer is new Solid.Debug.Analyzer (Data => Integer);

   --Object : Integer := 1;
begin -- Analyzer_Test
   for Index in 1 .. 4 loop
      Analyzer.Display (Item => Index, Width => 8);
   end loop;
end Analyzer_Test;
