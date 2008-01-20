generic -- Solid.Debug.Analyzer
   type Data is private;
package Solid.Debug.Analyzer is
   procedure Display (Item : in Data; Width : in Positive := Positive'Last);

   -- function Value (Item : in Data; Base : in XXX) return XXX'Base?
end Solid.Debug.Analyzer;
