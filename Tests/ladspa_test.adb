with Ada.Text_IO;
with Solid.Audio.Ladspa.Host;

procedure Ladspa_Test is
   procedure Output (Message : in String) is
   begin -- Output
      Ada.Text_IO.Put_Line (Message);
   end Output;

   procedure Initialize is new Solid.Audio.Ladspa.Host.Initialize (Report_Problem => Output);

   procedure Display (ID        : in Solid.Audio.Ladspa.Plugin_ID;
                      Label     : in String;
                      Name      : in String;
                      Maker     : in String;
                      Copyright : in String)
   is
   begin -- Display
      Ada.Text_IO.Put_Line (Solid.Audio.Ladspa.Plugin_ID'Image (ID) & ": " & Label & " - " & Name);
      Ada.Text_IO.Put_Line (Maker);
   end Display;

   procedure List_Plugins is new Solid.Audio.Ladspa.Host.Iterate (Process => Display);
begin -- Ladspa_Test
   Initialize;

   List_Plugins;
end Ladspa_Test;
