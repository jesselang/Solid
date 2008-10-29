with Ada.Text_IO;
with Solid.Audio.Ladspa.Host;

procedure Ladspa_Test is
   procedure Output (Message : in String) is
   begin -- Output
      Ada.Text_IO.Put_Line (Message);
   end Output;

   procedure Display (ID        : in     Solid.Audio.Ladspa.Plugin_ID;
                      Label     : in     String;
                      Name      : in     String;
                      Maker     : in     String;
                      Copyright : in     String;
                      Continue  : in out Boolean)
   is
   begin -- Display
      Ada.Text_IO.Put_Line (Solid.Audio.Ladspa.Plugin_ID'Image (ID) & ": " & Label & " - " & Name);
      Ada.Text_IO.Put_Line (Maker);
   end Display;

   procedure List_Plugins is new Solid.Audio.Ladspa.Host.Available_Plugins (Process => Display);

   procedure Display_Warnings is new Solid.Audio.Ladspa.Host.Warnings (Process => Output);

   Warnings : Boolean := True;
   Plugin   : Solid.Audio.Ladspa.Host.Plugin;
begin -- Ladspa_Test
   Solid.Audio.Ladspa.Host.Initialize (Warnings => Warnings);

   Display_Warnings;

   --List_Plugins;

   Solid.Audio.Ladspa.Host.Create (Plugin, Rate => 48_000, ID => 1794);

   declare
      Ports : Solid.Audio.Ladspa.Host.Port_Array := Solid.Audio.Ladspa.Host.Ports (Plugin);
   begin
      All_Ports : for Index in Ports'Range loop
         Ada.Text_IO.Put (Item => Positive'Image (Index) & ": " & Solid.Audio.Ladspa.Host.Name (Ports (Index).all) & " - ");

         if Ports (Index).all in Solid.Audio.Ladspa.Host.Control_Port'Class then
            Ada.Text_IO.Put ("Control ");
         else
            Ada.Text_IO.Put ("Audio ");
         end if;

         Ada.Text_IO.Put_Line
            (Item => Solid.Audio.Ladspa.Host.Port_Direction'Image (Solid.Audio.Ladspa.Host.Direction (Ports (Index).all) ) );
      end loop All_Ports;
   end;
end Ladspa_Test;
