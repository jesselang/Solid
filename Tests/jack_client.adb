with Ada.Text_IO;
with Ada.Calendar;
with Solid.Audio.Jack;
with Jack_Process;

procedure Jack_Client is
   use Solid.Audio;
   use type Ada.Calendar.Time;

   Finished : constant Ada.Calendar.Time := Ada.Calendar.Clock + 10.0;

   Client : Jack.Client;
begin -- Jack_Client
   Jack.Open (Connection => Client, Name => "ada_client");
   Jack.Register (Connection => Client, Input => "input", Output => "output", Process => Jack_Process.Foo'Access);
--   Jack.Register (Connection => Client, Input => "input2", Output => "output2", Process => Jack_Process.Foo'Access);
   Jack.Connect_To_Physical_Ports (Connection => Client);
   --delay 0.01;
   delay until Finished;
   Jack.Close (Connection => Client);
   Ada.Text_IO.Put_Line ("All done.");
end Jack_Client;
