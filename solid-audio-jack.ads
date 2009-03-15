private with Interfaces.C.Extensions;
private with System.Address_To_Access_Conversions;

package Solid.Audio.Jack is
   type Client is limited private;

   Client_Error : exception;
   -- The following operations may raise Client_Error.

   procedure Open (Connection : out Client; Name : in String; Server : in String := "");
   -- Opens a Connection with Name.

   procedure Close (Connection : in out Client);
   -- Closes the Connection.

   type Audio_Processor is access procedure (Input : in Sample_Buffer; Output : out Sample_Buffer);

   function Max_Buffer_Size (Connection : Client) return Solid.Audio.Buffer_Size;
   -- Returns the maximum buffer size that an Audio_Processor should be able to accomodate.

   -- Activate and deactivate are not used.
   -- Client is activated when Register is first used.

   procedure Register (Connection : in out Client; Input : in String; Output : in String; Process : not null Audio_Processor);

   procedure Connect_To_Physical_Ports (Connection : in out Client);
   -- Connects the Connection's ports to Jack's physical ports (sound card).

   procedure Silence (Buffer : out Sample_Buffer);
   -- Writes silence to Buffer.
private -- Solid.Audio.Jack
   type Port_Handle is new Interfaces.C.Extensions.opaque_structure_def_ptr;

   type Audio_Process is record
      Input   : Port_Handle;
      Output  : Port_Handle;
      Process : Audio_Processor;
   end record;

   No_Process : constant Audio_Process := (Input => null, Output => null, Process => null);

   type Two_Channels is array (Positive range 1 .. 2) of Audio_Process;
   -- Dirty hack to get my program working.  This should be a protected vector, reserved to two.

   package Process_Conversion is new System.Address_To_Access_Conversions (Two_Channels);

   subtype Process_Handle is Process_Conversion.Object_Pointer;

   type Client_Handle is new Interfaces.C.Extensions.opaque_structure_def_ptr;

   type Client_State is (Uninitialized, Initialized, Activated, Deactivated);

   type Client is limited record
      Handle    : Client_Handle;
      State     : Client_State   := Uninitialized;
      Processes : Process_Handle := new Two_Channels;
   end record;
end Solid.Audio.Jack;
