package Solid.Net.IAX is
   type Call_Number is mod 2 ** 15;
   for Call_Number'Size use 15;

   Unknown_Call_Number : constant Call_Number := 0;

   type Full_Timestamp is mod 2 ** 32;
   for Full_Timestamp'Size use 32;

   Empty_Full_Timestamp : constant Full_Timestamp := 0;

   type Mini_Timestamp is mod 2 ** 16;
   for Mini_Timestamp'Size use 16;

   Empty_Mini_Timestamp : constant Mini_Timestamp := 0;

   type Sequence_Number is mod 2 ** 8;
   for Sequence_Number'Size use 8;

   Empty_Sequence_Number : constant Sequence_Number := 0;

   type Frame_Type is
      (DTMF,
       Voice,
       Video,
       Control,
       Null_Frame,
       IAX_Control,
       Text,
       Image,
       HTML);

   -- Modify this to an array.
   --~ for Frame_Type_Type use
      --~ (DTMF        => 16#01#,
       --~ Voice       => 16#02#,
       --~ Video       => 16#03#,
       --~ Control     => 16#04#,
       --~ Null_Frame  => 16#05#,
       --~ IAX_Control => 16#06#,
       --~ Text        => 16#07#,
       --~ Image       => 16#08#,
       --~ HTML        => 16#09#);

   for Frame_Type'Size use 8;

   -- Skeleton type for subclasses.
   type Subclass is mod 2 ** 7;
   for Subclass'Size use 7;

   Empty_Subclass : constant Subclass := 0;

   -- Types for each subclass.
   -- DTMF type.
   type DTMF_Subclass is
      ('0',
       '1',
       '2',
       '3',
       '4',
       '5',
       '6',
       '7',
       '8',
       '9',
       'A',
       'B',
       'C',
       'D',
       '*',
       '#');

   type Voice_Subclass is
      (G_723_1,
       GSM,
       G_711u, -- Mu-law
       G_711a, -- A-law
       MP3,    -- Deprecated
       IMA_ADPCM,
       b16_le, -- 16-bit linear little-endian
       LPC10,
       G_729,
       Speex,
       ILBC);

   -- Use array.
   --~ for Voice_Subclass use
      --~ (G_723_1   => 16#0001#,
       --~ GSM       => 16#0002#,
       --~ G_711u    => 16#0004#,
       --~ G_711a    => 16#0008#,
       --~ MP3       => 16#0010#,
       --~ IMA_ADPCM => 16#0020#,
       --~ b16_le    => 16#0040#,
       --~ LPC10     => 16#0080#,
       --~ G_729     => 16#0100#,
       --~ Speex     => 16#0200#,
       --~ ILBC      => 16#0400#);

   type Video_Subclass is
      (JPEG,
       PNG,
       H_261,
       H_263);

   -- Use an array.
   --~ for Video_Subclass_Type use
      --~ (JPEG  => 16#10000#,
       --~ PNG   => 16#20000#,
       --~ H_261 => 16#40000#,
       --~ H_263 => 16#80000#);

   type Control_Frame_Subclass is
      (Hangup,
       Ring,
       Ringing,
       Answer,
       Busy,
       Congestion,
       Flash,
       Wink,
       Option,
       Key_Radio,
       Unkey_Radio,
       Progress);

   -- Use an array.
   --~ for Control_Frame_Subclass_Type use
      --~ (Hangup      => 16#01#,
       --~ Ring        => 16#02#,
       --~ Ringing     => 16#03#,
       --~ Answer      => 16#04#,
       --~ Busy        => 16#05#,
       --~ Congestion  => 16#08#,
       --~ Flash       => 16#09#,
       --~ Wink        => 16#0a#,
       --~ Option      => 16#0b#,
       --~ Key_Radio   => 16#0c#,
       --~ Unkey_Radio => 16#0d#,
       --~ Progress    => 16#0e#);

   Poke_Request : constant Subclass_Type := 16#1e#;

   type Bit_Array is array (Positive range <>) of Boolean;
   pragma Pack (Bit_Array);

   type Frame_Kind is (Full, Mini);

   type Frame (Kind : Frame_Kind) is record
      F_Bit    : Boolean             := Kind = Full; -- True if full frame.
      Source : Call_Number    := Unknown_Call_Number;
      Data               : Bit_Array (1 .. 96) := (others => False);

      case Kind is
         when Full =>
            R_Bit          : Boolean              := False;
            Destination  : Call_Number     := Unknown_Call_Number;
            Timestamp           : Full_Timestamp  := Empty_Full_Timestamp;
            Outbound : Sequence_Number := Empty_Sequence_Number;
            Inbound  : Sequence_Number := Empty_Sequence_Number;
            Class               : Frame_Class      := Null_Frame;
            C_Bit    : Boolean              := False;
            Subclass                 : Subclass        := Empty_Subclass;
         when Mini =>
            Mini_Timestamp : Mini_Timestamp := Empty_Mini_Timestamp;
      end case;
   end record;

   Word : constant := 4;

   for IAX_Frame use record
      Full_Frame_Flag          at 0 * Word range  0 .. 0;
      Source_Call_Number       at 0 * Word range  1 .. 15;
      Retransmit_Flag          at 0 * Word range 16 .. 16;
      Destination_Call_Number  at 0 * Word range 17 .. 31;
      Full_Timestamp           at 1 * Word range  0 .. 31;
      Outbound_Sequence_Number at 2 * Word range  0 .. 7;
      Inbound_Sequence_Number  at 2 * Word range  8 .. 15;
      Frame_Type               at 2 * Word range 16 .. 23;
      Subclass_Value_Format    at 2 * Word range 24 .. 24;
      Subclass                 at 2 * Word range 25 .. 31;
      Mini_Timestamp           at 0 * Word range 16 .. 31;
      Data                     at 3 * Word range  0 .. 95;
   end record;

   pragma Pack (IAX_Frame);
end Solid.Net.IAX;
