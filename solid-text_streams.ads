                 --~ Right.  You said it before, low-bandwidth, unbuf
--~ fered Get_Line operation directly on the stream.
--~ Apr 01 13:01:26 <caracal>       yep
--~ Apr 01 13:01:27 <jesselang>     If I had a Get_Character, unbuffered Get_Line, a
--~ nd maybe a Put_Line, you wouldn't have had to roll your own.

with Ada.Streams;

package Solid.Text_Streams is
   End_Of_Stream : exception;
   -- End_Of_Stream is raised whenever an operation on a Text_Stream fails
   -- because the stream has ended.

   type Text_Stream is limited private;

   type Line_Terminator is (CR, LF, CR_LF);

   procedure Create (Stream      :    out Text_Stream;
                     From        : access Ada.Streams.Root_Stream_Type'Class;
                     Line_Ending : in     Line_Terminator := CR_LF);
   -- Creates a new Text_Stream in Stream, using the stream in From.

   procedure Skip_Line (Stream : in out Text_Stream; Spacing : in Positive := 1);
   -- Skips Spacing number of line terminator(s) read from Stream,
   -- and any characters in between.
   -- Raises End_Of_Stream when the end of Stream has been reached.

   procedure Get (Stream : in out Text_Stream; Item : out Character);
   -- After skipping any line terminators, reads the next character from Stream into Item.
   -- Raises End_Of_Stream when the end of Stream has been reached.

   procedure Get (Stream : in out Text_Stream; Item : out String; Last : out Natural);
   -- After skipping any line terminators, reads Item from Stream.
   -- Blocks until Item is full, or the stream ends.
   -- Sets Last to the last character in Item read from Stream.
   -- If Last < Item'Last, the stream has ended.
   -- Raises End_Of_Stream when the end of Stream has been reached.

   procedure Get_Line (Stream : in out Text_Stream; Item : out String; Last : out Natural);
   -- Reads Item from Stream.  Returns when Item is full, when a line terminator is reached,
   -- or when the end of Stream has been reached.  When Item is full, Last is set to
   -- Item'Last.  When a line terminator is reached, Last is set to the last character of
   -- Item before the line terminator.  When the end of Stream has been reached, Last is set
   -- to the last character read.
   -- Raises End_Of_Stream when the end of Stream has been reached without reading any data.

   procedure Put (Stream : in out Text_Stream; Item : in Character);
   -- Puts Item to Stream.

   procedure Put (Stream : in out Text_Stream; Item : in String);
   -- Puts Item to Stream.

   procedure New_Line (Stream : in out Text_Stream; Spacing : in Positive := 1);
   -- Puts Spacing number of line terminator(s) to Stream.

   procedure Put_Line (Stream : in out Text_Stream; Item : in String);
   -- Puts Item to Stream, followed by a line terminator.

   -- Unbuffered operations.
   procedure Get (Stream : access Ada.Streams.Root_Stream_Type'Class; Item : out Character);
   -- After skipping any line terminators, reads the next character from Stream into Item.
   -- Raises End_Of_Stream when the end of Stream has been reached.

   procedure Get (Stream : access Ada.Streams.Root_Stream_Type'Class; Item : out String; Last : out Natural);
   -- After skipping any line terminators, reads Item from Stream.
   -- Blocks until Item is full, or the stream ends.
   -- Sets Last to the last character in Item read from Stream.
   -- If Last < Item'Last, the stream has ended.
   -- Raises End_Of_Stream when the end of Stream has been reached.

   procedure Get_Line (Stream : access Ada.Streams.Root_Stream_Type'Class; Item : out String; Last : out Natural);
   -- Reads Item from Stream.  Returns when Item is full, when a line terminator is reached,
   -- or when the end of Stream has been reached.  When Item is full, Last is set to
   -- Item'Last.  When a line terminator is reached, Last is set to the last character of
   -- Item before the line terminator.  When the end of Stream has been reached, Last is set
   -- to the last character read.
   -- Raises End_Of_Stream when the end of Stream has been reached without reading any data.

   -- Still to be added:
   --~ procedure Put (Stream : in out Text_Stream; Item : in Character);
   -- Puts Item to Stream.

   --~ procedure Put (Stream : in out Text_Stream; Item : in String);
   -- Puts Item to Stream.

   --~ procedure New_Line (Stream : in out Text_Stream; Spacing : in Positive := 1);
   -- Puts Spacing number of line terminator(s) to Stream.

   --~ procedure Put_Line (Stream : in out Text_Stream; Item : in String);
   -- Puts Item to Stream, followed by a line terminator.

   -- Conversion functions.
   function To_String (Item : Ada.Streams.Stream_Element_Array) return String;
   function To_Stream (Item : String) return Ada.Streams.Stream_Element_Array;
private -- Solid.Text_Streams
   type Text_Stream is limited record
      Stream             : access Ada.Streams.Root_Stream_Type'Class;
      Line_Ending        : Line_Terminator;
      First_Element      : Ada.Streams.Stream_Element;
      Read_First_Element : Boolean := False;
   end record;
end Solid.Text_Streams;
