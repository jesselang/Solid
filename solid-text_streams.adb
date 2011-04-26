package body Solid.Text_Streams is
   Stream_CR : constant := Character'Pos (ASCII.CR);
   Stream_LF : constant := Character'Pos (ASCII.LF);
   -- These constants are used for parsing lines from the Text_Stream, in Is_Line_Ending and in Skip_Full_Terminator.

   function Line_Ending (Terminator : Line_Terminator) return String is
   begin -- Line_Ending
      case Terminator is
         when CR =>
            return "" & ASCII.CR;
         when LF =>
            return "" & ASCII.LF;
         when CR_LF =>
            return ASCII.CR & ASCII.LF;
      end case;
   end Line_Ending;
   pragma Inline (Line_Ending);
   -- Line_Ending returns the line termination characters to be output to the Text_Stream
   -- based on the setting when Text_Stream is created.

   function To_Character (Item : Ada.Streams.Stream_Element) return Character is
   begin -- To_Character
      return Character'Val (Item);
   end To_Character;
   pragma Inline (To_Character);
   -- Converts Item from a Stream_Element to a Character.

   function To_String (Item : Ada.Streams.Stream_Element_Array) return String is
      Result : String (Positive (Item'First) .. Natural (Item'Last) );
   begin -- To_String
      for Index in Result'Range loop
         Result (Index) := To_Character (Item (Ada.Streams.Stream_Element_Offset (Index) ) );
      end loop;

      return Result;
   end To_String;
   pragma Inline (To_String);
   -- Converts Item from a Stream_Element_Array to a String.

   function Is_Line_Ending (Item : Ada.Streams.Stream_Element) return Boolean is
      use type Ada.Streams.Stream_Element;
   begin -- Is_Line_Ending
      return Item = Stream_CR or Item = Stream_LF;
   end Is_Line_Ending;
   pragma Inline (Is_Line_Ending);
   -- Returns True if Item is considered a line terminating character, otherwise False.

   procedure Read_Element (Stream : access Ada.Streams.Root_Stream_Type'Class; Item : out Ada.Streams.Stream_Element) is
      Buffer : Ada.Streams.Stream_Element_Array (1 .. 1);
      Last   : Ada.Streams.Stream_Element_Offset;

      use type Ada.Streams.Stream_Element_Offset;
   begin -- Read_Element
      Ada.Streams.Read (Stream => Stream.all, Item => Buffer, Last => Last);

      if Last = 0 then
         raise End_Of_Stream;
      end if;

      Item := Buffer (1);
   exception -- Read_Element
      when Constraint_Error =>
         raise End_Of_Stream;
   end Read_Element;
   -- Reads one Stream_Element from Stream to Item.
   -- Raises End_Of_Stream if the stream has ended.

   procedure Skip_Line_Terminators (Stream : in out Text_Stream) is
   begin -- Skip_Line_Terminators
      if not Stream.Read_First_Element or else Is_Line_Ending (Stream.First_Element) then
         loop
            Read_Element (Stream => Stream.Stream, Item => Stream.First_Element);

            exit when not Is_Line_Ending (Stream.First_Element);
         end loop;

         Stream.Read_First_Element := True;
      end if;
   end Skip_Line_Terminators;
   pragma Inline (Skip_Line_Terminators);
   -- Reads elements from Stream until something other than a line terminator is read.
   -- Sets Stream.First_Element to that element and sets Stream.Read_First_Element to True.
   -- Raises End_Of_Stream if the stream has ended.

   procedure Get_First_Element (Stream : in out Text_Stream;
                                Buffer :    out Ada.Streams.Stream_Element_Array;
                                Last   :    out Ada.Streams.Stream_Element_Offset);
   -- Puts the cached first element of Stream into Buffer, sets Last to Buffer'First.
   pragma Inline (Get_First_Element);

   procedure Get_First_Element (Stream : in out Text_Stream;
                                Buffer :    out Ada.Streams.Stream_Element_Array;
                                Last   :    out Ada.Streams.Stream_Element_Offset)
   is
   begin -- Get_First_Element
      if Stream.Read_First_Element then
         Buffer (Buffer'First)     := Stream.First_Element;
         Last                      := Buffer'First;
         Stream.Read_First_Element := False;
      end if;
   end Get_First_Element;

   procedure Read (Stream : in out Text_Stream; Item : out String; Last : out Natural) is
      use Ada.Streams;

      Buffer      : Stream_Element_Array (Stream_Element_Offset (Item'First) .. Stream_Element_Offset (Item'Last) );
      Buffer_Last : Stream_Element_Offset;
   begin -- Read
      Skip_Line_Terminators (Stream => Stream);
      -- Stream.Read_First_Element will always be True, and Stream.First_Element should always be a non-terminator.

      if Item'Length = 0 then
         Last := Item'First - 1;

         return;
      end if;

      Get_First_Element (Stream => Stream, Buffer => Buffer, Last => Buffer_Last);

      if Buffer'Length > 1 then
         Read (Stream => Stream.Stream.all, Item => Buffer (Buffer'First + 1 .. Buffer'Last), Last => Buffer_Last);
      end if;

      Item (Item'First .. Natural (Buffer_Last) ):= To_String (Buffer (Buffer'First .. Buffer_Last) );
      Last := Natural (Buffer_Last);
   exception -- Read
      when Constraint_Error =>
         raise End_Of_Stream;
   end Read;
   -- After skipping any line terminators, reads from Stream until Item is full or until the stream has ended.
   -- Sets Item to the characters read, sets Last to the last character assigned in Item.
   -- If Last < Item'Last, the stream has ended.
   -- Raises End_Of_Stream if the stream has ended.

   procedure Skip_Full_Terminator (Stream : in out Text_Stream) is
      use type Ada.Streams.Stream_Element;
   begin -- Skip_Full_Terminator
      case Stream.First_Element is
         when Stream_CR =>
            Read_Element (Stream => Stream.Stream, Item => Stream.First_Element);

            Stream.Read_First_Element := Stream.First_Element /= Stream_LF;
         when others =>
            Stream.Read_First_Element := False;
      end case;
   end Skip_Full_Terminator;
   pragma Inline (Skip_Full_Terminator);
   -- Intelligently reads the full line terminator from Stream.
   -- Assumes that Stream.First_Element contains a line termination element.
   -- Raises End_Of_Stream if the stream has ended.

   procedure Find_Line_Terminator (Stream : in out Text_Stream) is
   begin -- Find_Line_Terminator
      if not Stream.Read_First_Element or else not Is_Line_Ending (Stream.First_Element) then
         loop
            Read_Element (Stream => Stream.Stream, Item => Stream.First_Element);

            exit when Is_Line_Ending (Stream.First_Element);
         end loop;
      end if;

      Skip_Full_Terminator (Stream => Stream);
   end Find_Line_Terminator;
   pragma Inline (Find_Line_Terminator);
   -- Reads from Stream until a line terminator is found.  All elements up to and including the terminator are discarded.
   -- Raises End_Of_Stream if the stream has ended.

   procedure Read_Line (Stream : in out Text_Stream; Item : out String; Last : out Natural) is
      use Ada.Streams;

      Buffer      : Stream_Element_Array (Stream_Element_Offset (Item'First) .. Stream_Element_Offset (Item'Last) );
      Buffer_Last : Stream_Element_Offset := Buffer'First - 1;
   begin -- Read_Line
      if Item'Length = 0 then
         Last := Item'First - 1;

         return;
      elsif Stream.Read_First_Element then
         if Is_Line_Ending (Stream.First_Element) then
            Stream.Read_First_Element := False;
            Last := Item'First - 1;
            Skip_Full_Terminator (Stream => Stream);

            return;
         else
            Get_First_Element (Stream => Stream, Buffer => Buffer, Last => Buffer_Last);

            if Item'Length = 1 then
               Item (Item'First) := To_Character (Buffer (Buffer_Last) );
               Last              := Natural (Buffer_Last);

               return;
            end if;
         end if;
      end if;

      -- for I in Buffer_Last + 1 .. Buffer'Last loop
      loop
         begin
            Read_Element (Stream => Stream.Stream, Item => Stream.First_Element);
         exception
            when End_Of_Stream =>
               if Buffer_Last = Buffer'First - 1 then
                  raise;
               else
                  exit;
               end if;
         end;

         if Is_Line_Ending (Stream.First_Element) then
            Skip_Full_Terminator (Stream => Stream);

            exit;
         end if;

         Buffer_Last          := Buffer_Last + 1;
         Buffer (Buffer_Last) := Stream.First_Element;

         exit when Buffer_Last = Buffer'Last;
      end loop;

      Item (Item'First .. Natural (Buffer_Last) ):= To_String (Buffer (Buffer'First .. Buffer_Last) );
      Last := Natural (Buffer_Last);
   end Read_Line;
   -- Reads from Stream until Item is full or a line terminator is found, the line terminator is skipped.
   -- Sets Item to the characters read, sets Last to the last character assigned to Item.
   -- Raises End_Of_Stream if the stream has ended.

   procedure Create (Stream      :    out Text_Stream;
                     From        : access Ada.Streams.Root_Stream_Type'Class;
                     Line_Ending : in     Line_Terminator := CR_LF)
   is
   begin -- Create
      Stream.Stream      := From;
      Stream.Line_Ending := Line_Ending;
   end Create;

   procedure Skip_Line (Stream : in out Text_Stream; Spacing : in Positive := 1) is
   begin -- Skip_Line
      for Index in 1 .. Spacing loop
         Find_Line_Terminator (Stream => Stream);
      end loop;
   end Skip_Line;

   procedure Get (Stream : in out Text_Stream; Item : out Character) is
      Buffer : String (1 .. 1);
      Last   : Natural;
   begin -- Get
      Read (Stream => Stream, Item => Buffer, Last => Last);

      if Last = 0 then
         raise End_Of_Stream;
      end if;

      Item := Buffer (1);
   end Get;

   procedure Get (Stream : in out Text_Stream; Item : out String; Last : out Natural) is
   begin -- Get
      Read (Stream => Stream, Item => Item, Last => Last);
   end Get;

   procedure Get_Line (Stream : in out Text_Stream; Item : out String; Last : out Natural) is
   begin -- Get_Line
      Read_Line (Stream => Stream, Item => Item, Last => Last);
   end Get_Line;

   procedure Put (Stream : in out Text_Stream; Item : in Character) is
   begin -- Put
      Put (Stream => Stream, Item => (1 => Item) );
   end Put;

   procedure Put (Stream : in out Text_Stream; Item : in String) is
      use Ada.Streams;
   begin -- Put
      Write (Stream => Stream.Stream.all, Item => To_Stream (Item) );
   end Put;

   procedure New_Line (Stream : in out Text_Stream; Spacing : in Positive := 1) is
      function "*" (Left : String; Right : Positive) return String is
         Result : String (1 .. Left'Length * Right);
      begin -- "*"
         for Index in 1 .. Right loop
            Result ( (Index - 1) * Left'Length + 1 .. (Index) * Left'Length) := Left;
         end loop;

         return Result;
      end "*";
   begin -- New_Line
      Put (Stream => Stream, Item => Line_Ending (Stream.Line_Ending) * Spacing);
   end New_Line;

   procedure Put_Line (Stream : in out Text_Stream; Item : in String) is
   begin -- Put_Line
      Put (Stream => Stream, Item => Item);
      New_Line (Stream => Stream);
   end Put_Line;

   -- Unbuffered operaitons on raw streams.
   -- I think these will likely cause the special Text_Stream and its associated operations to be deprecated.

   procedure Get (Stream : access Ada.Streams.Root_Stream_Type'Class; Item : out Ada.Streams.Stream_Element);
   -- After skipping any line terminators, reads the next character from Stream into Item.
   -- Raises End_Of_Stream when the end of Stream has been reached.

   procedure Get (Stream : access Ada.Streams.Root_Stream_Type'Class; Item : out Ada.Streams.Stream_Element) is
      Buffer : Ada.Streams.Stream_Element;
   begin -- Get
      Skip_Line_Terminators : loop
         Read_Element (Stream => Stream, Item => Buffer);

         exit when not Is_Line_Ending (Buffer);
      end loop Skip_Line_Terminators;

      Item := Buffer;
   end Get;


   procedure Get (Stream : access Ada.Streams.Root_Stream_Type'Class; Item : out Character) is
      Buffer : Ada.Streams.Stream_Element;
   begin -- Get
      Get (Stream => Stream, Item => Buffer);

      Item := To_Character (Buffer);
   end Get;

   procedure Get (Stream : access Ada.Streams.Root_Stream_Type'Class; Item : out String; Last : out Natural) is
      Buffer : Ada.Streams.Stream_Element_Array
                  (Ada.Streams.Stream_Element_Offset (Item'First) .. Ada.Streams.Stream_Element_Offset (Item'Last) );

      Buffer_Last : Ada.Streams.Stream_Element_Offset;
   begin -- Get
      if Item'Length <= 0 then
         Last := Item'First - 1;

         return;
      end if;

      Get (Stream => Stream, Item => Buffer (Buffer'First) );
      Buffer_Last := Buffer'First;

      if Buffer'Length > 1 then
         Ada.Streams.Read (Stream => Stream.all, Item => Buffer (Buffer'First .. Buffer'Last), Last => Buffer_Last);
      end if;

      Item (Item'First .. Natural (Buffer_Last) ):= To_String (Buffer (Buffer'First .. Buffer_Last) );
      Last := Natural (Buffer_Last);
   exception -- Read
      when Constraint_Error =>
         raise End_Of_Stream;
   end Get;

   procedure Get_Line (Stream : access Ada.Streams.Root_Stream_Type'Class; Item : out String; Last : out Natural) is
      Buffer : Ada.Streams.Stream_Element_Array
                  (Ada.Streams.Stream_Element_Offset (Item'First) .. Ada.Streams.Stream_Element_Offset (Item'Last) );

      Buffer_Last : Ada.Streams.Stream_Element_Offset;

      use type Ada.Streams.Stream_Element_Offset;
   begin -- Get_Line
      if Item'Length <= 0 then
         Last := Item'First - 1;

         return;
      end if;

      Get (Stream => Stream, Item => Buffer (Buffer'First) );
      Buffer_Last := Buffer'First;

      if Buffer'Length > 1 then
         Find_Line_Terminators : for Index in Buffer'First + 1 .. Buffer'Last loop
            Read_Element (Stream => Stream, Item => Buffer (Index) );

            exit when Is_Line_Ending (Buffer (Index) );

            Buffer_Last := Index;
         end loop Find_Line_Terminators;
      end if;

      Item (Item'First .. Natural (Buffer_Last) ):= To_String (Buffer (Buffer'First .. Buffer_Last) );
      Last := Natural (Buffer_Last);
   end Get_Line;

   function To_Stream (Item : String) return Ada.Streams.Stream_Element_Array is
      Result : Ada.Streams.Stream_Element_Array
                  (Ada.Streams.Stream_Element_Offset (Item'First) .. Ada.Streams.Stream_Element_Offset (Item'Last) );
   begin -- To_Stream
      for Index in Result'Range loop
         Result (Index) := Character'Pos (Item (Positive (Index) ) );
      end loop;

      return Result;
   end To_Stream;
end Solid.Text_Streams;
