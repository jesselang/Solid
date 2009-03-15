package Solid.Audio.Buffers is
   Overflow : exception;

   type Buffer_End is (Front, Back);

   generic -- Ring_Buffers
      type Buffer_Index is mod <>;
   package Ring_Buffers is
      type Buffer_Data is array (Buffer_Index) of Sample;
      type Handle is access Buffer_Data;

      protected type Buffer is
         procedure Append (Item : in Sample_Buffer);
         -- Append Item to the buffer.
         -- Raises Overflow if Item is larger than the buffer.

         function Length return Buffer_Size;
         -- Returns the length of the buffer.

         entry Get (Item : out Sample_Buffer; From : in Buffer_End := Back);
         -- Gets data from either the front or the back of the buffer.
         -- Blocks until there is enough data to fill Item.
         -- Raises Overflow if Item is larger than the buffer.
      private -- Buffer
         entry Wait (Item : out Sample_Buffer; From : in Buffer_End);

         Data    : Handle       := new Buffer_Data;
         First   : Buffer_Index := Buffer_Index'First;
         Last    : Buffer_Index := Buffer_Index'First;
         Full    : Boolean      := False;
         Waiting : Boolean      := False;
      end Buffer;
   end Ring_Buffers;
end Solid.Audio.Buffers;
