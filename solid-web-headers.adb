with Ada.Strings.Fixed;
with Solid.Strings;
with Solid.Text_Streams;
with Solid.Web.Containers.Tables;

package body Solid.Web.Headers is
   function Read (Stream : access Ada.Streams.Root_Stream_Type'Class) return List is
      Headers   : List;
      Text      : Text_Streams.Text_Stream;
      Line      : String (1 .. 256);
      Last      : Natural;
      Delimiter : Natural;

      use Ada.Strings;
   begin -- Read
      Text_Streams.Create (Stream => Text, From => Stream);

      Each_Header : loop
         Text_Streams.Get_Line (Stream => Text, Item => Line, Last => Last);

         exit Each_Header when Last = 0;

         Delimiter := Fixed.Index (Line (1 .. Last), Pattern => ":");

         if Delimiter /= 0 then
            Headers.Add (Name  => Fixed.Trim (Line (1             .. Delimiter - 1), Side => Both),
                         Value => Fixed.Trim (Line (Delimiter + 1 .. Last),          Side => Both) );
         end if;
      end loop Each_Header;

      return Headers;
   end Read;

   procedure Write (Headers : in List; Stream : access Ada.Streams.Root_Stream_Type'Class) is
      procedure Write_Header (Name : in String; Values : in Strings.String_Array; Continue : in out Boolean);

      procedure Write_Headers is new Solid.Web.Containers.Tables.Iterate (Process => Write_Header);

      Text : Text_Streams.Text_Stream;

      procedure Write_Header (Name : in String; Values : in Strings.String_Array; Continue : in out Boolean) is
         use Solid.Strings;
      begin -- Write_Header
         Text_Streams.Put_Line (Stream => Text, Item => Name & ": " & (+Values (1) ) );
      end Write_Header;
   begin -- Write
      Text_Streams.Create (Stream => Text, From => Stream, Line_Ending => Text_Streams.CR_LF);
      Write_Headers (Container => Headers);
   end Write;
end Solid.Web.Headers;
