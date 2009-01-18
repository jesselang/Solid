with Ada.Strings.Fixed;
with Solid.Text_Streams;
with Solid.Web.Headers;
with Solid.Web.Messages;

package body Solid.Web.Response.Client is
   function Read (Stream : access Ada.Streams.Root_Stream_Type'Class) return Web.Response.Data is
      Parsed         : Data;
      Text           : Text_Streams.Text_Stream;
      Line           : String (1 .. 256);
      Last           : Natural;
      Payload_Buffer : Ada.Streams.Stream_Element_Array (1 .. 512);
      Payload_Last   : Ada.Streams.Stream_Element_Offset;

      use type Ada.Streams.Stream_Element_Offset;
      use Solid.Strings;
   begin -- Read
      Text_Streams.Create (Stream => Text, From => Stream);

      Status_Line : declare
         Space : Natural;
      begin -- Status_Line
         Text_Streams.Get_Line (Stream => Text, Item => Line, Last => Last);

         if Last = 0 or Line (1 .. 5) /= Messages.HTTP_Version_Token then
            raise Invalid;
         end if;

         -- Ignore the HTTP version.

         Space := Ada.Strings.Fixed.Index (Line (9 .. Last), Pattern => " ");

         if Space = 0 then
            raise Invalid;
         end if;

         Parsed.Code := Messages.Status_Code'Value ('S' & Line (Space + 1 .. Space + 3) );
         Parsed.Reason := +Line (Space + 5 .. Last);
      end Status_Line;

      Parsed.Headers := Web.Headers.Read (Stream => Stream);

      Read_Payload : loop
         Ada.Streams.Read (Stream => Stream.all, Item => Payload_Buffer, Last => Payload_Last);

         exit Read_Payload when Payload_Last = 0;

         Parsed.Payload := Parsed.Payload & (+Text_Streams.To_String (Payload_Buffer (1 .. Payload_Last) ) );
      end loop Read_Payload;

      return Parsed;
   exception
      when Constraint_Error =>
         raise Invalid;
      when Text_Streams.End_Of_Stream =>
         raise Invalid;
   end Read;
end Solid.Web.Response.Client;
