with Ada.Text_IO.Text_Streams;
with Solid.CGI.Containers.Tables;
with Solid.CGI.Headers;
with Solid.Strings;
with Solid.Text_Streams;

use Solid.Strings;

procedure Solid.CGI.Program is
   Input  : Text_Streams.Text_Stream;
   Output : Text_Streams.Text_Stream;

   procedure Connect_Streams is
   begin -- Connect_Streams
      Text_Streams.Create (Stream      => Input,
                           From        => Ada.Text_IO.Text_Streams.Stream (Ada.Text_IO.Current_Input),
                           Line_Ending => Text_Streams.CR_LF);
      Text_Streams.Create (Stream      => Output,
                           From        => Ada.Text_IO.Text_Streams.Stream (Ada.Text_IO.Current_Output),
                           Line_Ending => Text_Streams.CR_LF);
   end Connect_Streams;

   procedure Read_Request (Object : out Request.Data) is
      use Text_Streams;
   begin -- Read_Request
      null;
   end Read_Request;

   procedure Write_Response (Object : in Response.Data) is
      Name_Separator  : constant String := ": ";
      Value_Separator : constant String := "; ";

      use Text_Streams;

      procedure Write_Header (Name : in String; Values : in String_Array) is
      begin -- Write_Header
         Put (Stream => Output, Item => Name);
         Put (Stream => Output, Item => Name_Separator);

         for Index in Values'Range loop
            Put (Stream => Output, Item => -Values (Index) );

            exit when Index = Values'Last;

            Put (Stream => Output, Item => Value_Separator);
         end loop;

         New_Line (Stream => Output);
      end Write_Header;

      Response_Headers : constant CGI.Headers.List := Response.Headers (Object);

      procedure Write_Headers is new Containers.Tables.Iterate (Process => Write_Header);
   begin -- Write_Response
      Write_Headers (Containers.Tables.Table (Response.Headers (Object) ) );
      New_Line (Stream => Output);
      Put (Stream => Output, Item => Response.Payload (Object) );
   end Write_Response;

   Program_Request  : Request.Data;
   Program_Response : Response.Data;
begin -- Solid.CGI.Program
   Connect_Streams;
   Read_Request (Object => Program_Request);
   Program_Response := Process (Program_Request);
   Write_Response (Object => Program_Response);
end Solid.CGI.Program;
