with Ada.Streams;
with Ada.Text_IO.Text_Streams;
with Solid.CGI.Containers.Tables;
with Solid.CGI.Environment;
with Solid.CGI.Headers;
with Solid.CGI.Standard.Environment;
with Solid.CGI.Request.Set;
with Solid.Strings;
with Solid.Text_Streams;

use Solid.Strings;

package body Solid.CGI.Standard is
   procedure Program is
      Invalid_Gateway : exception;

      Input_Stream  : constant Ada.Text_IO.Text_Streams.Stream_Access := Ada.Text_IO.Text_Streams.Stream (Ada.Text_IO.Current_Input);
      Output_Stream : constant Ada.Text_IO.Text_Streams.Stream_Access := Ada.Text_IO.Text_Streams.Stream (Ada.Text_IO.Current_Output);
      Input  : Text_Streams.Text_Stream;
      Output : Text_Streams.Text_Stream;

      procedure Connect_Streams is
      begin -- Connect_Streams
         Text_Streams.Create (Stream      => Input,
                              From        => Input_Stream,
                              Line_Ending => Text_Streams.CR_LF);
         Text_Streams.Create (Stream      => Output,
                              From        => Output_Stream,
                              Line_Ending => Text_Streams.CR_LF);
      end Connect_Streams;

      procedure Read_Request (Object : in out Request.Data) is
         use Text_Streams;
      begin -- Read_Request
         if CGI.Environment.Value (Environment.Current, Name => CGI.Environment.Gateway_Interface) /= "CGI/1.1" then
            raise Invalid_Gateway;
         end if;

         Request.Set.Environment (Object => Object, Environment => Environment.Current);
         --~ Request.Set.Method (Object => Object,
                             --~ Method => Request.Request_Method'Value (Environment.Value (Environment.Current.all, Name => Environment.Request_Method) ) );
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
         Ada.Streams.Write (Stream => Output_Stream.all, Item => Response.Payload (Object) );
      end Write_Response;

      Program_Request  : Request.Data;
      Program_Response : Response.Data;
   begin -- Program
      Connect_Streams;
      Read_Request (Object => Program_Request);
      Program_Response := Process (Program_Request);
      Write_Response (Object => Program_Response);
   exception -- Program
      when Invalid_Gateway =>
         Ada.Text_IO.Put_Line (File => Ada.Text_IO.Current_Error,
                               Item => "Error - it appears I'm not being called with a valid gateway interface.");
   end Program;

end Solid.CGI.Standard;
