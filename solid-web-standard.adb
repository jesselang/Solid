with Ada.Exceptions;
with Ada.Streams;
with Ada.Text_IO.Text_Streams;
with GNAT.String_Split;
with Solid.Web.Cookies;
with Solid.Web.Containers.Tables;
-- with Solid.Web.Environment;
with Solid.Web.Headers;
with Solid.Web.MIME;
with Solid.Web.Parameters;
with Solid.Web.Request.Set;
with Solid.Web.Standard.Environment;
with Solid.Strings;
with Solid.Text_Streams;

use Solid.Strings;

package body Solid.Web.Standard is
   procedure Program is
      Invalid_Post    : exception;
      -- Raised if a post request sanity check fails.

      Input_Stream  : constant Ada.Text_IO.Text_Streams.Stream_Access :=
                                                                  Ada.Text_IO.Text_Streams.Stream (Ada.Text_IO.Current_Input);
      Output_Stream : constant Ada.Text_IO.Text_Streams.Stream_Access :=
                                                                  Ada.Text_IO.Text_Streams.Stream (Ada.Text_IO.Current_Output);
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
         Post_Query      : String (1 .. Positive (Payload_Read) );
         Post_Query_Last : Natural;

         Payload_Buffer  : Ada.Streams.Stream_Element_Array (1 .. Ada.Streams.Stream_Element_Offset (Payload_Read) );
         Payload_Last    : Ada.Streams.Stream_Element_Offset;

         use type Request.Request_Method;
         use type Request.Count;
         use type Ada.Streams.Stream_Element_Offset;

         use Text_Streams;
      begin -- Read_Request
         if Web.Environment.Value (Environment.Current, Name => Web.Environment.Gateway_Interface) /= "CGI/1.1" then
            raise Invalid_Gateway with "gateway sanity check failed.";
         end if;

         Request.Set.Environment (Object => Object, Environment => Environment.Current);

         if Request.Method (Object) = Request.Get then
            -- Parameters could be parsed and set upon request.
            Request.Set.Parameters (Object     => Object,
                                    Parameters => Web.Parameters.Parse_URL_Encoding (Request.Query (Object => Object) ) );
         else
            if Request.Content_Type (Object) = MIME.Application_Form_Data then
               if Request.Content_Length (Object) > 0 then
                  Text_Streams.Get_Line (Stream => Input, Item => Post_Query, Last => Post_Query_Last);

                  if Request.Content_Length (Object) /= Request.Count (Post_Query_Last) then
                     raise Invalid_Post with "content length validation failed.";
                  end if;

                  -- Is post query needed?
                  Request.Set.Post_Query (Object => Object, Post_Query => Post_Query (1 .. Post_Query_Last) );
                  -- Parameters could be parsed and set upon request.
                  Request.Set.Parameters
                     (Object     => Object,
                      Parameters => Web.Parameters.Parse_URL_Encoding (Request.Query (Object => Object) & '&' &
                                                                       Post_Query (1 .. Post_Query_Last) ) );
               end if;
            else
               raise Invalid_Post with "unhandled content type: " & Request.Content_Type (Object) & '.';
            end if;
         end if;

         -- It would be nice to parse cookies upon request.
         Parse_Cookies : declare
            Cookie_String        : constant String := Web.Environment.Value (Environment.Current,
                                                                             Name => Web.Environment.HTTP_Cookie);

            Cookie_Set : GNAT.String_Split.Slice_Set;
            Name_Value : GNAT.String_Split.Slice_Set;
            Cookies    : Web.Cookies.List;

            use GNAT.String_Split;
         begin -- Parse_Cookies
            Create (Cookie_Set,
                    From       => Web.Environment.Value (Environment.Current, Name => Web.Environment.HTTP_Cookie),
                    Separators => ";");

            One_Cookie : for Index in 1 .. Slice_Count (Cookie_Set) loop
               Create (Name_Value, From => Slice (Cookie_Set, Index), Separators => "=");

               if Slice_Count (Name_Value) = 2 then
                  Cookies.Add (Name => Slice (Name_Value, 1), Value => Slice (Name_Value, 2) );
               end if;
            end loop One_Cookie;

            Request.Set.Cookies (Object => Object, Cookies => Cookies);
         end Parse_Cookies;
         -- Parse headers.
         Read_Payload : loop
            Ada.Streams.Read (Stream => Input_Stream.all, Item => Payload_Buffer, Last => Payload_Last);

            exit when Payload_Last = 0;

            Request.Set.Append_Payload (Object => Object, Payload => Payload_Buffer (1 .. Payload_Last) );
         end loop Read_Payload;

         Request.Set.Session_Context (Object, Settings => Session_Context);
      end Read_Request;

      procedure Write_Response (Object : in Response.Data) is
         Name_Separator  : constant String := ": ";
         Value_Separator : constant String := "; ";

         use Text_Streams;

         procedure Write_Header (Name : in String; Values : in String_Array; Continue : in out Boolean);

         procedure Write_Headers is new Containers.Tables.Iterate (Process => Write_Header);

         procedure Write_Header (Name : in String; Values : in String_Array; Continue : in out Boolean) is
         begin -- Write_Header
            Put (Stream => Output, Item => Name);
            Put (Stream => Output, Item => Name_Separator);

            for Index in Values'Range loop
               Put (Stream => Output, Item => +Values (Index) );

               exit when Index = Values'Last;

               Put (Stream => Output, Item => Value_Separator);
            end loop;

            New_Line (Stream => Output);
         end Write_Header;

         Response_Headers : constant Web.Headers.List := Response.Headers (Object);
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
         raise;
      when Text_Streams.End_Of_Stream =>
         raise Invalid_Gateway with "I/O stream ended unexpectedly.";
      when O : others =>
         Program_Response := Response.Build (Content_Type => "text/plain",
                                             Message_Body => Ada.Exceptions.Exception_Name (O) & " - " &
                                                             Ada.Exceptions.Exception_Message (O) );
         Write_Response (Object => Program_Response);
   end Program;

   function Value (Name : Solid.Web.Environment.Variable) return String is
   begin -- Value
      return Web.Environment.Value (Standard.Environment.Current, Name => Name);
   end Value;

   function Value (Name : String) return String is
   begin
      return Web.Environment.Value (Standard.Environment.Current, Name => Name);
   end Value;
end Solid.Web.Standard;
