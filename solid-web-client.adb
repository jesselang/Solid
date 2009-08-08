with Ada.Strings.Fixed;
with Ada.Text_IO;
with GNAT.Sockets;
with PragmARC.Mixed_Case;
with Solid.Finalization;
with Solid.Text_Streams;
with Solid.Web.Messages;
with Solid.Web.Response.Client;

package body Solid.Web.Client is
   type Host_Info is record
      Text    : Strings.U_String;
      Address : GNAT.Sockets.Sock_Addr_Type := GNAT.Sockets.No_Sock_Addr;
   end record;

   No_Host : constant Host_Info := (Address => GNAT.Sockets.No_Sock_Addr, others => <>);
                                    -- Address is defined due to a GNAT error.

   function Get_Host (URL : String) return Host_Info;
   -- Returns a socket address for the URL.
   -- Returns No_Host if the address could not be parsed from URL.
   pragma Inline (Get_Host);

   function Request (URL : String) return String;
   -- Returns the request part of URL.
   pragma Inline (Request);

   procedure Put_Authorization (Stream : in out Text_Streams.Text_Stream; Credentials : in Authorization_Info);
   -- Outputs Credentials to Stream.
   pragma Inline (Put_Authorization);

   function Get (URL         : String;
                 Timeout     : Connection_Timeout := No_Timeout;
                 Credentials : Authorization_Info := No_Authorization;
                 Headers     : Headers_List       := No_Headers)
   return Response.Data is
      use GNAT.Sockets;

      Socket  : Socket_Type;
      Host    : Host_Info;
      Text    : Text_Streams.Text_Stream;
      Result  : Response.Data;

      use Solid.Strings;
   begin -- Get
      Host := Get_Host (URL);

      if Host = No_Host then
         raise Host_Error;
      end if;

      Create_Socket  (Socket => Socket);
      Connect_Socket (Socket => Socket, Server => Host.Address);

      Text_Streams.Create (Stream => Text, From => Stream (Socket), Line_Ending => Text_Streams.CR_LF);
      Text_Streams.Put_Line (Stream => Text, Item => "GET " & Request (URL) & ' ' & Messages.HTTP_Version_Token & "1.0");
      Text_Streams.Put_Line (Stream => Text, Item => "Host: " & (+Host.Text) );
      Web.Headers.Write (Headers => Headers, Stream => Stream (Socket) );
      Text_Streams.New_Line (Stream => Text);

      Result := Response.Client.Read (Stream => Stream (Socket) );
      Close_Socket (Socket => Socket);

      return Result;
   end Get;

   function Post (URL         : String;
                  Parameters  : Parameters_List;
                  Timeout     : Connection_Timeout := No_Timeout;
                  Credentials : Authorization_Info := No_Authorization;
                  Headers     : Headers_List       := No_Headers)
   return Response.Data is
      use GNAT.Sockets;

      Socket  : Socket_Type;
      Host    : Host_Info;
      Text    : Text_Streams.Text_Stream;
      Result  : Response.Data;

      use Solid.Strings;
   begin -- Post
      Host := Get_Host (URL);

      if Host = No_Host then
         raise Host_Error;
      end if;

      Create_Socket  (Socket => Socket);
      Connect_Socket (Socket => Socket, Server => Host.Address);

      Text_Streams.Create (Stream => Text, From => Stream (Socket), Line_Ending => Text_Streams.CR_LF);
      Text_Streams.Put_Line (Stream => Text, Item => "POST " & Request (URL) & ' ' & Messages.HTTP_Version_Token & "1.0");
      Text_Streams.Put_Line (Stream => Text, Item => "Host: " & (+Host.Text) );
      Web.Headers.Write (Headers => Headers, Stream => Stream (Socket) );
      Text_Streams.New_Line (Stream => Text);

      -- I believe we're missing the actual POSTing here.

      Result := Response.Client.Read (Stream => Stream (Socket) );
      Close_Socket (Socket => Socket);

      return Result;
   end Post;

   function Get_Host (URL : String) return Host_Info is
      Host_Start     : Natural;
      Host_End       : Natural;
      Port_Index     : Natural;
      Info           : Host_Info;

      use Ada.Strings.Fixed;
      use GNAT.Sockets;
      use Solid.Strings;
   begin -- Get_Host
      if URL'Length = 0 or URL (URL'First .. URL'First + 6) /= "http://" then
         return No_Host;
      end if;

      Host_Start := Index (URL, Pattern => "@");

      if Host_Start = 0 then
         Host_Start := URL'First + 6; -- "http://"'Length
      end if;

      Host_End := Index (URL (Host_Start + 1 .. URL'Last), Pattern => "/");

      if Host_End = 0 then
         Host_End := URL'Last + 1;
      end if;

      Port_Index := Index (URL (Host_Start + 1 .. Host_End - 1), Pattern => ":");

      if Port_Index /= 0 then
         Info.Address.Port := Port_Type'Value (URL (Port_Index + 1 .. Host_End - 1) );
      else
         Info.Address.Port := Port_Type (Default_HTTP_Port);
         Port_Index := Host_End;
      end if;

      Info.Text           := +URL (Host_Start + 1 .. Port_Index - 1);
      Info.Address.Addr  := Addresses (Get_Host_By_Name (URL (Host_Start + 1 .. Port_Index - 1) ) );

      return Info;
   end Get_Host;

   function Request (URL : String) return String is
      Start : constant Natural := Ada.Strings.Fixed.Index (URL (URL'First + 7 .. URL'Last), Pattern => "/");
   begin -- Request
      if Start = 0 then
         return "/";
      else
         return URL (Start .. URL'Last);
      end if;
   end Request;

   procedure Put_Authorization (Stream : in out Text_Streams.Text_Stream; Credentials : in Authorization_Info) is
   begin -- Put_Authorization
      if Credentials /= No_Authorization then
         Text_Streams.Put (Stream => Stream, Item => "Authorization: Basic "); -- Currently only Basic authorization is supported.

         Text_Streams.New_Line (Stream => Stream);
      end if;
   end Put_Authorization;

   procedure Finalize;
   package Finalizer is new Solid.Finalization (Process => Finalize);

   procedure Finalize is
   begin -- Finalize
      GNAT.Sockets.Finalize;
   end Finalize;
begin -- Solid.Web.Client
   GNAT.Sockets.Initialize;
end Solid.Web.Client;
