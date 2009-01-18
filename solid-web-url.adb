with Ada.Strings.Fixed;
with Ada.Strings.Maps;

package body Solid.Web.URL is
   --  The general URL form as described in RFC2616 is:
   --
   --  http_URL = "http:" "//" host [ ":" port ] [ abs_path [ "?" query ]]
   --
   --  Note also that there are different RFC describing URL like the 2616 and
   --  1738 but they use different terminologies. Here we try to follow the
   --  names used in RFC2616 but we have implemented some extensions at the
   --  end of this package. For example the way Path and File are separated or
   --  the handling of user/password which is explicitly not allowed in the
   --  RFC but are used and supported in many browsers. Here are the extended
   --  URL supported:
   --
   --  http://username:password@www.here.com:80/dir1/dir2/xyz.html?p=8&x=doh
   --   |                            |       | |          |       |
   --   protocol                     host port path       file    parameters
   --
   --                                          <--  pathname  -->

   function Protocol (URL : Object) return URL_Protocol is
   begin -- Protocol
      return URL.Protocol;
   end Protocol;

   use Solid.Strings;

   function Username (URL : Object) return String is
   begin -- Username
      return +URL.Username;
   end Username;

   function Password (URL : Object) return String is
   begin -- Password
      return +URL.Password;
   end Password;

   function Host (URL : Object) return String is
   begin -- Host
      return +URL.Host;
   end Host;

   function Port (URL : Object) return Network_Port is
   begin -- Port
      return URL.Port;
   end Port;

   function Path (URL : Object) return String is
   begin -- Path
      return +URL.Path;
   end Path;

   function Parameters (URL : Object) return Web.Parameters.List is
   begin -- Parameters
      return URL.Parameters;
   end Parameters;

   function URL_String (URL : Object) return String is
   begin -- URL_String
      return "";
   end URL_String;

   function Parse (URL : String) return Object is
      Protocol_Index : constant Natural := Ada.Strings.Fixed.Index (URL, Pattern => "://");

      Parsed : Object;
   begin -- Parse
      if URL'Length = 0 or Protocol_Index = 0 then
         raise Invalid;
      end if;

      declare
         Path_Index : constant Natural := Ada.Strings.Fixed.Index (URL (Protocol_Index + 1 .. URL'Last), Pattern => "/");
         Auth_Index : constant Natural := Ada.Strings.Fixed.Index (URL (Protocol_Index + 1 .. Path_Index - 1), Pattern => "@");
      begin
         null;
      end;

      return (others => <>);
   end Parse;

   function Query (URL : String) return String is
      First : constant Natural := Ada.Strings.Fixed.Index (Source => URL, Pattern => "?") + 1;

      Last  : Natural := Ada.Strings.Fixed.Index (Source => URL, Pattern => "#");
   begin -- Query
      if First = 0 then
         return "";
      elsif Last = 0 then
         return URL (First .. URL'Last);
      else
         return URL (First .. Last);
      end if;
   end Query;

   Encoding_Token  : constant Character := '%';
   Encoding_Length : constant           := 2;
   Space_Encoding  : constant Character := '+';

   function Decode (S : String) return String is
      Encodings : constant Natural := Ada.Strings.Fixed.Count (S, Pattern => (1 => Encoding_Token) );

      Encoded_Value  : Natural;
      Decoded_Value  : Character;
      Result         : String  := S;
      Previous_Index : Natural := Result'First - 1;
      Index          : Natural := Result'First;
      Last           : Natural := Result'Last;
   begin -- Decode
      Ada.Strings.Fixed.Translate (Source => Result, Mapping => Ada.Strings.Maps.To_Mapping (From => "+", To => " ") );

      if Encodings = 0 or Result'Length = 0 then
         return Result;
      end if;

      Decode_Encodings : loop
         Index := Ada.Strings.Fixed.Index (Result, Pattern => (1 => Encoding_Token), From => Index);

         if Previous_Index >= Result'First then
            if Index > 0 then
               Result (Previous_Index + 1 .. Index - 1 - Encoding_Length) :=
                                                                     Result (Previous_Index + 1 + Encoding_Length .. Index - 1);
            else
               Result (Previous_Index + 1 .. Last - Encoding_Length) :=
                                                                     Result (Previous_Index + 1 + Encoding_Length .. Last);
            end if;

            Last := Last - Encoding_Length;
         end if;

         exit Decode_Encodings when Index = 0;

         Encoded_Value := Natural'Value ("16#" & Result (Index + 1 .. Index + Encoding_Length) & '#');
         Decoded_Value := Character'Val (Encoded_Value);
         Result (Index) := Decoded_Value;
         Previous_Index := Index;
      end loop Decode_Encodings;

      return Result (Result'First .. Last);
   end Decode;
end Solid.Web.URL;
