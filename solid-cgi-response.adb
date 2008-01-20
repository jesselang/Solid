with Solid.CGI.Environment;
with Solid.CGI.Parameters;
with Solid.CGI.URL;
with Solid.Strings;
with Solid.Text_Streams;

use Solid.Strings;

package body Solid.CGI.Response is
   function Test (Client : Request.Data) return Data is
      Result : Strings.U_String;

      procedure Append_Variable (Name : in String; Value : in String; Continue : in out Boolean) is
      begin -- Append_Variable
         Result := Result & Name & ": " & Value & ASCII.LF;
      end Append_Variable;

      procedure Append_Variables is new Environment.Iterate (Process => Append_Variable);

      Query : constant Parameters.List := Request.Parameters (Client);
   begin -- Test
      Append_Variables (Object => Request.Environment (Client) );

      -- Result := Result & URL.Decode (Request.Query (Client) );

      Result := Result & Text_Streams.To_String (Request.Payload (Client) );

      return Build (Content_Type => "text/plain", Message_Body => -Result);
   end Test;

   function Build (Content_Type : String;
                   Message_Body : String;
                   Headers      : CGI.Headers.List := CGI.Headers.No_Headers)
   return Data is
      Result : Data;
   begin -- Build
      Result.Headers := Headers;

      if not Result.Headers.Exists (Name => CGI.Headers.Content_Type) then
         Result.Headers.Add (Name => CGI.Headers.Content_Type, Value => Content_Type);
      else
         Result.Headers.Update (Name => CGI.Headers.Content_Type, Value => Content_Type);
      end if;

      Result.Payload := +Message_Body;

      return Result;
   end Build;

   function Headers (Object : Data) return CGI.Headers.List is
   begin -- Headers
      return Object.Headers;
   end Headers;

   function Payload (Object : Data) return Ada.Streams.Stream_Element_Array is
   begin -- Payload
      return Text_Streams.To_Stream (-Object.Payload);
   end Payload;
end Solid.CGI.Response;
