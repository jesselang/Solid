with Solid.Web.Environment;
with Solid.Web.Parameters;
with Solid.Web.URL;
with Solid.Strings;
with Solid.Text_Streams;

use Solid.Strings;

package body Solid.Web.Response is
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

      return Build (Content_Type => "text/plain", Message_Body => +Result);
   end Test;

   function Build (Content_Type : String;
                   Message_Body : String;
                   Headers      : Web.Headers.List := Web.Headers.No_Headers)
   return Data is
      Result : Data;
   begin -- Build
      Result.Headers := Headers;

      if not Result.Headers.Exists (Name => Web.Headers.Content_Type) then
         Result.Headers.Add (Name => Web.Headers.Content_Type, Value => Content_Type);
      else
         Result.Headers.Update (Name => Web.Headers.Content_Type, Value => Content_Type);
      end if;

      Result.Payload := +Message_Body;

      return Result;
   end Build;

   function URL (Location : String) return Data is
      Result : Data;
   begin -- URL
      Result.Headers.Add (Name => "Location", Value => Location);

      return Result;
   end URL;

   function Code (Object : Data) return Web.Messages.Status_Code is
   begin -- Code
      return Object.Code;
   end Code;

   function Reason (Object : Data) return String is
   begin -- Reason
      return +Object.Reason;
   end Reason;

   function Headers (Object : Data) return Web.Headers.List is
   begin -- Headers
      return Object.Headers;
   end Headers;

   function Payload (Object : Data) return Ada.Streams.Stream_Element_Array is
   begin -- Payload
      return Text_Streams.To_Stream (+Object.Payload);
   end Payload;
end Solid.Web.Response;
