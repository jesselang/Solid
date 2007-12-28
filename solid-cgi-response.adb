with Ada.Strings.Unbounded;
with Solid.CGI.Environment;
with Solid.Strings;
with Solid.Text_Streams;

use Solid.Strings;

package body Solid.CGI.Response is
   function Test (Client : Request.Data) return Data is
      Result : Strings.U_String;

      use type Ada.Strings.Unbounded.Unbounded_String;

      procedure Append_Variable (Name : in String; Value : in String; Continue : in out Boolean) is
      begin -- Append_Variable
         Result := Result & Name & ": " & Value & ASCII.LF;
      end Append_Variable;

      procedure Append_Variables is new Environment.Iterate (Process => Append_Variable);
   begin -- Test
      Append_Variables (Object => Request.Environment (Client) );

      return Build (Content_Type => "text/plain", Message_Body => -Result);
   end Test;

   function Build (Content_Type : String; Message_Body : String) return Data is
      Result : Data;
   begin -- Build
      Result.Headers.Add (Name => "Content-type", Value => Content_Type);
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
