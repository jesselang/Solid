with Solid.Web.Parameters;
with Solid.Strings;
-- Utility URL functions.
package Solid.Web.URL is
   type Object is private;

   type URL_Protocol is (HTTP, HTTPS);

   function Protocol   (URL : Object) return URL_Protocol;
   function Username   (URL : Object) return String;
   function Password   (URL : Object) return String;
   function Host       (URL : Object) return String;
   function Port       (URL : Object) return Network_Port;
   function Path       (URL : Object) return String;
   function Parameters (URL : Object) return Web.Parameters.List;
   function URL_String (URL : Object) return String;
   -- Returns a string representation of URL.

   Invalid : exception;

   function Parse (URL : String) return Object;
   -- Parses URL into an Object.
   -- Raises Invalid if URL could not be parsed.

   -- Query and Decode are used by Solid.Web.Standard.Program.

   function Query (URL : String) return String;
   -- Returns query portion of the URL.

   -- Encode/Decode
   function Decode (S : String) return String;

private -- Solid.Web.URL
   type Object is record
      Protocol   : URL_Protocol;
      Username   : Strings.U_String;
      Password   : Strings.U_String;
      Host       : Strings.U_String;
      Port       : Network_Port;
      Path       : Strings.U_String;
      Parameters : Web.Parameters.List;
   end record;

   --~ use type Ada.Strings.Maps.Character_Set;
   --~ Default_Encoding_Set : constant Strings.Maps.Character_Set
     --~ := Strings.Maps.To_Set
         --~ (Span => (Low  => Character'Val (128),
                   --~ High => Character'Val (Character'Pos (Character'Last))))
     --~ or
       --~ Strings.Maps.To_Set (";/?:@&=+$,<>#%""{}|\^[]`' ");

end Solid.Web.URL;
