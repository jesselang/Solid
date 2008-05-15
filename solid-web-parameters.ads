-- ADT and operations for HTTP query parameters.
with Solid.Web.Containers.Tables;

package Solid.Web.Parameters is
   type List is new Containers.Tables.Table with null record;
   -- See Solid.Web.Containers.Tables for inherited operations.

   Parse_Error : exception;

   type Parse_Style is (Strict, Relaxed);

   function Parse_URL_Encoding (Query : String; Style : Parse_Style := Relaxed) return List;
   -- Parses URL encoded Query, excluding the '?', returning a List of parameters.
   -- Raises Parse_Error if Query could not be parsed.
   -- If a malformed parameter is found and Style = Strict, Parse_Error will be raised.
   -- Otherwise, if Style = Relaxed, the malformed parameter will be ignored.
end Solid.Web.Parameters;
