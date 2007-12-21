with Solid.CGI.Containers.Tables;

package Solid.CGI.Parameters is
   type List is new Containers.Tables.Table with null record;

   Parse_Error : exception;

   function Parse_URL_Encoding (Query : String) return List;
   -- Parses URL encoded Query, returning a List of parameters.
   -- Raises Parse_Error if Query could not be parsed.
end Solid.CGI.Parameters;
