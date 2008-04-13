with GNAT.String_Split;
with Solid.CGI.URL;

package body Solid.CGI.Parameters is

   function Parse_URL_Encoding (Query : String) return List is
      Parameters : GNAT.String_Split.Slice_Set;
      Name_Value : GNAT.String_Split.Slice_Set;
      Result     : CGI.Parameters.List;

      use GNAT.String_Split;
   begin -- Parameters
      Create (Parameters, From => Query, Separators => "&");

      for Index in 1 .. Slice_Count (Parameters) loop
         Create (Name_Value, From => Slice (Parameters, Index => Index), Separators => "=");

         if Slice_Count (Name_Value) = 2 then
            Result.Add (Name => URL.Decode (Slice (Name_Value, Index => 1) ),
                        Value => URL.Decode (Slice (Name_Value, Index => 2) ) );
         elsif Slice_Count (Name_Value) = 1 then
            Result.Add (Name => URL.Decode (Slice (Name_Value, Index => 1) ), Value => "");
         else
            null; -- Invalid parameter, move on.
         end if;
      end loop;

      return Result;
   end Parse_URL_Encoding;
end Solid.CGI.Parameters;
