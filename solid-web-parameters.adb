with GNAT.String_Split;
with Solid.Web.URL;

package body Solid.Web.Parameters is

   function Parse_URL_Encoding (Query : String; Style : Parse_Style := Relaxed) return List is
      Parameters : GNAT.String_Split.Slice_Set;
      Name_Value : GNAT.String_Split.Slice_Set;
      Result     : Web.Parameters.List;

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
            if Style = Strict then
               raise Parse_Error with "Malformed parameter.";
            end if;
         end if;
      end loop;

      return Result;
   end Parse_URL_Encoding;
end Solid.Web.Parameters;
