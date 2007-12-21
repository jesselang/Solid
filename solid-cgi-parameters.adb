with GNAT.String_Split;

--with Solid.CGI.Parameters.Set;

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
            Result.Add (Name => Slice (Name_Value, Index => 1), Value => Slice (Name_Value, Index => 2) );
         end if;
      end loop;

      return Result;
   end Parse_URL_Encoding;
end Solid.CGI.Parameters;
