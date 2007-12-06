package body Solid.CGI.Containers.Tables is
   function Size (Container : Table) return Count is
   begin -- Size
      return 0;
   end Size;

   --~ function Size (Container : Table; Name : String) return Count is
   --~ begin -- Size
      --~ return 0;
   --~ end Size;

   function Exist (Container : Table; Name : String) return Boolean is
   begin -- Exist
      return False;
   end Exist;

   function Get (Container : Table; Name : String; Index : Value_Index := First_Value) return String is
   begin -- Get
      return "";
   end Get;

   procedure Iterate (Container : in Table) is
   begin -- Iterate
      null;
   end Iterate;
end Solid.CGI.Containers.Tables;
