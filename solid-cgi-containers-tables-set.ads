
package Solid.CGI.Containers.Tables.Set is
   procedure Add (Container : in out Table; Name : in String; Value : in String);

   procedure Update (Container : in out Table; Name : in String; Value : in String; Index : in Value_Index := First_Value);

   procedure Reset (Container : in out Table);
end Solid.CGI.Containers.Tables.Set;
