with Ada.Strings.Unbounded.Hash;

with Solid.Data_Structures.Hashed_Multimaps;
with Solid.Strings;

package Solid.CGI.Containers.Tables is
   type Table is tagged private;

   function Size (Container : Table) return Count;

   function Exist (Container : Table; Name : String) return Boolean;

   function Get (Container : Table; Name : String; Position : Index := Index'First) return String;

   type String_Array is array (Index range <>) of Solid.Strings.U_String;

   generic -- Iterate
      with procedure Process (Name : in String; Values : in String_Array);
   procedure Iterate (Container : in Table);
private -- Solid.CGI.Containers.Tables
   package Implementation is new Solid.Data_Structures.Hashed_Multimaps (Map_Key         => Solid.Strings.U_String,
                                                                         Element         => Solid.Strings.U_String,
                                                                         Hash            => Ada.Strings.Unbounded.Hash,
                                                                         Equivalent_Keys => Ada.Strings.Unbounded."=",
                                                                         "="             => Ada.Strings.Unbounded."=");

   type Table is tagged record
      Handle : Implementation.Map;
   end record;
end Solid.CGI.Containers.Tables;
