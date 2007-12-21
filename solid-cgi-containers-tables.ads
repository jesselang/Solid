with Ada.Strings.Unbounded.Hash;

with Solid.Data_Structures.Hashed_Multimaps;
with Solid.Strings;

package Solid.CGI.Containers.Tables is
   type Table is tagged private;

   Empty : constant Table;

   function Size (Container : Table) return Count;

   function Exist (Container : Table; Name : String) return Boolean;

   function Get (Container : Table; Name : String; Position : Index := Index'First) return String;

   generic -- Iterate
      with procedure Process (Name : in String; Values : in Strings.String_Array);
   procedure Iterate (Container : in Table);

   procedure Add (Container : in out Table; Name : in String; Value : in String);

   procedure Update (Container : in out Table; Name : in String; Value : in String; Position : in Index := Index'First);

   procedure Clear (Container : in out Table);
private -- Solid.CGI.Containers.Tables
   package Implementation is new Solid.Data_Structures.Hashed_Multimaps (Map_Key         => Strings.U_String,
                                                                         Element         => Strings.U_String,
                                                                         Hash            => Ada.Strings.Unbounded.Hash,
                                                                         Equivalent_Keys => Ada.Strings.Unbounded."=",
                                                                         "="             => Ada.Strings.Unbounded."=");

   type Table is tagged record
      Handle : Implementation.Map;
   end record;

   Empty : constant Table := (Handle => Implementation.Empty_Map);
end Solid.CGI.Containers.Tables;
