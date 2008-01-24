private with Solid.Data_Structures.Hashed_Multimaps;

with Ada.Strings.Unbounded.Hash;
with Solid.Strings;

package Solid.CGI.Containers.Tables is
   Table_Failure : exception;

   type Table is tagged private;

   Empty : constant Table;

   function Size (Container : Table) return Count;

   function Exists (Container : Table; Name : String) return Boolean;

   function Get (Container : Table; Name : String; Position : Index := Index'First) return String;

   generic -- Iterate
      with procedure Process (Name : in String; Values : in Strings.String_Array; Continue : in out Boolean);
   procedure Iterate (Container : in Table'Class);

   generic -- Iterate
      with procedure Process (Value : in String; Continue : in out Boolean);
   procedure Iterate_Values (Container : in Table'Class; Name : in String);
   -- Raises Table_Failure if Name is not found in Container.

   procedure Add (Container : in out Table; Name : in String; Value : in String);
   -- Raises Table_Failure if an error occurs.

   procedure Update (Container : in out Table; Name : in String; Value : in String; Position : in Index := Index'First);
   -- Raises Table_Failure if an error occurs.

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
