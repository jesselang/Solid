private with Ada.Strings.Unbounded.Hash;

private with Solid.Data_Structures.Hashed_Multimaps;

package Solid.CGI.Headers is
   type List is private;

   procedure Add (To : in out List; Name : in String; Value : in String);

   generic
      with procedure Process (Name : in String; Values : in String);
   procedure Iterate (Item : in List);
private -- Solid.CGI.Headers
   package Implementation is new Data_Structures.Hashed_Multimaps (Map_Key         => Ada.Strings.Unbounded.Unbounded_String,
                                                                   Element         => Ada.Strings.Unbounded.Unbounded_String,
                                                                   Hash            => Ada.Strings.Unbounded.Hash,
                                                                   Equivalent_Keys => Ada.Strings.Unbounded."=",
                                                                   "="             => Ada.Strings.Unbounded."=");
   type List is record
      Handle : Implementation.Map;
   end record;
end Solid.CGI.Headers;
