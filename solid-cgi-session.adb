with Ada.Numerics.Discrete_Random;
with Ada.Strings.Unbounded.Hash;
with GNAT.MD5;
with Solid.Strings;
use Solid.Strings;

package body Solid.CGI.Session is
   function Name (Session : Data'Class) return String is
   begin -- Name
      return +Session.Name;
   end Name;

   procedure Set_Name (Session : in out Data'Class; To : in String) is
   begin -- Set_Name
      Session.Name := +To;
   end Set_Name;

   function Identity (Session : Data'Class) return String is
   begin -- Identity
      if Session.Identity = No_Identity then
         return "";
      else
         return Session.Identity;
      end if;
   end Identity;

   procedure New_Identity (Session : in out Data'Class) is
      -- Hash a pseudo-random with the session name.
      function Entropy return String is
         subtype Entropy_Length is Natural range 0 .. 16;
         package Random_Length is new Ada.Numerics.Discrete_Random (Entropy_Length);
         package Random_Characters is new Ada.Numerics.Discrete_Random (Character);

         Length     : Random_Length.Generator;
         Characters : Random_Characters.Generator;
      begin -- Entropy
         Random_Length.Reset (Length);
         Random_Characters.Reset (Characters);

         declare
            Result_Length : constant Natural := Random_Length.Random (Length);
            Result : String (1 .. Result_Length);
         begin
            for Index in Result'Range loop
               Result (Index) := Random_Characters.Random (Characters);
            end loop;

            return Result;
         end;
      end Entropy;
   begin -- New_Identity
      Session.Identity := GNAT.MD5.Digest (Entropy & (+Session.Name) );
   end New_Identity;

   type Valid_Tuple is new Tuple with null record; -- Used to create valid aggregates for Exists and Get.

   function Exists (Session : Data'Class; Key : String) return Boolean is
   begin -- Exists
      return Session.Tuples.Contains (Valid_Tuple'(Key => +Key) );
   end Exists;

   procedure Delete (Session : in out Data'Class; Key : in String) is
   begin -- Delete
      if not Exists (Session, Key => Key) then
         raise Not_Found;
      end if;

      Session.Tuples.Delete (Item => (Valid_Tuple'(Key => +Key) ) );
   end Delete;

   function Get_Tuple (Session : Data'Class; Key : String) return Tuple'Class is
      Position : Tuple_Tables.Cursor := Session.Tuples.Find (Valid_Tuple'(Key => +Key) );

      use type Tuple_Tables.Cursor;
   begin -- Get_Tuple
      if Position = Tuple_Tables.No_Element then
         raise Not_Found;
      end if;

      return Tuple_Tables.Element (Position);
   end Get_Tuple;

   procedure Set_Tuple (Session : in out Data'Class; Item : in Tuple'Class) is
   begin -- Set_Tuple
      if Exists (Session, Key => +Item.Key) then
         Session.Tuples.Replace (New_Item => Item);
      else
         Session.Tuples.Insert (New_Item => Item);
      end if;
   end Set_Tuple;

   function Hash (Element : Tuple'Class) return Ada.Containers.Hash_Type is
   begin -- Hash
      return Ada.Strings.Unbounded.Hash (Element.Key);
   end Hash;

   function Equivalent_Elements (Left : Tuple'Class; Right : Tuple'Class) return Boolean is
   begin -- Equivalent_Elements
      return Left.Key = Right.Key;
   end Equivalent_Elements;
end Solid.CGI.Session;
