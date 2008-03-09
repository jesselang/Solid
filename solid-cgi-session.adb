with Ada.Numerics.Discrete_Random;
with Ada.Strings.Unbounded.Hash;
with GNAT.MD5;
with PragmARC.Safe_Semaphore_Handler;
with Solid.Strings;
use Solid.Strings;

package body Solid.CGI.Session is
   procedure Initialize (Settings : in out Context_Handle; Name : String := "Session"; Lifetime : Duration := Duration'Last) is
   begin -- Initialize
      if Settings = null then
         raise Invalid_Context;
      end if;

      Locked : declare
         Lock : PragmARC.Safe_Semaphore_Handler.Safe_Semaphore (Settings.Lock'Access);
      begin -- Locked
         Settings.Name := +Name;
         Settings.Lifetime := Lifetime;
         Settings.Initialize_Implementation;
         Settings.Valid := True;
      end Locked;
   end  Initialize;

   function Valid (Settings : Context'Class) return Boolean is
   begin -- Valid
      return Settings.Valid;
   end Valid;

   function Lifetime (Settings : Context'Class) return Duration is
   begin -- Lifetime
      return Settings.Lifetime;
   end Lifetime;

   procedure Set_Lifetime (Settings : in out Context'Class; To : in Duration) is
   begin -- Set_Lifetime
      Settings.Lifetime := To;
   end Set_Lifetime;

   procedure Initialize (Session : in out Data'Class; Settings : in Context_Handle);

   function Create (Settings : not null Context_Handle) return Data'Class is
   begin -- Create
      if Settings = null or else not Valid (Settings => Settings.all) then
         raise Invalid_Context;
      end if;

      Locked : declare
         Lock    : PragmARC.Safe_Semaphore_Handler.Safe_Semaphore (Settings.Lock'Access);
         Session : Data'Class := New_Session (Settings => Settings.all);
      begin -- Locked
         Initialize (Session => Session, Settings => Settings);
         Create_Implementation (Session => Session);

         return Session;
      end Locked;
   end Create;

   procedure Delete (Session : in out Data'Class) is
   begin -- Delete
      if Session.Settings = null or else not Valid (Settings => Session.Settings.all) then
         raise Invalid_Context;
      end if;

      Locked : declare
         Lock : PragmARC.Safe_Semaphore_Handler.Safe_Semaphore (Session.Settings.Lock'Access);
      begin -- Locked
         Delete_Implementation (Session => Session);
      end Locked;
   end Delete;

   procedure Read (From : in out Context'Class; Identity : in String; To : out Data'Class) is
      Lock : PragmARC.Safe_Semaphore_Handler.Safe_Semaphore (From.Lock'Access);
   begin -- Read
      Read_Implementation (From => From, Identity => Identity, To => To);
   end Read;

   procedure Write (Session : in out Data'Class) is
   begin -- Write
      if Session.Settings = null or else not Valid (Settings => Session.Settings.all) then
         raise Invalid_Context;
      end if;

      Locked : declare
         Lock : PragmARC.Safe_Semaphore_Handler.Safe_Semaphore (Session.Settings.Lock'Access);
      begin -- Locked
         Write_Implementation (Session => Session);
      end Locked;
   end Write;

   function Name (Session : Data'Class) return String is
   begin -- Name
      return +Session.Settings.Name;
   end Name;

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
      Session.Identity := GNAT.MD5.Digest (Entropy & (+Session.Settings.Name) );
   end New_Identity;

   function Expires (Session : Data'Class) return Ada.Calendar.Time is
   begin -- Expires
      return Session.Expires;
   end Expires;

   type Valid_Tuple is new Tuple with null record; -- Used to create valid aggregates for Exists and Get.

   function Exists (Session : Data'Class; Key : String) return Boolean is
   begin -- Exists
      return Session.Data_Set.Contains (Valid_Tuple'(Key => +Key) );
   end Exists;

   procedure Delete (Session : in out Data'Class; Key : in String) is
   begin -- Delete
      if not Exists (Session, Key => Key) then
         raise Not_Found;
      end if;

      Session.Data_Set.Delete (Item => (Valid_Tuple'(Key => +Key) ) );
   end Delete;

   procedure Initialize (Session : in out Data'Class; Settings : Context_Handle) is
      use type Ada.Calendar.Time;
   begin -- Initialize
      Session.Settings := Settings;
      Session.Expires := Ada.Calendar.Clock + Settings.Lifetime;
      Session.New_Identity;
   end Initialize;

   function Get_Tuple (Session : Data'Class; Key : String) return Tuple'Class is
      Position : Tuple_Tables.Cursor := Session.Data_Set.Find (Valid_Tuple'(Key => +Key) );

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
         Session.Data_Set.Replace (New_Item => Item);
      else
         Session.Data_Set.Insert (New_Item => Item);
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
