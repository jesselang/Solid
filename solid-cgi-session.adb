with Ada.Numerics.Discrete_Random;
with Ada.Strings.Unbounded.Hash;
with Ada.Text_IO;
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
         Settings.Initialize;
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

   procedure Initialize (Session : in out Data; Settings : in Context_Handle);

   function Create (Settings : not null Context_Handle) return Data is
   begin -- Create
      if Settings = null or else not Valid (Settings.all) then
         raise Invalid_Context;
      end if;

      Locked : declare
         Lock    : PragmARC.Safe_Semaphore_Handler.Safe_Semaphore (Settings.Lock'Access);
      begin -- Locked
         return Session : Data do
            Initialize (Session => Session, Settings => Settings);
            Create (Settings => Session.Settings.all, Session => Session);
         end return;
      end Locked;
   end Create;

   procedure Delete (Session : in out Data) is
   begin -- Delete
      if Session.Settings = null or else not Valid (Session.Settings.all) then
         raise Invalid_Context;
      end if;

      Locked : declare
         Lock : PragmARC.Safe_Semaphore_Handler.Safe_Semaphore (Session.Settings.Lock'Access);
      begin -- Locked
         Delete (Settings => Session.Settings.all, Session => Session);
      end Locked;
   end Delete;

--   procedure Read (From : not null Context_Handle; Identity : in String; To : out Data) is
   function Read (From : not null Context_Handle; Identity : String) return Data is
      Lock : PragmARC.Safe_Semaphore_Handler.Safe_Semaphore (From.Lock'Access);

      use type Ada.Calendar.Time;
   begin -- Read
      if not Valid (From.all) then
         raise Invalid_Context;
      end if;

      return Result : Data do
         Read (Settings => From.all, Identity => Identity, Session => Result);

         if Ada.Calendar.Clock > Result.Expires then
            raise Expired;
         end if;

         Result.Settings := From;
      end return;
   end Read;

   procedure Write (Session : in out Data) is
   begin -- Write
      if not Session.Modified then
         return;
      end if;

      if Session.Settings = null or else not Valid (Session.Settings.all) then
         raise Invalid_Context;
      end if;

      Locked : declare
         Lock : PragmARC.Safe_Semaphore_Handler.Safe_Semaphore (Session.Settings.Lock'Access);
      begin -- Locked
         Write (Settings => Session.Settings.all, Session => Session);
         Session.Modified := False;
      end Locked;
   end Write;

   function Name (Session : Data) return String is
   begin -- Name
      if Session.Settings = null then
         raise Invalid_Context;
      end if;

      return +Session.Settings.Name;
   end Name;

   function Identity (Session : Data) return String is
   begin -- Identity
      if Session.Identity = No_Identity then
         return "";
      else
         return Session.Identity;
      end if;
   end Identity;

   procedure New_Identity (Session : in out Data) is
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

   function Expires (Session : Data) return Ada.Calendar.Time is
   begin -- Expires
      return Session.Expires;
   end Expires;

   type Valid_Tuple is new Tuple with null record; -- Used to create valid aggregates for Exists and Get.

   function Exists (Session : Data; Key : String) return Boolean is
   begin -- Exists
      return Session.Data_Set.Contains (Valid_Tuple'(Key => +Key) );
   end Exists;

   procedure Delete (Session : in out Data; Key : in String) is
   begin -- Delete
      if not Exists (Session, Key => Key) then
         raise Not_Found;
      end if;

      Session.Data_Set.Delete (Item => (Valid_Tuple'(Key => +Key) ) );
   end Delete;

   procedure Input (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : out Data) is
   begin -- Input
      Item.Identity := Session_Identity'Input (Stream);
      Item.Data_Set := Tuple_Tables.Set'Input (Stream);
      Item.Expires := Ada.Calendar.Time'Input (Stream);
   end Input;

   procedure Output (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : in Data) is
   begin -- Output
      Session_Identity'Output (Stream, Item.Identity);
      Tuple_Tables.Set'Output (Stream, Item.Data_Set);
      Ada.Calendar.Time'Output (Stream, Item.Expires);
   end Output;

   procedure Initialize (Session : in out Data; Settings : Context_Handle) is
      use type Ada.Calendar.Time;
   begin -- Initialize
      Session.Settings := Settings;
      Session.Expires := Ada.Calendar.Clock + Settings.Lifetime;
      Session.Modified := True;
      Session.New_Identity;
   end Initialize;

   procedure Finalize (Object : in out Data) is
   begin -- Finalize
      if Object.Settings = null or else not Valid (Object.Settings.all) then
         return; -- If Object does not have a valid context, nothing can be done.
      end if;

      if Object.Modified then
         Write (Session => Object);
      end if;

      Close (Settings => Object.Settings.all, Session => Object);
   exception -- Finalize
      when others =>
         null; -- Ignore exceptions during finalization.
   end Finalize;

   function Get_Tuple (Session : Data; Key : String) return Tuple'Class is
      Position : Tuple_Tables.Cursor := Session.Data_Set.Find (Valid_Tuple'(Key => +Key) );

      use type Tuple_Tables.Cursor;
   begin -- Get_Tuple
      if Position = Tuple_Tables.No_Element then
         raise Not_Found;
      end if;

      return Tuple_Tables.Element (Position);
   end Get_Tuple;

   procedure Set_Tuple (Session : in out Data; Item : in Tuple'Class) is
   begin -- Set_Tuple
      if Exists (Session, Key => +Item.Key) then
         Session.Data_Set.Replace (New_Item => Item);
      else
         Session.Data_Set.Insert (New_Item => Item);
      end if;

      Session.Modified := True;
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
