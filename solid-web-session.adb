with Ada.Numerics.Discrete_Random;
with Ada.Strings.Unbounded.Hash;
with Ada.Text_IO;
with GNAT.MD5;
with PragmARC.Safe_Semaphore_Handler;
with Solid.Strings;
use Solid.Strings;

package body Solid.Web.Session is
   function Valid (Session : Data) return Boolean is
      use type Storage.Context_Handle;
   begin -- Valid
      return Session.Settings /= Storage.No_Context;
   end Valid;

   procedure Initialize (Session : in out Data; Settings : in not null Storage.Context_Handle);

   procedure Delete (Session : in out Data) is
      procedure Process is
      begin -- Process
         Storage.Delete (Settings => Session.Settings.all, Session => Session);
      end Process;

      procedure Safe_Delete is new Storage.Safe_Process (Process => Process);

      use type Storage.Context_Handle;
   begin -- Delete
      if Session.Settings = Storage.No_Context or else not Storage.Valid (Session.Settings.all) then
         raise Invalid_Context;
      end if;

      Safe_Delete (Session.Settings.all);
   end Delete;

   procedure Write (Session : in out Data) is
      procedure Process is
      begin -- Process
         Storage.Write (Settings => Session.Settings.all, Session => Session);
         Session.Modified := False;
      end Process;

      procedure Safe_Write is new Storage.Safe_Process (Process => Process);

      use type Storage.Context_Handle;
   begin -- Write
      if not Session.Modified then
         return;
      end if;

      if Session.Settings = Storage.No_Context or else not Storage.Valid (Session.Settings.all) then
         raise Invalid_Context;
      end if;

      Safe_Write (Session.Settings.all);
   end Write;

   function Name (Session : Data) return String is
      use type Storage.Context_Handle;
   begin -- Name
      if Session.Settings = Storage.No_Context then
         raise Invalid_Context;
      end if;

      return Storage.Name (Session.Settings.all);
   end Name;

   function Identity (Session : Data) return String is
      use type Storage.Context_Handle;
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
      Session.Identity := GNAT.MD5.Digest (Entropy & (Storage.Name (Session.Settings.all) ) );
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


   package body Storage is
      procedure Initialize (Settings : in out Context_Handle;
                            Name     : in     String           := "Session";
                            Lifetime : in     Storage_Lifetime := Storage_Lifetime'Last)
      is
         procedure Process is
         begin -- Process
            Settings.Name := +Name;
            Settings.Lifetime := Lifetime;
            Settings.Initialize;
            Settings.Valid := True;
         end Process;

         procedure Safe_Initialize is new Safe_Process (Process => Process);
      begin -- Initialize
         if Settings = Storage.No_Context then
            raise Invalid_Context;
         end if;

         Safe_Initialize (Settings.all);
      end  Initialize;

      function Name (Settings : Context'Class) return String is
      begin -- Name
         return +Settings.Name;
      end Name;

      function Valid (Settings : Context'Class) return Boolean is
      begin -- Valid
         return Settings.Valid;
      end Valid;

      function Lifetime (Settings : Context'Class) return Duration is
      begin -- Lifetime
         return Settings.Lifetime;
      end Lifetime;

      procedure Set_Lifetime (Settings : in out Context'Class; To : in Duration) is
         procedure Process is
         begin -- Process
            Settings.Lifetime := To;
         end Process;

         procedure Safe_Set_Lifetime is new Safe_Process (Process => Process);
      begin -- Set_Lifetime
         Safe_Set_Lifetime (Settings);
      end Set_Lifetime;

      procedure Safe_Process (Settings : in out Context'Class) is
         Lock : PragmARC.Safe_Semaphore_Handler.Safe_Semaphore (Settings.Lock'Access);
      begin -- Safe_Process
         Process;
      end Safe_Process;
   end Storage;

   procedure Create (Settings : not null Storage.Context_Handle; Session : out Data) is
      procedure Process is
      begin -- Process
         Initialize (Session => Session, Settings => Settings);
         Storage.Create (Settings => Session.Settings.all, Session => Session);
      end Process;

      procedure Safe_Create is new Storage.Safe_Process (Process => Process);
   begin -- Create
      if not Storage.Valid (Settings.all) then
         raise Invalid_Context;
      end if;

      Safe_Create (Settings.all);
   end Create;

   function Create (Settings : not null Storage.Context_Handle) return Handle is
      Session : Handle := new Data;
   begin -- Create
      if not Storage.Valid (Settings.all) then
         raise Invalid_Context;
      end if;

      Create (Settings => Settings, Session => Session.all);

      return Session;

      --------------------------------------------------------------------------
      -- Once FSF GNAT supports extended returns, Data may be returned directly.
      --~ return Session : Data do
         --~ Create (Settings => Settings, Session => Session);
      --~ end return;
   end Create;

   function Read (From : not null Storage.Context_Handle; Identity : String) return Handle is
      Session : Handle := new Data;

      procedure Process is
      begin -- Process
         Storage.Read (Settings => From.all, Identity => Identity, Session => Session.all);
      end Process;

      procedure Safe_Read is new Storage.Safe_Process (Process => Process);

      use type Ada.Calendar.Time;
   begin -- Read
      if not Storage.Valid (From.all) then
         raise Invalid_Context;
      end if;

      Safe_Read (From.all);

      if Ada.Calendar.Clock > Session.Expires then
         raise Expired;
      end if;

      Session.Settings := From;

      return Session;

      ------------------------------------------------------------------------------------
      -- Once FSF GNAT supports extended return statements, Data may be directly returned.
      --~ return Result : Data do
         --~ declare
            --~ procedure Process is
            --~ begin -- Process
               --~ Storage.Read (Settings => From.all, Identity => Identity, Session => Result);
            --~ end Process;

            --~ procedure Safe_Read is new Storage.Safe_Process (Process => Process);
         --~ begin
            --~ Safe_Read (From.all);

            --~ if Ada.Calendar.Clock > Result.Expires then
               --~ raise Expired;
            --~ end if;

            --~ Result.Settings := From;
         --~ end;
      --~ end return;
   end Read;

   procedure Initialize (Session : in out Data; Settings : in not null Storage.Context_Handle) is
      use type Ada.Calendar.Time;
   begin -- Initialize
      Session.Settings := Settings;
      Session.Expires := Ada.Calendar.Clock + Settings.Lifetime;
      Session.Modified := True;
      Session.New_Identity;
   end Initialize;

   procedure Finalize (Object : in out Data) is
      use type Storage.Context_Handle;
   begin -- Finalize
      if Object.Settings = Storage.No_Context or else not Storage.Valid (Object.Settings.all) then
         return; -- If Object does not have a valid context, nothing can be done.
      end if;

      if Object.Modified then
         Write (Session => Object);
      end if;

      Storage.Close (Settings => Object.Settings.all, Session => Object);
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
end Solid.Web.Session;
