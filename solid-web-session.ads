-- Session data types and operations.  See child package "Storage" for implementation of new storage methods.
-- See Solid.Web.Session.Files for a basic storage method suitable for "standard" low-volume web applications.
private with Ada.Containers.Indefinite_Hashed_Sets;
private with GNAT.MD5;
with PragmARC.Binary_Semaphore_Handler; -- Should be "private with", but older versions of GNAT have problems with this.
private with Solid.Calendar;
with Ada.Calendar;
with Ada.Finalization;
with Ada.Streams;
with Solid.Strings;

package Solid.Web.Session is
   type Data is limited private;
   -- The session data stores a client's set of data, based on a unique identity.
   -- Any data type may be stored in the session.  See Solid.CGI.Session.[Generic_]Tuples.

   type Handle is access Data;
   -- Clients should use a Handle to Data.
   -- Once FSF GNAT supports extended return statements, clients can use Data directly without needing a Handle.
   -- Deallocation of Data is the client's responsibility.

   function Valid (Session : Data) return Boolean;

   Not_Found : exception;

   -- Create and Read operations are found below the Storage child package.

   procedure Delete (Session : in out Data);
   -- Deletes Session from its associated context.
   -- Raises Not_Found if Session could not be found.

   procedure Write (Session : in out Data);
   -- Writes Session to its associated context.

   function Name (Session : Data) return String;
   -- Returns the name of the session's context.
   -- Raises Invalid_Context if Session's context is not valid.

   function Identity (Session : Data) return String;
   -- Returns the identity of Session.

   procedure New_Identity (Session : in out Data);
   -- Generates a new identity for Session.
   -- Raises Invalid_Context if Session's context is not valid.

   function Expires (Session : Data) return Ada.Calendar.Time;
   -- Returns the expiration time of Session.

   -- Tuple operations.
   function Exists (Session : Data; Key : String) return Boolean;
   -- Returns True if a tuple with Key is found in Session, otherwise False.

   procedure Delete (Session : in out Data; Key : in String);
   -- Deletes the tuple with Key in Session.
   -- Raises Not_Found if not Exists (Session, Key).

   -- For commonly-used tuple operations, see Solid.Web.Session.Tuples.

   Invalid_Context : exception;

   package Storage is
      -- Context.
      type Context is abstract tagged limited private;
      type Context_Handle is access Context'Class;
      -- A session context contains the session name, and any resources needed for extension.
      -- Extension is required for storage implementation of session data.

      No_Context : constant Context_Handle;

      subtype Storage_Lifetime is Duration range 0.0 .. 31_536_000.0; -- approximately 1 year, a good long time.

      procedure Initialize (Settings : in out Context_Handle;
                            Name     : in     String           := "Session";
                            Lifetime : in     Storage_Lifetime := Storage_Lifetime'Last);
      -- Initializes the session context in Settings, setting the session name to Name.
      -- See the Initialize procedure for the extension you are using, it is often more appropriate.
      -- Raises Invalid_Context if the context could not be initialized.

      function Name (Settings : Context'Class) return String;
      -- Returns the Name of Settings.

      function Valid (Settings : Context'Class) return Boolean;
      -- Returns True if Settings was initialized, otherwise False.

      function Lifetime (Settings : Context'Class) return Duration;
      -- Gets the lifetime for session data objects created using Settings.

      procedure Set_Lifetime (Settings : in out Context'Class; To : in Duration);
      -- Sets the lifetime for session data objects created using Settings.

      generic -- Safe_Process
         with procedure Process;
      procedure Safe_Process (Settings : in out Context'Class);
      -- Executes Process with exclusive access to Settings.  Used only by operations in the parent package.
      -- Do not use in extensions of this package!!!

      -- Operations invoked by this package, which must be overriden to create new session storage schemes.
      procedure Initialize (Settings : in out Context) is abstract;
      -- Performs any initialization steps such that Settings can be considered valid (ready for use).
      -- Raises Invalid_Context if initialization fails.

      -- The following abstract operations should assume that Settings is valid.

      procedure Finalize (Settings : in out Context) is abstract;
      -- Performs any finalization steps once Settings is no longer needed.  This could become an operation
      -- for Context as a controlled type.

      function Exists (Settings : Context; Session : Data) return Boolean is abstract;
      -- Returns whether a session with Identity (Session) already exists in Settings.

      procedure Create (Settings : in out Context; Session : in out Data) is abstract;
      -- Creates Session from Settings.  This may include storing a placeholder for Session.

      procedure Delete (Settings : in out Context; Session : in out Data) is abstract;
      -- Delete Session from Settings.

      procedure Read (Settings : in out Context; Identity : in String; Session : out Data) is abstract;
      -- Read Session from Settings with Identity.

      procedure Write (Settings : in out Context; Session : in out Data) is abstract;
      -- Write Session to Settings.

      procedure Close (Settings : in out Context; Session : in out Data) is abstract;
      -- Close Session without writing to Settings.
   private -- Storage
      -- The context probably needs to be protected somehow to be task-safe for concurrent access by persistent applications.
      -- Currently, the simplest way I can think of to do this is use a binary semaphore (yeah, so sue me!).
      type Context is abstract tagged limited record
         Name     :         Strings.U_String;
         Lifetime :         Duration;
         Lock     : aliased PragmARC.Binary_Semaphore_Handler.Binary_Semaphore;
         Valid    :         Boolean := False;
      end record;

      No_Context : constant Context_Handle := null;
   end Storage;

   procedure Create (Settings : not null Storage.Context_Handle; Session : out Data);

   function Create (Settings : not null Storage.Context_Handle) return Handle;
   -- Returns a new session data associated with the session context in Settings.
   -- Raises Invalid_Context if not Valid (Settings).
   -- Once FSF GNAT supports extended return statements, this operation will likely change to return Data.

   No_Session : constant Handle;

   Expired : exception;

   function Read (From : not null Storage.Context_Handle; Identity : String) return Handle;
   -- Reads session data from the context From with Identity, and assigns the session to To.
   -- Raises Not_Found if the session could not be found.
   -- Raises Expired if the session expired its lifetime.
   -- Once FSF GNAT supports extended return statements, this operation will likely change to return Data.

   -- Stream I/O operations, used by storage implementations to read/write session data.
   procedure Input (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : out Data);
   procedure Output (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : in Data);
private -- Solid.Web.Session
   type Tuple is abstract tagged record
      Key : Solid.Strings.U_String;
   end record;

   function Hash (Element : Tuple'Class) return Ada.Containers.Hash_Type;
   function Equivalent_Elements (Left : Tuple'Class; Right : Tuple'Class) return Boolean;

   package Tuple_Tables is new Ada.Containers.Indefinite_Hashed_Sets (Element_Type        => Tuple'Class,
                                                                      Hash                => Hash,
                                                                      Equivalent_Elements => Equivalent_Elements,
                                                                      "="                 => "=");

   subtype Session_Identity is GNAT.MD5.Message_Digest;
   No_Identity : constant Session_Identity := (others => ASCII.NUL);

   type Data is new Ada.Finalization.Limited_Controlled with record
      Settings : Storage.Context_Handle;
      Identity : Session_Identity  := No_Identity;
      Data_Set : Tuple_Tables.Set;
      Expires  : Ada.Calendar.Time := Calendar.No_Time;
      Modified : Boolean           := False;
   end record;

   overriding
   procedure Finalize (Object : in out Data);

   function Get_Tuple (Session : Data; Key : String) return Tuple'Class;
   -- Returns a class-wide type tuple for Key in Session.
   -- Raises Not_Found if not Exists (Session, Key).
   -- To be used by instantiations of the Tuples package.

   procedure Set_Tuple (Session : in out Data; Item : in Tuple'Class);
   -- Sets Item for Session.
   -- To be used by instantiations of the Tuples package.

   --~ No_Session : constant Data   := (Ada.Finalization.Limited_Controlled with others => <>);
   No_Session : constant Handle := null;
end Solid.Web.Session;
