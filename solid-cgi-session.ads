private with Ada.Containers.Indefinite_Hashed_Sets;
private with GNAT.MD5;
private with PragmARC.Binary_Semaphore_Handler;
private with Solid.Calendar;
with Ada.Calendar;
with Ada.Finalization;
with Ada.Streams;
with Solid.Strings;

package Solid.CGI.Session is
   type Context is abstract tagged limited private;
   type Context_Handle is access Context'Class;
   -- A session context contains the session name, and any resources needed for extension.
   -- Extension is required for storage implementation of session data.

   Invalid_Context : exception;

   procedure Initialize (Settings : in out Context_Handle; Name : String := "Session"; Lifetime : Duration := Duration'Last);
   -- Initializes the session context in Settings, setting the session name to Name.
   -- See the Initialize procedure for the extension you are using, it is often more appropriate.
   -- Raises Invalid_Context if the context could not be initialized.

   function Valid (Settings : Context'Class) return Boolean;
   -- Returns True if Settings was initialized, otherwise False.

   function Lifetime (Settings : Context'Class) return Duration;
   -- Gets the lifetime for session data objects created using Settings.

   procedure Set_Lifetime (Settings : in out Context'Class; To : in Duration);
   -- Sets the lifetime for session data objects created using Settings;

   type Data is new Ada.Finalization.Limited_Controlled with private;
   -- The session data stores a client's set of data, based on a unique identity.
   -- Any data type may be stored in the session.  See Solid.CGI.Session.[Generic_]Tuples.
   -- This type should not be extended.  It is controlled for finalization purposes, and
   -- declared as such in the public part of this specification so that abstract operations
   -- for Context can be defined.

   type Handle is access Data;

   function Create (Settings : not null Context_Handle) return Data;
   -- Returns a new session data associated with the session context in Settings.
   -- Raises Invalid_Context if not Valid (Settings).

   Not_Found : exception;

   procedure Delete (Session : in out Data);
   -- Deletes Session from its associated context.
   -- Raises Not_Found if Session could not be found.

   Expired : exception;

   function Read (From : not null Context_Handle; Identity : String) return Data;
--   procedure Read (From : not null Context_Handle; Identity : in String; To : out Data);
   -- Reads session data from the context From with Identity, and assigns the session to To.
   -- Raises Not_Found if the session could not be found.
   -- Raises Expired if the session expired its lifetime.

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

   -- Stream I/O operations.
   procedure Input (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : out Data);

   procedure Output (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : in Data);


   -- For commonly-used tuple operations, see Solid.CGI.Session.Tuples.

   -- Operations invoked by this package, which must be overriden to create new session storage schemes.
   procedure Initialize (Settings : in out Context) is abstract;
   -- Performs any initialization steps such that Settings can be considered valid (ready for use).

   procedure Finalize (Settings : in out Context) is abstract;
   -- Performs any finalization steps once Settings is no longer needed.  This could become an operation
   -- for Context as a controlled type.

   function Exists (Settings : Context; Session : Data'Class) return Boolean is abstract;
   -- Returns whether a session with Identity (Session) already exists in Settings.

   procedure Create (Settings : in out Context; Session : in out Data'Class) is abstract;
   -- Creates Session from Settings.  This may include storing a placeholder for Session.

   procedure Delete (Settings : in out Context; Session : in out Data'Class) is abstract;
   -- Delete Session from Settings.

   procedure Read (Settings : in out Context; Identity : in String; Session : out Data'Class) is abstract;
   -- Read Session from Settings with Identity.

   procedure Write (Settings : in out Context; Session : in out Data'Class) is abstract;
   -- Write Session to Settings.

   procedure Close (Settings : in out Context; Session : in out Data'Class) is abstract;
   -- Close Session without writing to Settings.
private -- Solid.CGI.Session
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

   -- The context probably needs to be protected somehow to be task-safe for concurrent access by persistent applications.
   -- Currently, the simplest way I can think of to do this is use a binary semaphore (yeah, so sue me!).
   type Context is abstract tagged limited record
      Name     :         Strings.U_String;
      Lifetime :         Duration;
      Lock     : aliased PragmARC.Binary_Semaphore_Handler.Binary_Semaphore;
      Valid    :         Boolean := False;
   end record;

   type Data is new Ada.Finalization.Limited_Controlled with record
      Settings : Context_Handle;
      Identity : Session_Identity  := No_Identity;
      Data_Set : Tuple_Tables.Set;
      Expires  : Ada.Calendar.Time := Calendar.No_Time;
      Modified : Boolean           := False;
      --~ Internal : Boolean           := False;
   end record;

   --~ overriding
   --~ procedure Adjust (Object : in out Data);

   overriding
   procedure Finalize (Object : in out Data);

   function Get_Tuple (Session : Data; Key : String) return Tuple'Class;
   -- Returns a class-wide type tuple for Key in Session.
   -- Raises Not_Found if not Exists (Session, Key).
   -- To be used by instantiations of the Tuples package.

   procedure Set_Tuple (Session : in out Data; Item : in Tuple'Class);
   -- Sets Item for Session.
   -- To be used by instantiations of the Tuples package.
end Solid.CGI.Session;
