private with Ada.Containers.Indefinite_Hashed_Sets;
private with GNAT.MD5;
private with PragmARC.Binary_Semaphore_Handler;
private with Solid.Calendar;
with Ada.Calendar;
with Ada.Finalization;
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

   procedure Set_Lifetime (Settings : in out Context'Class; To : in Duration);

   type Data is abstract new Ada.Finalization.Controlled with private;
   -- The session data stores a client's set of data, based on a unique identity.
   -- Any data type may be stored in the session.  See Solid.CGI.Session.[Generic_]Tuples.

   function Create (Settings : not null Context_Handle) return Data'Class;
   -- Returns a new session data associated with the session context in Settings.
   -- Raises Invalid_Context if not Valid (Settings).

   Not_Found : exception;

   procedure Delete (Session : in out Data'Class);

   In_Use : exception;

   procedure Read (From : in out Context'Class; Identity : in String; To : out Data'Class);

   procedure Write (Session : in out Data'Class);

   function Name (Session : Data'Class) return String;
   -- Returns the name of the session's context.
   -- Raises Invalid_Context if Session's context is not valid.

   function Identity (Session : Data'Class) return String;
   -- Returns the identity of Session.

   procedure New_Identity (Session : in out Data'Class);
   -- Generates a new identity for Session.
   -- Raises Invalid_Context if Session's context is not valid.

   function Expires (Session : Data'Class) return Ada.Calendar.Time;
   -- Returns the expiration time of Session.

   -- Tuple operations.
   function Exists (Session : Data'Class; Key : String) return Boolean;
   -- Returns True if a tuple with Key is found in Session, otherwise False.

   procedure Delete (Session : in out Data'Class; Key : in String);
   -- Deletes the tuple with Key in Session.
   -- Raises Not_Found if not Exists (Session, Key).

   procedure Delete_Implementation (Session : in out Data) is abstract;
   -- Deletes Session.
   -- Raises Invalid_Context if Session's context is not valid.
   -- Raises Not_Found if the session could not be located and deleted.

   --function Read_Implementation (From : Context; Identity : String) return Data'Class is abstract;
   procedure Read_Implementation (From : in out Context; Identity : in String; To : out Data'Class) is abstract;
   -- Returns existing session data from the context in From with Identity.
   -- Raises Invalid_Context if Session's context is not valid.
   -- Raises Not_Found if the session data could not be located.

   procedure Write_Implementation (Session : in out Data) is abstract;
   -- Writes Session to its associated context.
   -- Raises Invalid_Context if Session's context is not valid.

   type Handle is access Data'Class;

   -- For commonly-used tuple operations, see Solid.CGI.Session.Tuples.

   -- Operations invoked by this package, which must be overriden to create new session storage schemes.
   procedure Initialize_Implementation (Settings : in out Context) is abstract;
   function New_Session (Settings : Context) return Data'Class is abstract;
   procedure Create_Implementation (Session : in out Data) is abstract;
   -- Creates a new session Data object.
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

   type Data is abstract new Ada.Finalization.Controlled with record
      Settings : Context_Handle;
      Identity : Session_Identity  := No_Identity;
      Data_Set : Tuple_Tables.Set;
      Expires  : Ada.Calendar.Time := Calendar.No_Time;
   end record;

   function Get_Tuple (Session : Data'Class; Key : String) return Tuple'Class;
   -- Returns a class-wide type tuple for Key in Session.
   -- Raises Not_Found if not Exists (Session, Key).
   -- To be used by instantiations of the Tuples package.

   procedure Set_Tuple (Session : in out Data'Class; Item : in Tuple'Class);
   -- Sets Item for Session.
   -- To be used by instantiations of the Tuples package.
end Solid.CGI.Session;
