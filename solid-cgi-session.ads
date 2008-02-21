private with Ada.Containers.Indefinite_Hashed_Sets;
private with GNAT.MD5;
with Solid.Strings;

package Solid.CGI.Session is
   type Data is abstract tagged private;

   function Create (Name : String := "Session") return Data is abstract;
   -- Creates a new session Data object, using Name.

   procedure Read (Session : in out Data) is abstract;

   procedure Write (Session : in Data) is abstract;

   function Name (Session : Data'Class) return String;

   procedure Set_Name (Session : in out Data'Class; To : in String);

   function Identity (Session : Data'Class) return String;

   procedure New_Identity (Session : in out Data'Class);

   function Exists (Session : Data'Class; Key : String) return Boolean;

   Not_Found : exception;

   procedure Delete (Session : in out Data'Class; Key : in String);
   -- Raises Not_Found if not Exists (Session, Key).

   type Handle is access Data'Class;

   -- For commonly-used tuple operations, see Solid.CGI.Session.Tuples.
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

   type Data is abstract tagged record
      Name     : Solid.Strings.U_String; -- Cookie name.
      Identity : Session_Identity := No_Identity;
      Tuples   : Tuple_Tables.Set;
   end record;

   function Get_Tuple (Session : Data'Class; Key : String) return Tuple'Class;
   -- Returns a class-wide type tuple for Key in Session.
   -- Raises Not_Found if not Exists (Session, Key).
   -- To be used by instantiations of the Tuples package.

   procedure Set_Tuple (Session : in out Data'Class; Item : in Tuple'Class);
   -- Sets Item for Session.
   -- To be used by instantiations of the Tuples package.
end Solid.CGI.Session;
