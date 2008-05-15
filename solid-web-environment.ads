-- Abstract representation of the web environment.  See Solid.Web.Standard.Environment for an implementation.

package Solid.Web.Environment is
   type Data is abstract tagged null record;
   type Handle is access Data'Class;

   type Variable is (Auth_Type,
                     Content_Length,
                     Content_Type,
                     Document_Root,
                     Gateway_Interface,
                     HTTP_Accept,
                     HTTP_Cookie,
                     HTTP_Host,
                     HTTP_User_Agent,
                     Path_Info,
                     Path_Translated,
                     Query_String,
                     Remote_Addr,
                     Remote_Host,
                     Remote_Ident,
                     Remote_User,
                     Request_Method,
                     Request_URI,
                     Script_Name,
                     Server_Addr,
                     Server_Admin,
                     Server_Name,
                     Server_Port,
                     Server_Protocol,
                     Server_Signature,
                     Server_Software);

   function Value (Object : Handle; Name : Variable) return String;
   -- Get the web environment variable with Name.
   -- Returns "" (null string) if not found.

   function Value (Object : Handle; Name : String) return String;
   -- Get the web environment variable with Name.
   -- Returns "" (null string) if not found.

   generic -- Iterate
      with procedure Process (Name : in String; Value : in String; Continue : in out Boolean);
   procedure Iterate (Object : in Handle);
   -- Iterates over web environment variables.
   -- Stops iterating when Continue is set to False.

   -- Abstract operation to be overridden.
   function Value (Object : Data; Name : String) return String is abstract;
   type Callback is access procedure (Name : in String; Value : in String; Continue : in out Boolean);
   procedure Iterate_Process (Object : in Data; Process : Callback) is abstract;
end Solid.Web.Environment;
