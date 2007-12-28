package Solid.CGI.Environment is
   type Data is abstract tagged null record;
   type Handle is access Data'Class;

   type Variable is (Auth_Type,
                     Content_Length,
                     Content_Type,
                     Gateway_Interface,
                     HTTP_Accept,
                     HTTP_Cookies,
                     HTTP_User_Agent,
                     Path_Info,
                     Path_Translated,
                     Query_String,
                     Remote_Addr,
                     Remote_Host,
                     Remote_Ident,
                     Remote_User,
                     Request_Method,
                     Script_Name,
                     Server_Name,
                     Server_Port,
                     Server_Protocol,
                     Server_Signature,
                     Server_Software);

   function Value (Object : Handle; Name : Variable) return String;
   -- Get the CGI environment variable with Name.
   -- Returns "" (null string) if not found.

   function Value (Object : Handle; Name : String) return String;
   -- Get the CGI environment variable with Name.
   -- Returns "" (null string) if not found.

   -- Abstract operation to be overridden.
   function Value (Object : Data; Name : String) return String is abstract;
end Solid.CGI.Environment;
