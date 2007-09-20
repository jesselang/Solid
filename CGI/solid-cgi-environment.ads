package Solid.CGI.Environment is

   type Variable is (Auth_Type,
                     Content_Length,
                     Content_Type,
                     Gateway_Interface,
                     HTTP_Accept,
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
                     Server_Software);
end Solid.CGI.Environment;
