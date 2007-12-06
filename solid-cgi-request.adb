package body Solid.CGI.Request is
   function Method (Request : Data) return Request_Method is
   begin -- Method
      return Request.Method;
   end Method;

   --~ type Identifier_List is array (Request_Identifier) of Boolean;

   --~ protected Identifier_System is
      --~ procedure Seize   (Identifier : out Request_Identifier);
      --~ procedure Release (Identifier : in  Request_Identifier);
   --~ private
      --~ List : Identifier_List    := (others => False);
      --~ Next : Request_Identifier := Request_Identifier'First;
   --~ end Identifier_System;

   --~ procedure Release (Identifier : in Request_Identifier) is
   --~ begin -- Release
      --~ Identifier_System.Release (Identifier => Identifier);
   --~ end Release;

   overriding procedure Initialize (Object : in out Data) is
   begin -- Initialize
      null;
   end Initialize;

   --~ protected body Identifier_System is
      --~ procedure Seize (Identifier : out Request_Identifier) is
      --~ begin -- Seize
         --~ null;
      --~ end Seize;

      --~ procedure Release (Identifier : in Request_Identifier) is
      --~ begin -- Release
         --~ null;
      --~ end Release;
   --~ end Identifier_System;
end Solid.CGI.Request;
