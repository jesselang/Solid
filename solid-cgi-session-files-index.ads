with Ada.Finalization;

private package Solid.CGI.Session.Files.Index is
   function Exists (Session : Data) return Boolean;

   procedure Add (Session : in Data);

   procedure Delete (Session : in out Data);

   function In_Use (Session : Data) return Boolean;

   procedure In_Use (Session : in out Data) is null;

   procedure Not_In_Use (Session : in out Data) is null;
end Solid.CGI.Session.Files.Index;
