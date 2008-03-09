private package Solid.CGI.Session.Files.Storage is
   -- These operations should only be used when the corresponding entry in the index has been marked "in use".

   procedure Write (Session : in out Data) is null;

   procedure Delete (Session : in out Data) is null;
end Solid.CGI.Session.Files.Storage;
