-- Store session data in a file system.  Intended for standard CGI use.
package Solid.CGI.Session.Files is
   type Data is new Session.Data with private;

   overriding
   function Create (Name : String := "Session") return Data;

   overriding
   procedure Read (Session : in out Data);
   -- Reads Session from storage.
   -- Raises Not_Found if Session was not found in storage.

   overriding
   procedure Write (Session : in Data);

private -- Solid.CGI.Session.Files
   type Data is new Session.Data with null record;

   -- Directory?

   -- Object to normalize the Directory formal?
end Solid.CGI.Session.Files;
