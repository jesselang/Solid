-- An interface to shared libraries on Linux.
with System;

package Solid.Interfaces.Libraries is
   type Handle is private;

   No_Handle : constant Handle;

   function Error return String;
   -- Returns the error information when called immediately after calling any of the following operations.

   function Load (Path : String) return Handle;
   -- Loads the library in Path.
   -- Returns No_Handle if a library identified by Path could not be found.
   -- If Path contains a slash ('/'), then it is interpreted as a (relative or absolute) pathname.
   -- Otherwise, the default library path is searched.

   procedure Unload (Library : in out Handle);
   -- Unloads Library.
   -- Pre:  Library /= No_Handle
   -- Post: Library  = No_Handle

   Not_Found : exception;

   procedure Call (Library : in out Handle; Name : in String);
   -- Calls a procedure identified by Name in Library.
   -- Raises Not_Found if Name could not be found in Library.

   generic -- Find
      type Operation is private;
   function Find (Library : Handle; Name : String) return Operation;
   -- Returns an Operation (an access procedure or access function type) identified by Name in Library.
   -- Raises Not_Found if Name could not be found in Library.
private -- Solid.Interfaces.Libraries
   type Handle is new System.Address;

   No_Handle : constant Handle := Handle (System.Null_Address);
end Solid.Interfaces.Libraries;
