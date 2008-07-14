with Ada.Unchecked_Conversion;
with Interfaces.C.Strings;

package body Solid.Interfaces.Libraries is
   pragma Linker_Options ("-ldl");
   package C renames Standard.Interfaces.C;

   function Error return String;
   -- Returns the last error from dlopen, dlclose, and dlsym.

   function Error return String is
      function dlerror return C.Strings.chars_ptr;
      pragma Import (C, dlerror);

      Error_String : constant C.Strings.chars_ptr := dlerror;

      use type C.Strings.chars_ptr;
   begin -- Error
      if Error_String = C.Strings.Null_Ptr then
         return "";
      end if;

      return C.Strings.Value (Error_String);
   end Error;

   function Load (Path : String) return Handle is
      function dlopen (filename : C.Strings.chars_ptr; flag : C.int) return Handle;
      pragma Import (C, dlopen);

      RTLD_NOW : constant := 2;
   begin -- Load
      return dlopen (C.Strings.New_String (Path), flag => RTLD_NOW);
   end Load;

   procedure Unload (Library : in out Handle) is
      function dlclose (h : Handle) return C.int;
      pragma Import (C, dlclose);

      Result : C.int;

      use type C.int;
   begin -- Unload
      Result := dlclose (Library);

      if Result = 0 then
         Library := No_Handle;
      end if;
   end Unload;

   function dlsym (h : Handle; symbol : C.Strings.chars_ptr) return System.Address;
   pragma Import (C, dlsym);

   procedure Call (Library : in out Handle; Name : in String) is
      type Operation is access procedure;

      function Convert is new Ada.Unchecked_Conversion (System.Address, Operation);

      Location       : System.Address;
      Implementation : Operation;

      use type System.Address;
   begin -- Call
      Location := dlsym (Library, symbol => C.Strings.New_String (Name) );

      if Location = System.Null_Address then
         raise Not_Found with "Could not find symbol " & '"' & Name & '"' & " [" & Error & ']';
      end if;

      Implementation := Convert (Location);

      Implementation.all;
   end Call;

   function Find (Library : Handle; Name : String) return Operation is
      function Convert is new Ada.Unchecked_Conversion (System.Address, Operation);

      Location       : System.Address;

      use type System.Address;
   begin -- Find
      Location := dlsym (Library, symbol => C.Strings.New_String (Name) );

      if Location = System.Null_Address then
         raise Not_Found with "Could not find symbol " & '"' & Name & '"' & " [" & Error & ']';
      end if;

      return Convert (Location);
   end Find;
end Solid.Interfaces.Libraries;
