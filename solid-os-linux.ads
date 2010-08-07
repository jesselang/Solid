-- This package provides information about the resources of a Linux system.
-- A better, cross-platform abstraction would no doubt be handy, but superfluous at this point.

package Solid.OS.Linux is
   Not_Available : exception;

   type Processor_Count is range 1 .. 1_024;

   function Processors return Processor_Count;
   -- Returns the number of processors (cores).
   -- Raises Not_Available if unable to return the result.

   type Load_Value is delta 0.01 range 0.0 .. 200.0;
   type Process_Count is range 0 .. 10_000;

   type Load_Info is record
      One_Minute        : Load_Value;
      Five_Minute       : Load_Value;
      Fifteen_Minute    : Load_Value;
      Running_Processes : Process_Count;
      Total_Processes   : Process_Count;
   end record;

   function Load_Average return Load_Info;
   -- Returns load average information.
   -- Raises Not_Available if unable to return the result.

   type Memory_Value is delta 0.1 range 0.0 .. 1_000_000.0; -- Megabytes.

   type Memory_Info is record
      Total      : Memory_Value;
      Free       : Memory_Value;
      Swap_Total : Memory_Value;
      Swap_Free  : Memory_Value;
   end record;

   function Memory_Information return Memory_Info;
   -- Returns system memory information.
   -- Raises Not_Available if unable to return the result.

   type Hardware_Address is mod 2 ** 48;
   for Hardware_Address'Size use 48;

   No_Address : constant Hardware_Address;

   function Get_Address (Network_Interface : String) return Hardware_Address;
   -- Returns the hardware (MAC) address of Network_Interface.
   -- Returns No_Address if Network_Interface was not found, or the address could not be acquired.

   function Image (Address : Hardware_Address) return String;
   -- Return a string representation of Address.
   -- Returns an empty string ("") if an error occurs.
private -- Solid.OS.Linux
   No_Address : constant Hardware_Address := 0;
end Solid.OS.Linux;
