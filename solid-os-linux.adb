with Ada.Strings.Fixed;
with Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Interfaces.C;
with PragmARC.Images;
with System.OS_Constants;

package body Solid.OS.Linux is
   use Ada;

   function Processors return Processor_Count is
      type Processor_ID is range 0 .. Processor_Count'Last;

      function Parse_ID (Line : String) return Processor_ID;
      -- Parses Line, returning Processor_ID.
      -- Raises Not_Available if the ID could not be parsed.

      function Parse_ID (Line : String) return Processor_ID is
         Index : constant Natural := Strings.Fixed.Index (Line, ": ");
      begin -- Parse_ID
         return Processor_ID'Value (Line (Index + 2 .. Line'Last) );
      end Parse_ID;

      subtype Chip_Index is Processor_ID range 0 .. 31;
      subtype Core_Index is Processor_ID range 0 .. 31;
      type Core_Matrix is array (Chip_Index, Core_Index) of Boolean;

      Matrix      : Core_Matrix     := (others => (others => False) );
      Cores_Found : Boolean         := False;
      Count       : Processor_Count;
      File        : Ada.Text_IO.File_Type;
      Line        : String (1 .. 256);
      Last        : Natural;
      Chip        : Processor_ID;
   begin -- Processors
      Text_IO.Open (File => File, Mode => Text_IO.In_File, Name => "/proc/cpuinfo");

      Read_File : loop
         exit Read_File when Text_IO.End_Of_File (File);

         Text_IO.Get_Line (File => File, Item => Line, Last => Last);

         if Last >= 12 then
            if not Cores_Found and Line (1 .. 9) = "processor" then
               Chip := Parse_ID (Line (1 .. Last) );
               Matrix (Chip, 0) := True;
            elsif Line (1 .. 11) = "physical id" then
               Cores_Found := True;
               Chip := Parse_ID (Line (1 .. Last) );
            elsif Line (1 .. 7) = "core id" then
               Matrix (Chip, Parse_ID (Line (1 .. Last) ) ) := True;
            end if;
         end if;
      end loop Read_File;

      Text_IO.Close (File => File);

      if not Matrix (0, 0) then
         raise Not_Available;
      else
         Count := 1;

         Count_Chips : for Chip_Count in Chip_Index loop
            Count_Cores : for Core_Count in Core_Index loop
               if (Chip_Count /= 0 or Core_Count /= 0) and Matrix (Chip_Count, Core_Count) then
                  Count := Count + 1;
               end if;
            end loop Count_Cores;
         end loop Count_Chips;

         return Count;
      end if;
   end Processors;

   function Load_Average return Load_Info is
      Space : constant String := " ";
      Slash : constant String := "/";

      File      : Ada.Text_IO.File_Type;
      Line      : String (1 .. 64);
      First     : Natural;
      Last      : Natural;
      Delimiter : Natural;
      Result    : Load_Info;
   begin -- Load_Average
      Text_IO.Open (File => File, Mode => Text_IO.In_File, Name => "/proc/loadavg");
      Text_IO.Get_Line (File => File, Item => Line, Last => Last);
      Text_IO.Close (File => File);

      -- One minute load.
      First := 1;
      Delimiter := Ada.Strings.Fixed.Index (Line (First .. Last), Pattern => Space);
      Result.One_Minute := Load_Value'Value (Line (First .. Delimiter - 1) );
      First := Delimiter + 1;

      -- Five minute load.
      Delimiter := Ada.Strings.Fixed.Index (Line (First .. Last), Pattern => Space);
      Result.Five_Minute := Load_Value'Value (Line (First .. Delimiter - 1) );
      First := Delimiter + 1;

      -- Fifteen minute load.
      Delimiter := Ada.Strings.Fixed.Index (Line (First .. Last), Pattern => Space);
      Result.Fifteen_Minute := Load_Value'Value (Line (First .. Delimiter - 1) );
      First := Delimiter + 1;

      -- Running processes.
      Delimiter := Ada.Strings.Fixed.Index (Line (First .. Last), Pattern => Slash);
      Result.Running_Processes := Process_Count'Value (Line (First .. Delimiter - 1) );
      First := Delimiter + 1;

      -- Total processes.
      Delimiter := Ada.Strings.Fixed.Index (Line (First .. Last), Pattern => Space);
      Result.Total_Processes := Process_Count'Value (Line (First .. Delimiter - 1) );

      return Result;
   exception -- Load_Average
      when others =>
         raise Not_Available;
   end Load_Average;

   function Memory_Information return Memory_Info is
      function Parse_Value (Line : String) return Memory_Value;
      -- Parses Line into the memory value.
      -- Raises Not_Available if the ID could not be parsed.

      Colon : constant String := ":";

      function Parse_Value (Line : String) return Memory_Value is
         Colon_Index : constant Natural := Ada.Strings.Fixed.Index (Line, Pattern => ":");
         Last_Space  : constant Natural := Ada.Strings.Fixed.Index (Line, Pattern => " ", Going => Ada.Strings.Backward);
      begin -- Parse_Value
         return Memory_Value (Float'Value (Line (Colon_Index + 1 .. Last_Space - 1) ) / 1_000.0);
      exception -- Parse_Value
         when others =>
            raise Not_Available;
      end Parse_Value;

      File   : Ada.Text_IO.File_Type;
      Line   : String (1 .. 64);
      Last   : Natural;
      Index  : Natural;
      Result : Memory_Info;
   begin -- Memory_Information
      Text_IO.Open (File => File, Mode => Text_IO.In_File, Name => "/proc/meminfo");

      Read_File : loop
         exit Read_File when Text_IO.End_Of_File (File);

         Text_IO.Get_Line (File => File, Item => Line, Last => Last);
         Index := Ada.Strings.Fixed.Index (Line, Pattern => Colon);

         if Line (1 .. Index - 1) = "MemTotal" then
            Result.Total := Parse_Value (Line (1 .. Last) );
         elsif Line (1 .. Index - 1) = "MemFree" then
            Result.Free := Parse_Value (Line (1 .. Last) );
         elsif Line (1 .. Index - 1) = "SwapTotal" then
            Result.Swap_Total := Parse_Value (Line (1 .. Last) );
         elsif Line (1 .. Index - 1) = "SwapFree" then
            Result.Swap_Free := Parse_Value (Line (1 .. Last) );
         end if;
      end loop Read_File;

      Text_IO.Close (File => File);

      return Result;
   end Memory_Information;

   function Get_Address (Network_Interface : String) return Hardware_Address is
      package OS_Constants renames System.OS_Constants;
      use Interfaces;

      function Socket (Family : C.int; Mode : C.int; Protocol : C.int) return C.int;
      pragma Import (C, Socket);

      type Socket_Address is record
         Family : C.short;
         Data   : C.char_array (0 .. 13);
      end record;
      pragma Convention (C, Socket_Address);

      type Interface_Request is record
         Name    : Interfaces.C.char_array (0 .. 15);
         Address : Socket_Address;
      end record;
      pragma Convention (C, Interface_Request);

      SIOCGIFADDR : constant := 16#8927#; -- From /usr/include/bits/ioctls.h.

      function IO_Control (File_Descriptor : Interfaces.C.int; Request : Interfaces.C.int; Argument : access Interface_Request)
      return Interfaces.C.int;
      pragma Import (C, IO_Control, "ioctl");

      Socket_Interface :         C.int;
      Address_Result   : aliased Interface_Request;
      Dummy            :         C.size_t;
      Control_Result   :         C.int;
      Result           :         Hardware_Address;

      subtype Byte_Array_Address is C.char_array (0 .. 5);

      function Convert is new Ada.Unchecked_Conversion (Source => Byte_Array_Address, Target => Hardware_Address);

      use type C.int;
   begin -- Get_Address
      Socket_Interface := Socket (OS_Constants.AF_INET, Mode => OS_Constants.SOCK_STREAM, Protocol => 0);
      C.To_C (Item => Network_Interface, Target => Address_Result.Name, Count => Dummy);
      Control_Result := IO_Control (Socket_Interface, Request => SIOCGIFADDR, Argument => Address_Result'Access);

      if Socket_Interface = -1 then
         return No_Address;
      elsif Control_Result = -1 then
         return No_Address;
      else
         Result := Convert (Address_Result.Address.Data (0 .. 5) );
         return Result;
      end if;
   exception -- Get_Address
      when others =>
         raise Not_Available;
   end Get_Address;

   function Image (Address : Hardware_Address) return String is
      type Byte is range 0 .. 2 ** 8 - 1;
      for Byte'Size use 8;

      function Image is new PragmARC.Images.Signed_Image (Byte);

      type Byte_Array is array (1 .. 6) of Byte;

      Overlay : Byte_Array;
      for Overlay'Address use Address'Address;

      Result : String (1 .. 17) := (others => ':');
      Result_Index : Natural := 1;
   begin -- Image
      for Index in Overlay'Range loop
         Result (Result_Index .. Result_Index + 1) := Image (Overlay (Index), Width => 2, Zero_Filled => True, Base => 16);

         Result_Index := Result_Index + 3;
      end loop;

      return Result;
   exception
      when others =>
         return "";
   end Image;
end Solid.OS.Linux;
