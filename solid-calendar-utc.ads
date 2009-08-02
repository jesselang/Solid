with Ada.Calendar.Time_Zones;

package Solid.Calendar.UTC is
   type UTC_Time is new Ada.Calendar.Time;
   -- A new time type which should represent a time in the UTC time zone.

   function Clock return UTC_Time;
   -- Returns the current time in the UTC time zone.

   function To_Time (Date : UTC_Time; UTC_Offset : Ada.Calendar.Time_Zones.Time_Offset) return Ada.Calendar.Time;
   -- Returns the localized time for Date using UTC_Offset.

   OS_Offset : constant Ada.Calendar.Time_Zones.Time_Offset;
   -- The UTC offset of the local time on the OS.

   function To_UTC (Date : Ada.Calendar.Time; UTC_Offset : Ada.Calendar.Time_Zones.Time_Offset := OS_Offset) return UTC_Time;
   -- Returns the UTC time for Date using UTC_Offset.
private -- Solid.Calendar.UTC
   OS_Offset : constant Ada.Calendar.Time_Zones.Time_Offset := Ada.Calendar.Time_Zones.Time_Offset'First;
end Solid.Calendar.UTC;
