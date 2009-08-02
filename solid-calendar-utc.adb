package body Solid.Calendar.UTC is
   function Clock return UTC_Time is
   begin -- Clock
      return To_UTC (Ada.Calendar.Clock);
   end Clock;

   function To_Time (Date : UTC_Time; UTC_Offset : Ada.Calendar.Time_Zones.Time_Offset) return Ada.Calendar.Time is
      use type Ada.Calendar.Time;
      use type Ada.Calendar.Time_Zones.Time_Offset;
   begin -- To_Time
      return Ada.Calendar.Time (Date) + Duration (60 * UTC_Offset);
   end To_Time;

   function To_UTC (Date : Ada.Calendar.Time; UTC_Offset : Ada.Calendar.Time_Zones.Time_Offset := OS_Offset) return UTC_Time is
      use type Ada.Calendar.Time;
      use type Ada.Calendar.Time_Zones.Time_Offset;
   begin -- To_UTC
      if UTC_Offset = OS_Offset then
         return UTC_Time (Date - Duration (60 * Ada.Calendar.Time_Zones.UTC_Time_Offset (Date) ) );
      else
         return UTC_Time (Date - Duration (60 * UTC_Offset) );
      end if;
   end To_UTC;
end Solid.Calendar.UTC;
