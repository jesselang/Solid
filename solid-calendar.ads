with Ada.Calendar;

package Solid.Calendar is
   No_Time    : constant Ada.Calendar.Time := Ada.Calendar.Time_Of (Year  => Ada.Calendar.Year_Number'First,
                                                                    Month => Ada.Calendar.Month_Number'First,
                                                                    Day   => Ada.Calendar.Day_Number'First);
   -- A simple constant which defines an uninitialized value.

   UNIX_Epoch : constant Ada.Calendar.Time := Ada.Calendar.Time_Of (Year  => 1970,
                                                                    Month => 1,
                                                                    Day   => 1);
   -- The classic UNIX epoch, January 1st, 1970.  To be used to calculate the number of seconds
   -- since this monumental yet fictitious moment.
end Solid.Calendar;
