with Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones;
with PragmARC.Date_Handler;
with PragmARC.Mixed_Case;
with Solid.Strings;

package body Solid.CGI.Cookies is
   -- Fri, 31-Dec-2010 23:59:59 GMT
   function Date_Image (Date : Ada.Calendar.Time) return String;
   -- Returns date image in the format acceptable for cookies.

   procedure Set (Headers : in out CGI.Headers.List;
                  Name    : in     String;
                  Value   : in     String;
                  Expires : in     Ada.Calendar.Time := Solid.Calendar.No_Time;
                  Path    : in     String            := "";
                  Domain  : in     String            := "")
   is
      Cookie_Value : Strings.U_String;

      use type Ada.Calendar.Time;
      use Solid.Strings;
   begin -- Set
      Cookie_Value := +Name & '=' & Value;

      if Expires /= Calendar.No_Time then
         Cookie_Value := Cookie_Value & "; expires=" & Date_Image (Expires);
      end if;

      if Path /= "" then
         -- Some data validation would be nice.
         Cookie_Value := Cookie_Value & "; path=" & Path;
      end if;

      if Domain /= "" then
         -- Some data validation would be nice.+
         Cookie_Value := Cookie_Value & "; domain=" & Domain;
      end if;

      Headers.Add (Name => "Set-Cookie", Value => +Cookie_Value);
   end Set;

   function Date_Image (Date : Ada.Calendar.Time) return String is
      Year        : Ada.Calendar.Year_Number;
      Month       : Ada.Calendar.Month_Number;
      Day         : Ada.Calendar.Day_Number;
      Hour        : Ada.Calendar.Formatting.Hour_Number;
      Minute      : Ada.Calendar.Formatting.Minute_Number;
      Second      : Ada.Calendar.Formatting.Second_Number;
      Sub_Second  : Ada.Calendar.Formatting.Second_Duration;
      Leap_Second : Boolean;

      use PragmARC.Date_Handler;
   begin -- Date_Image
      Ada.Calendar.Formatting.Split (Date        => Date,
                                     Year        => Year,
                                     Month       => Month,
                                     Day         => Day,
                                     Hour        => Hour,
                                     Minute      => Minute,
                                     Second      => Second,
                                     Sub_Second  => Sub_Second,
                                     Leap_Second => Leap_Second);
      return PragmARC.Mixed_Case (Day_Name'Image (Day_Of_Week (Date) ) (1 .. 3) ) & ", " & Day_Image (Day) & '-' &
             Month_Image_Short (Month => Month) & '-' & Year_Image_Long (Year => Year) & ' ' & Hour_Image_24 (Hour => Hour) &
             ':' & Minute_Image (Minute => Minute) & ':' &
             Seconds_Image (Seconds => PragmARC.Date_Handler.Minute_Duration (Duration (Second) + Sub_Second) ) & " GMT";
   end Date_Image;
end Solid.CGI.Cookies;
