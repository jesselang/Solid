-- Set-Cookie: RMID=732423sdfs73242; expires=Fri, 31-Dec-2010 23:59:59 GMT; path=/; domain=.example.net
private with Solid.Strings;
with Ada.Calendar;
with Solid.Calendar;
with Solid.CGI.Containers.Tables;
with Solid.CGI.Headers;

package Solid.CGI.Cookies is
   type List is new Solid.CGI.Containers.Tables.Table with null record;

   procedure Set (Headers : in out CGI.Headers.List;
                  Name    : in     String;
                  Value   : in     String;
                  Expires : in     Ada.Calendar.Time := Solid.Calendar.No_Time;
                  Path    : in     String            := "";
                  Domain  : in     String            := "");
   -- Sets a cookie in the list of headers.  The headers must then be used when building a response.
end Solid.CGI.Cookies;
