-- ADT and operation for HTTP cookies.
with Ada.Calendar;
with Solid.Calendar;
with Solid.CGI.Containers.Tables;
with Solid.CGI.Headers;

package Solid.CGI.Cookies is
   type List is new Solid.CGI.Containers.Tables.Table with null record;
   -- See Solid.CGI.Containers.Tables for inherited operations.

   procedure Set (Headers : in out CGI.Headers.List;
                  Name    : in     String;
                  Value   : in     String;
                  Expires : in     Ada.Calendar.Time := Solid.Calendar.No_Time;
                  Path    : in     String            := "";
                  Domain  : in     String            := "");
   -- Sets a cookie in the list of headers.  The headers must then be used when building a response.
end Solid.CGI.Cookies;
