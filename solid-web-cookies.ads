-- ADT and operation for HTTP cookies.
with Ada.Calendar;
with Solid.Calendar;
with Solid.Web.Containers.Tables;
with Solid.Web.Headers;

package Solid.Web.Cookies is
   type List is new Solid.Web.Containers.Tables.Table with null record;
   -- See Solid.Web.Containers.Tables for inherited operations.

   procedure Set (Headers : in out Web.Headers.List;
                  Name    : in     String;
                  Value   : in     String;
                  Expires : in     Ada.Calendar.Time := Solid.Calendar.No_Time;
                  Path    : in     String            := "";
                  Domain  : in     String            := "");
   -- Sets a cookie in the list of headers.  The headers must then be used when building a response.
end Solid.Web.Cookies;
