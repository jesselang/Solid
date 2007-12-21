with Ada.Strings.Fixed;

package body Solid.CGI.URL is
   --  The general URL form as described in RFC2616 is:
   --
   --  http_URL = "http:" "//" host [ ":" port ] [ abs_path [ "?" query ]]
   --
   --  Note also that there are different RFC describing URL like the 2616 and
   --  1738 but they use different terminologies. Here we try to follow the
   --  names used in RFC2616 but we have implemented some extensions at the
   --  end of this package. For example the way Path and File are separated or
   --  the handling of user/password which is explicitly not allowed in the
   --  RFC but are used and supported in many browsers. Here are the extended
   --  URL supported:
   --
   --  http://username:password@www.here.com:80/dir1/dir2/xyz.html?p=8&x=doh
   --   |                            |       | |          |       |
   --   protocol                     host port path       file    parameters
   --
   --                                          <--  pathname  -->

   -- What about hash? i.e.: "#target"?

   function Query (URL : String) return String is
      First : constant Natural := Ada.Strings.Fixed.Index (Source => URL, Pattern => "?") + 1;

      Last  : Natural := Ada.Strings.Fixed.Index (Source => URL, Pattern => "#");
   begin -- Query
      if First = 0 then
         return "";
      elsif Last = 0 then
         return URL (First .. URL'Last);
      else
         return URL (First .. Last);
      end if;
   end Query;
end Solid.CGI.URL;
