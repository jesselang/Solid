-- http://hoohoo.ncsa.uiuc.edu/cgi/
with Ada.Streams;

package Solid.CGI is
   type Base_Stream is abstract new Ada.Streams.Root_Stream_Type with null record;
end Solid.CGI;
