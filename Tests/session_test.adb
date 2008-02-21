with Ada.Text_IO;
with Solid.CGI.Session.Files;
with Solid.CGI.Session.Tuples;
with Solid.Strings;
use Solid.Strings;

procedure Session_Test is
   Session : Solid.CGI.Session.Data'Class := Solid.CGI.Session.Files.Create;
begin -- Session_Test
   Ada.Text_IO.Put_Line (Session.Name & ": " & Session.Identity);
   Solid.CGI.Session.Tuples.Strings.Set (Session, Key => "Test_Key", Value => +"Test_Value");
   Ada.Text_IO.Put_Line (+Solid.CGI.Session.Tuples.Strings.Get (Session, Key => "Test_Key") );
   Solid.CGI.Session.Tuples.Numbers.Set (Session, Key => "Test_Key", Value => 373);
   Ada.Text_IO.Put_Line (Integer'Image (Solid.CGI.Session.Tuples.Numbers.Get (Session, Key => "Test_Key") ) );
end Session_Test;
