with Ada.Text_IO;
with Solid.Web.Session.Files;
with Solid.Web.Session.Tuples;
with Solid.Strings;
use Solid.Strings;

procedure Session_Test is
   Identity : constant String := "04f7d52d43e1d5d3e5073041111bd294";
   Context : Solid.Web.Session.Storage.Context_Handle := Solid.Web.Session.Files.Initialize (Path => "/tmp/solid-sessions");
   --~ Session : Solid.CGI.Session.Data := Solid.CGI.Session.Create (Context);
   Session : Solid.Web.Session.Handle := Solid.Web.Session.Read (Context, Identity => Identity);
begin -- Session_Test

   --~ Ada.Text_IO.Put_Line (Solid.CGI.Session.Name (Session) & ": " & Solid.CGI.Session.Identity (Session) );
   --Solid.CGI.Session.Tuples.Strings.Set (Session, Key => "Test_Key", Value => +"Test_Value");
   Ada.Text_IO.Put_Line (+Solid.Web.Session.Tuples.Strings.Get (Session.all, Key => "Test_Key") );
   --~ Solid.CGI.Session.Tuples.Numbers.Set (Session, Key => "Test_Key", Value => 373);
   --Ada.Text_IO.Put_Line (Integer'Image (Solid.CGI.Session.Tuples.Numbers.Get (Session, Key => "Test_Key") ) );
   Solid.Web.Session.Tuples.Strings.Set (Session.all, Key => "Test_Key", Value => +"Test_Value");
end Session_Test;
