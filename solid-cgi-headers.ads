with Solid.CGI.Containers.Tables;

package Solid.CGI.Headers is
   type List is new Containers.Tables.Table with null record;
   No_Headers : constant List;

   Content_Type : constant String := "Content-type";
private -- Solid.CGI.Headers
   No_Headers : constant List := (Containers.Tables.Empty with null record);
end Solid.CGI.Headers;
