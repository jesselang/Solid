with Solid.Strings;
use Solid.Strings;

package body Solid.CGI.Containers.Tables.Set is
   procedure Add (Container : in out Table; Name : in String; Value : in String) is
   begin -- Add
      Implementation.Append (Container => Container.Handle, Key => +Name, New_Item => +Value);
   end Add;

   procedure Update (Container : in out Table; Name : in String; Value : in String; Position : in Index := Index'First) is
      Item_Position : constant Implementation.Index := Implementation.Index (Position);
   begin -- Update
      Implementation.Update (Container => Container.Handle, Key => +Name, New_Item => +Value, Position => Item_Position);
   end Update;

   procedure Clear (Container : in out Table) is
   begin -- Clear
      Implementation.Clear (Container => Container.Handle);
   end Clear;
end Solid.CGI.Containers.Tables.Set;
