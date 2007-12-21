with Solid.Strings;
use Solid.Strings;

package body Solid.CGI.Containers.Tables is
   function Size (Container : Table) return Count is
   begin -- Size
      return Count (Implementation.Length (Container.Handle) );
   end Size;

   function Exist (Container : Table; Name : String) return Boolean is
   begin -- Exist
      return Implementation.Exist (Container.Handle, Key => +Name);
   end Exist;

   function Get (Container : Table; Name : String; Position : Index := Index'First) return String is
      Value_Index : constant Implementation.Index := Implementation.Index (Position);
   begin -- Get
      return -Implementation.Get (Container.Handle, Key => +Name, Position => Value_Index);
   end Get;

   procedure Iterate (Container : in Table) is
      procedure Keys (Position : in Implementation.Cursor; Continue : in out Boolean);

      procedure Iterate_Keys is new Implementation.Iterate (Process => Keys);

      procedure Keys (Position : in Implementation.Cursor; Continue : in out Boolean) is
         Value_Array : String_Array (Index'First .. Index (Implementation.Values (Container.Handle, Key => +Name) ) );
         Value_Index : Index := Index'First;

         procedure Values (Value : in U_String; Continue : in out Boolean);

         procedure Iterate_Values is new Implementation.Iterate_Values (Process => Values);

         procedure Values (Value : in U_String; Continue : in out Boolean) is
         begin -- Values
            Value_Array (Value_Index) := Value;
            Value_Index := Value_Index + 1;
         end Values;

      begin -- Keys
         Iterate_Values (Container => Container.Handle, Position => Position);
         Process (Name => -Implementation.Key (Position), Values => Value_Array);
      end Keys;
   begin -- Iterate
      Iterate_Keys (Container => Container.Handle);
   end Iterate;

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
end Solid.CGI.Containers.Tables;
