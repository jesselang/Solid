with Solid.Strings;
use Solid.Strings;

package body Solid.Web.Containers.Tables is
   function Size (Container : Table) return Count is
   begin -- Size
      return Count (Implementation.Length (Container.Handle) );
   end Size;

   function Exists (Container : Table; Name : String) return Boolean is
   begin -- Exists
      return Implementation.Exists (Container.Handle, Key => +Name);
   end Exists;

   function Get (Container : Table; Name : String; Position : Index := Index'First) return String is
      Value_Index : constant Implementation.Index := Implementation.Index (Position);
   begin -- Get
      return +Implementation.Get (Container.Handle, Key => +Name, Position => Value_Index);
   exception -- Get
      when Data_Structures.Map_Failure =>
         return "";
   end Get;

   procedure Iterate (Container : in Table'Class) is
      procedure Keys (Position : in Implementation.Cursor; Continue : in out Boolean);

      procedure Iterate_Keys is new Implementation.Iterate (Process => Keys);

      procedure Keys (Position : in Implementation.Cursor; Continue : in out Boolean) is
         Value_Array : String_Array (Positive'First ..
                                     Positive (Implementation.Values (Container.Handle, Position => Position) ) );
         Value_Index : Positive := Positive'First;

         procedure Values (Value : in U_String; Continue : in out Boolean);

         procedure Iterate_Values is new Implementation.Iterate_Values (Process => Values);

         procedure Values (Value : in U_String; Continue : in out Boolean) is
         begin -- Values
            Value_Array (Value_Index) := Value;
            Value_Index := Value_Index + 1;
         end Values;

      begin -- Keys
         Iterate_Values (Container => Container.Handle, Position => Position);
         Process (Name => +Implementation.Key (Position), Values => Value_Array, Continue => Continue);
      end Keys;
   begin -- Iterate
      Iterate_Keys (Container => Container.Handle);
   end Iterate;

   procedure Iterate_Values (Container : in Table'Class; Name : in String) is
      Position : constant Implementation.Cursor := Implementation.Find (Container => Container.Handle, Key => +Name);

      procedure Iteration_Wrapper (Value : in Strings.U_String; Continue : in out Boolean);

      procedure Iterate is new Implementation.Iterate_Values (Process => Iteration_Wrapper);

      procedure Iteration_Wrapper (Value : in Strings.U_String; Continue : in out Boolean) is
      begin -- Iteration_Wrapper
         Process (Value => +Value, Continue => Continue);
      end Iteration_Wrapper;

      use type Implementation.Cursor;
   begin -- Iterate_Values
      if Position = Implementation.No_Element then
         raise Table_Failure with "Iterate_Values: " & Name & " not found in container.";
      end if;

      Iterate (Container => Container.Handle, Position => Position);
   end Iterate_Values;

   procedure Add (Container : in out Table; Name : in String; Value : in String) is
   begin -- Add
      Implementation.Append (Container => Container.Handle, Key => +Name, New_Item => +Value);
   exception -- Add
      when Data_Structures.Map_Failure =>
         raise Table_Failure;
   end Add;

   procedure Update (Container : in out Table; Name : in String; Value : in String; Position : in Index := Index'First) is
      Item_Position : constant Implementation.Index := Implementation.Index (Position);
   begin -- Update
      Implementation.Update (Container => Container.Handle, Key => +Name, New_Item => +Value, Position => Item_Position);
   exception -- Update
      when Data_Structures.Map_Failure =>
         raise Table_Failure;
   end Update;

   procedure Clear (Container : in out Table) is
   begin -- Clear
      Implementation.Clear (Container => Container.Handle);
   end Clear;
end Solid.Web.Containers.Tables;
