package body Solid.Data_Structures.Hashed_Multimaps is

   function Length (Container : Map) return Count is
   begin -- Length
      return Count (Map_Implementation.Length (Container.Handle) );
   end Length;

   function Values (Container : Map; Position : Cursor) return Count is
      Values : Element_Implementation.Vector;

      use type Map_Implementation.Cursor;
   begin -- Values
      if Position.Handle = Map_Implementation.No_Element then
         raise Map_Failure;
      else
         Values := Map_Implementation.Element (Position.Handle);
         return Count (Element_Implementation.Length (Values) );
      end if;
   end Values;

   function Values (Container : Map; Key : Map_Key) return Count is
      Position : constant Map_Implementation.Cursor := Map_Implementation.Find (Container.Handle, Key => Key);
   begin -- Values
      return Values (Container => Container, Position => (Handle => Position) );
   end Values;

   procedure Clear (Container : in out Map) is
   begin -- Clear
      Map_Implementation.Clear (Container => Container.Handle);
   end Clear;

   function Exists (Container : Map; Key : Map_Key) return Boolean is
      Position : constant Map_Implementation.Cursor := Map_Implementation.Find (Container.Handle, Key => Key);

      use type Map_Implementation.Cursor;
      use type Ada.Containers.Count_Type;
   begin -- Exists
      if Position = Map_Implementation.No_Element then
         return False;
      else
         return Element_Implementation.Length (Container => Map_Implementation.Element (Position) ) /= 0;
      end if;
      --return Position /= Map_Implementation.No_Element;
   end Exists;

   function Find (Container : Map; Key : Map_Key) return Cursor is
      Position : constant Map_Implementation.Cursor := Map_Implementation.Find (Container.Handle, Key => Key);

      use type Map_Implementation.Cursor;
   begin -- Find
      return Cursor'(Handle => Position);
   end Find;

   function Get (Container : Map; Key : Map_Key; Position : Index := Index'First) return Element is
      Item_Position : constant Map_Implementation.Cursor := Map_Implementation.Find (Container.Handle, Key => Key);

      Values : Element_Implementation.Vector;

      use type Map_Implementation.Cursor;
   begin -- Get
      if Item_Position = Map_Implementation.No_Element then
         raise Map_Failure;
      else
         Values := Map_Implementation.Element (Container.Handle, Key => Key);
         return Element_Implementation.Element (Values, Index => Position);
      end if;
   end Get;

   procedure Append (Container : in out Map;
                     Key       : in     Map_Key;
                     New_Item  : in     Element)
   is
      procedure Append_To_Values (Key : in Map_Key; Values : in out Element_Implementation.Vector) is
      begin -- Append_To_Values
         Element_Implementation.Append (Container => Values, New_Item => New_Item);
      end Append_To_Values;

      Item_Position : constant Map_Implementation.Cursor := Map_Implementation.Find (Container.Handle, Key => Key);

      Values : Element_Implementation.Vector;

      use type Map_Implementation.Cursor;
   begin -- Append
      if Item_Position = Map_Implementation.No_Element then
         Element_Implementation.Append (Container => Values, New_Item => New_Item);
         Map_Implementation.Insert (Container => Container.Handle, Key => Key, New_Item => Values);
      else
         Map_Implementation.Update_Element (Container => Container.Handle,
                                            Position  => Item_Position,
                                            Process   => Append_To_Values'Access);
      end if;
   exception -- Append
      when Constraint_Error | Program_Error =>
         raise Map_Failure;
   end Append;

   procedure Update (Container : in out Map;
                     Key       : in     Map_Key;
                     New_Item  : in     Element;
                     Position  : in     Index := Index'First)
   is
      Item_Position : constant Map_Implementation.Cursor := Map_Implementation.Find (Container.Handle, Key => Key);

      Item : Element_Implementation.Vector;

      use type Map_Implementation.Cursor;
   begin -- Update
      if Item_Position = Map_Implementation.No_Element then
         Append (Container => Container, Key => Key, New_Item => New_Item);
      else
         Item := Map_Implementation.Element (Item_Position);
         Element_Implementation.Replace_Element (Container => Item, Index => Position, New_Item => New_Item);
      end if;
   exception -- Update
      when Constraint_Error | Program_Error =>
         raise Map_Failure;
   end Update;

   function Key (Position : Cursor) return Map_Key is
   begin -- Key
      return Map_Implementation.Key (Position.Handle);
   exception -- Key
      when Constraint_Error =>
         raise Map_Failure;
   end Key;

   procedure Iterate (Container : in Map) is
      Continue : Boolean := True;

      procedure Iteration_Wrapper (Position : in Map_Implementation.Cursor) is
         Wrapped_Position : constant Cursor := (Handle => Position);
      begin -- Iteration_Wrapper
         if not Continue then
            return;
         end if;

         Process (Position => Wrapped_Position, Continue => Continue);
      end Iteration_Wrapper;
   begin -- Iterate
      Map_Implementation.Iterate (Container => Container.Handle, Process => Iteration_Wrapper'Access);
   end Iterate;

   procedure Iterate_Values (Container : in Map; Position : in Cursor)
   is
      Continue : Boolean := True;

      procedure Iteration_Wrapper (Position : in Element_Implementation.Cursor) is
      begin -- Iteration_Wrapper
         if not Continue then
            return;
         end if;

         Process (Value => Element_Implementation.Element (Position), Continue => Continue);
      end Iteration_Wrapper;
   begin -- Iterate_Values
      if Position = No_Element then
         raise Map_Failure with "Iterate_Values: Position = No_Element";
      end if;

      Element_Implementation.Iterate (Container => Map_Implementation.Element (Position  => Position.Handle),
                                      Process   => Iteration_Wrapper'Access);
   end Iterate_Values;
end Solid.Data_Structures.Hashed_Multimaps;
