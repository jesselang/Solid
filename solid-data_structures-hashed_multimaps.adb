package body Solid.Data_Structures.Hashed_Multimaps is

   procedure Append (Container : in out Map;
                     Key       : in     Map_Key;
                     New_Item  : in     Element)
   is
      procedure Append_To_Values (Key : in Map_Key; Values : in out Element_Implementation.Vector) is
      begin -- Append_To_Values
         Element_Implementation.Append (Container => Values, New_Item => New_Item);
      end Append_To_Values;

      Position : constant Map_Implementation.Cursor := Map_Implementation.Find (Container => Container.Handle, Key => Key);

      Values : Element_Implementation.Vector;

      use type Map_Implementation.Cursor;
   begin -- Append
      if Position = Map_Implementation.No_Element then
         Element_Implementation.Append (Container => Values, New_Item => New_Item);
         Map_Implementation.Insert (Container => Container.Handle, Key => Key, New_Item => Values);
      else
         Map_Implementation.Update_Element (Container => Container.Handle,
                                            Position  => Position,
                                            Process   => Append_To_Values'Access);
      end if;
   end Append;

   function Key (Position : Cursor) return Map_Key is
   begin -- Key
      return Map_Implementation.Key (Position => Position.Handle);
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

   procedure Iterate_Values (Container : in Map;
                             Position  : in Cursor)
   is
      Continue : Boolean := True;

      procedure Iteration_Wrapper (Position : in Element_Implementation.Cursor) is
      begin -- Iteration_Wrapper
         Process (Value => Element_Implementation.Element (Position), Continue => Continue);
      end Iteration_Wrapper;
   begin -- Iterate_Values
      Element_Implementation.Iterate (Container => Map_Implementation.Element (Position  => Position.Handle),
                                      Process   => Iteration_Wrapper'Access);
   end Iterate_Values;
end Solid.Data_Structures.Hashed_Multimaps;
