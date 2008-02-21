-- This is free software; you can redistribute it and/or modify it under
-- terms of the GNU General Public License as published by the Free Soft-
-- ware Foundation; either version 2, or (at your option) any later ver-
-- sion.  Singo_RC is distributed in the hope that it will be useful, but WITH-
-- OUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
-- for more details.  You should have received a copy of the GNU General
-- Public License distributed with Singo_RC.  If not, write
-- to the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
-- MA 02111-1307, USA.
--
-- As a special exception, if other files instantiate generics from this
-- unit, or you link this unit with other files to produce an executable,
-- this unit does not by itself cause the resulting executable to be
-- covered by the GNU General Public License. This exception does not
-- however invalidate any other reasons why the executable file might be
-- covered by the GNU Public License.

-- Operations for obtaining desired byte orderings.
-- Written by Chip Richards, modifications by Jeffrey Carter.
-- This package assumes that System.Default_Bit_Order also defines the byte ordering.

pragma License (Modified_GPL);

generic -- Solid.Platform.Endian
   type Discrete is (<>);
package Solid.Platform.Endian is
   function Swap (From : Discrete) return Discrete;
   -- Reverses the byte ordering of From.

   function To_Big (From : Discrete) return Discrete;
   -- If System.Default_Bit_Order = System.High_Order_First, returns From;
   -- otherwise, returns From with its bytes reversed.

   function To_Little (From : Discrete) return Discrete;
   -- If System.Default_Bit_Order = System.Low_Order_First, returns From;
   -- otherwise, returns From with its bytes reversed.

   function From_Big    (From : Discrete) return Discrete renames To_Big;
   function From_Little (From : Discrete) return Discrete renames To_Little;
end Solid.Platform.Endian;
