with Ada.Strings.Fixed;
with Ada.Text_IO;
with PragmARC.Images.Image;

package body Solid.Debug.Analyzer is
   Intersect : constant Character := '+';
   Line      : constant Character := '-';
   Border    : constant Character := '|';

   type Bit_Overlay is array (Positive range <>) of Boolean;
   pragma Pack (Bit_Overlay);

   procedure Display (Item : in Data; Width : in Positive := Positive'Last) is
      Display_Width : constant Positive := Positive'Min (Item'Size, Width);

      procedure Display_Line is
         use Ada.Strings.Fixed;
      begin -- Display_Line
         Ada.Text_IO.Put (Intersect);
         Ada.Text_IO.Put (Display_Width * Line);
         Ada.Text_IO.Put (Intersect);
         Ada.Text_IO.New_Line;
      end Display_Line;

      procedure Display_Header is
      begin -- Display_Header
         Display_Line;
         Ada.Text_IO.Put (Border);

         for Index in 1 .. Display_Width loop
            if (Index - 1) rem 10 = 0 then
               Ada.Text_IO.Put (PragmARC.Images.Image ( (Index - 1) / 10 ) );
            else
               Ada.Text_IO.Put (' ');
            end if;
         end loop;

         Ada.Text_IO.Put (Border);
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put (Border);

         for Index in 1 .. Display_Width loop
            Ada.Text_IO.Put (PragmARC.Images.Image ( (Index - 1) rem 10 ) );
         end loop;

         Ada.Text_IO.Put (Border);
         Ada.Text_IO.New_Line;

         Display_Line;
      end Display_Header;

      procedure Display_Overlay is
         Overlay : Bit_Overlay (1 .. Item'Size);
         for Overlay'Address use Item'Address;
         Finish_Row : Natural;

         use Ada.Strings.Fixed;
      begin -- Display_Overlay
         Ada.Text_IO.Put (Border);

         for Bit in Overlay'Range loop
            if Overlay (Bit) then
               Ada.Text_IO.Put ("1");
            else
               Ada.Text_IO.Put ("0");
            end if;

            if Bit rem Display_Width = 0 then
               Ada.Text_IO.Put (Border);
               Ada.Text_IO.New_Line;

               if Bit < Overlay'Last then
                  Ada.Text_IO.Put (Border);
               end if;
            end if;
         end loop;

         Finish_Row := Display_Width - (Overlay'Last rem Display_Width);

         if Finish_Row > 0 and Finish_Row < Display_Width then
            Ada.Text_IO.Put (Finish_Row * Ada.Strings.Space);
            Ada.Text_IO.Put (Border);
            Ada.Text_IO.New_Line;
         end if;

         Display_Line;
      end Display_Overlay;
   begin -- Display
      Display_Header;
      Display_Overlay;
   end Display;
end Solid.Debug.Analyzer;
