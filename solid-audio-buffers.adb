package body Solid.Audio.Buffers is
   package body Ring_Buffers is
         protected body Buffer is
            procedure Append (Item : in Sample_Buffer) is
            begin -- Append
               if Item'Length > Data'Length then
                  raise Overflow;
               end if;

               for Index in Item'Range loop
                  if Last = Data'Last then
                     Full := True;
                  end if;

                  Last := Last + 1;
                  Data (Last) := Item (Index);
               end loop;

               if Full then
                  First := Last + 1;
               end if;

               Waiting := False;
            end Append;

            function Length return Buffer_Size is
            begin -- Length
               if First = Buffer_Index'First and Last = Buffer_Index'First then
                  return 0;
               elsif First > Last then
                  return Buffer_Size (Data'Last - First + Last - Data'First + 1) + 1;
               else
                  return Buffer_Size (Last - First + 1);
               end if;
            end Length;

            entry Get (Item : out Sample_Buffer; From : in Buffer_End := Back) when not Waiting is
               Ring_Offset : Buffer_Index;
            begin -- Get
               if Item'Length > Data'Length then
                  raise Overflow;
               elsif Item'Length > Length then
                  requeue Wait;
               end if;

               if From = Front then
                  Ring_Offset := First;
               else
                  Ring_Offset := Last - Buffer_Index (Item'Length);
               end if;

               for Index in 0 .. Item'Length - 1 loop
                  Item (Item'First + Buffer_Size (Index) ) := Data (Ring_Offset + Buffer_Index (Index) );
               end loop;
            end Get;

            entry Wait (Item : out Sample_Buffer; From : in Buffer_End) when not Waiting is
            begin -- Wait
               if Wait'Count = 0 then
                  Waiting := True;
               end if;

               requeue Get with abort;
            end Wait;
         end Buffer;
   end Ring_Buffers;
end Solid.Audio.Buffers;
