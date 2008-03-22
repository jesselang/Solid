package body Solid.Interrupt_Handling is
   protected body Handler is
      entry Wait when Occurred is
      begin -- Wait
         Occurred := False;
      end Wait;

      function Has_Occurred return Boolean is
      begin -- Has_Occurred
         return Count > 0;
      end Has_Occurred;

      procedure Handle is
      begin -- Handle
         Count := Count + 1;
         Occurred := True;
      end Handle;
   end Handler;

   protected body Interrupt_Or_Terminate is
      entry Wait when Occurred is
      begin -- Wait
         Occurred := False;
      end Wait;

      function Has_Occurred return Boolean is
      begin -- Has_Occurred
         return Count > 0;
      end Has_Occurred;

      procedure Handle is
      begin -- Handle
         Count := Count + 1;
         Occurred := True;
      end Handle;
   end Interrupt_Or_Terminate;
end Solid.Interrupt_Handling;
