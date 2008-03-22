-- Simple interrupt handler. Handles a single interrupt. Tasks may wait for the interrupt to occur.
-- One task is released per occurrence.

with Ada.Interrupts.Names;

package Solid.Interrupt_Handling is
   protected type Handler (Interrupt : Ada.Interrupts.Interrupt_ID) is
      entry Wait;
      -- Allows a task to block until Interrupt occurs. One task is released per occurrence.

      function Has_Occurred return Boolean;
      -- Returns True if Interrupt has occurred at least once.  Otherwise, returns False.
   private -- Handler
      procedure Handle;
      pragma Attach_Handler (Handle, Interrupt);

      Occurred : Boolean := False;
      Count    : Natural := 0;
   end Handler;

   protected type Interrupt_Or_Terminate is
      entry Wait;
      -- Allows a task to block until an interrupt or terminate signal occurs. One task is released per occurrence.

      function Has_Occurred return Boolean;
      -- Returns True if an interrupt or terminate signal has occurred at least once.  Otherwise, returns False.
   private -- Interrupt_Or_Terminate
      procedure Handle;
      pragma Unreserve_All_Interrupts;
      pragma Attach_Handler (Handle, Ada.Interrupts.Names.SIGINT);
      pragma Attach_Handler (Handle, Ada.Interrupts.Names.SIGTERM);

      Occurred : Boolean := False;
      Count    : Natural := 0;
   end Interrupt_Or_Terminate;
end Solid.Interrupt_Handling;
