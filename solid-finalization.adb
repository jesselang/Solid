with Ada.Finalization;

package body Solid.Finalization is
   type Finalizer is new Ada.Finalization.Limited_Controlled with null record;

   overriding
   procedure Finalize (Object : in out Finalizer);

   overriding
   procedure Finalize (Object : in out Finalizer) is
   begin -- Finalize
      Process;
   end Finalize;

   Ensure_Finalization : Finalizer;
end Solid.Finalization;
