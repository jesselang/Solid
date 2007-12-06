generic
   type Discrete is (<>);
   with function Get_Value (D : Data) return Discrete;
package Solid.CGI.Request.Discrete_Handler is
   generic
      Value : in Discrete;
      with procedure Process (Item : in out Data);
   procedure Greater_Than (Item : in out Data);

   generic
      Value : in Discrete;
      with procedure Process (Item : in out Data);
   procedure Greater_Than_Or_Equal_To (Item : in out Data);

   generic
      Value : in Discrete;
      with procedure Process (Item : in out Data);
   procedure Equal_To (Item : in out Data);

   generic
      Value : in Discrete;
      with procedure Process (Item : in out Data);
   procedure Equal_To_Or_Less_Than (Item : in out Data);

   generic
      Value : in Discrete;
      with procedure Process (Item : in out Data);
   procedure Less_Than (Item : in out Data);

   generic
      Value : in Discrete;
      with procedure Greater (Item : in out Data);
      with procedure Equal   (Item : in out Data);
      with procedure Less    (Item : in out Data);
   procedure This_That_Or_The_Other (Item : in out Data);
end Solid.CGI.Request.Discrete_Handler;
