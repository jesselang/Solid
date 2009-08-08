-- Root package for the web framework portion of this library.
-- A high-level abstraction is offered, making web-based applications easier to develop using this framework.

package Solid.Web is
   -- pragma Pure;

   type Network_Port is new Natural;
   Default_HTTP_Port : constant Network_Port;
private -- Solid.Web
   Default_HTTP_Port : constant Network_Port := 80;
end Solid.Web;
