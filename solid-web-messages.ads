package Solid.Web.Messages is
   -- Shamelessly taken from AWS.Messages.

   HTTP_Version_Token : constant String := "HTTP/";

   -----------------
   -- Status Code --
   -----------------

   type Status_Code is
     (S100, S101, S102,
      --  1xx : Informational - Request received, continuing process

      S200, S201, S202, S203, S204, S205, S206, S207,
      --  2xx : Success - The action was successfully received, understood and
      --  accepted

      S300, S301, S302, S303, S304, S305, S307,
      --  3xx : Redirection - Further action must be taken in order to
      --  complete the request

      S400, S401, S402, S403, S404, S405, S406, S407, S408, S409,
      S410, S411, S412, S413, S414, S415, S416, S417, S422, S423, S424,
      --  4xx : Client Error - The request contains bad syntax or cannot be
      --  fulfilled

      S500, S501, S502, S503, S504, S505, S507
      --  5xx : Server Error - The server failed to fulfill an apparently
      --  valid request
      );

   subtype Informational is Status_Code range S100 .. S102;
   subtype Success       is Status_Code range S200 .. S207;
   subtype Redirection   is Status_Code range S300 .. S307;
   subtype Client_Error  is Status_Code range S400 .. S424;
   subtype Server_Error  is Status_Code range S500 .. S507;

   function Image (S : in Status_Code) return String;
   --  Returns Status_Code image. This value does not contain the leading S

   function Reason_Phrase (S : in Status_Code) return String;
   --  Returns the reason phrase for the status code S, see [RFC 2616 - 6.1.1]
end Solid.Web.Messages;
