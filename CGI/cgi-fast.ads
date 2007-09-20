-- http://www.fastcgi.com/devkit/doc/fcgi-spec.html
package CGI.Fast is
   -- FCGI_VERSION_1 : constant

   type Data_Length is range 0 .. 65_535;
   subtype Pad_Length is Data_Length range 0 .. 255;

   type FastCGI_Record is record
   end record;
   pragma Pack (FastCGI_Record);
   -- All these are unsigned char, which is the same as a byte (8 bits).
        --~ typedef struct {
            --~ unsigned char version;
            --~ unsigned char type;
            --~ unsigned char requestIdB1;
            --~ unsigned char requestIdB0;
            --~ unsigned char contentLengthB1;
            --~ unsigned char contentLengthB0;
            --~ unsigned char paddingLength;
            --~ unsigned char reserved;
            --~ unsigned char contentData[contentLength];
            --~ unsigned char paddingData[paddingLength];
        --~ } FCGI_Record;
end CGI.Fast;
