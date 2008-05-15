-- Shamelessly copied from AWS.MIME.
-- Not sure if this is going to stay here.

package Solid.Web.MIME is
   ----------
   -- Text --
   ----------

   Text_CSS                    : constant String := "text/css";
   Text_HTML                   : constant String := "text/html";
   Text_Plain                  : constant String := "text/plain";
   Text_XML                    : constant String := "text/xml";
   Text_X_SGML                 : constant String := "text/x-sgml";

   -----------
   -- Image --
   -----------

   Image_Gif                   : constant String := "image/gif";
   Image_Jpeg                  : constant String := "image/jpeg";
   Image_Png                   : constant String := "image/png";
   Image_Tiff                  : constant String := "image/tiff";
   Image_X_Portable_Anymap     : constant String := "image/x-portable-anymap";
   Image_X_Portable_Bitmap     : constant String := "image/x-portable-bitmap";
   Image_X_Portable_Graymap    : constant String := "image/x-portable-graymap";
   Image_X_Portable_Pixmap     : constant String := "image/x-portable-pixmap";
   Image_X_RGB                 : constant String := "image/x-rgb";
   Image_X_Xbitmap             : constant String := "image/x-xbitmap";
   Image_X_Xpixmap             : constant String := "image/x-xpixmap";
   Image_X_Xwindowdump         : constant String := "image/x-xwindowdump";

   -----------------
   -- Application --
   -----------------

   Application_Postscript      : constant String := "application/postscript";
   Application_Pdf             : constant String := "application/pdf";
   Application_Zip             : constant String := "application/zip";
   Application_Octet_Stream    : constant String := "application/octet-stream";
   Application_Form_Data       : constant String := "application/x-www-form-urlencoded";
   Application_Mac_Binhex40    : constant String := "application/mac-binhex40";
   Application_Msword          : constant String := "application/msword";
   Application_Powerpoint      : constant String := "application/powerpoint";
   Application_Rtf             : constant String := "application/rtf";
   Application_X_Compress      : constant String := "application/x-compress";
   Application_X_GTar          : constant String := "application/x-gtar";
   Application_X_GZip          : constant String := "application/x-gzip";
   Application_X_Latex         : constant String := "application/x-latex";
   Application_X_Sh            : constant String := "application/x-sh";
   Application_X_Shar          : constant String := "application/x-shar";
   Application_X_Tar           : constant String := "application/x-tar";
   Application_X_Tcl           : constant String := "application/x-tcl";
   Application_X_Tex           : constant String := "application/x-tex";
   Application_X_Texinfo       : constant String := "application/x-texinfo";
   Application_X_Troff         : constant String := "application/x-troff";
   Application_X_Troff_Man     : constant String := "application/x-troff-man";

   -----------
   -- Audio --
   -----------

   Audio_Basic                 : constant String := "audio/basic";
   Audio_Mpeg                  : constant String := "audio/mpeg";
   Audio_X_Wav                 : constant String := "audio/x-wav";
   Audio_X_Pn_Realaudio        : constant String := "audio/x-pn-realaudio";
   Audio_X_Pn_Realaudio_Plugin : constant String := "audio/x-pn-realaudio-plugin";
   Audio_X_Realaudio           : constant String := "audio/x-realaudio";

   -----------
   -- Video --
   -----------

   Video_Mpeg                  : constant String := "video/mpeg";
   Video_Quicktime             : constant String := "video/quicktime";
   Video_X_Msvideo             : constant String := "video/x-msvideo";

   ---------------
   -- Multipart --
   ---------------

   Multipart_Form_Data         : constant String := "multipart/form-data";
   Multipart_Byteranges        : constant String := "multipart/byteranges";
   Multipart_Related           : constant String := "multipart/related";
   Multipart_X_Mixed_Replace   : constant String := "multipart/x-mixed-replace";
end Solid.Web.MIME;
