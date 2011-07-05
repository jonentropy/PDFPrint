// Copyright (c) 2001-2002 Alessandro Briosi
//
// Permission is hereby granted, free of charge, to any person 
// obtaining a copy of this software and associated documentation 
// files (the "Software"), to deal in the Software without 
// restriction, including without limitation the rights to use, copy, 
// modify, merge, publish, distribute, sublicense, and/or sell copies 
// of the Software, and to permit persons to whom the Software is 
// furnished to do so, subject to the following conditions:
// 
// The above copyright notice and this permission notice shall be 
// included in all copies or substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, 
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF 
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND 
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS 
// BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN 
// ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN 
// CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE 
// SOFTWARE.
// 
//
// This software was written by Alessandro Briosi with the 
// assistance of Russell Lang, as an example of how the 
// Ghostscript DLL may be used Delphi.
//

unit gsimage;

interface

uses windows, gsapi,graphics;

type
   TGsImage = record
     handle:Pointer;
     device:Pointer;
     raster:integer; //will it be used?
     format:UINT;
     image:PChar;
     bmih:BITMAPINFOHEADER; //will it be used?
     palette:HPALETTE;
     bytewidth:integer;
     sep:integer; //???
     // Other data I think is not needed
   end;


function image_create(handle:Pointer;device:pointer):TGsImage;
function image_size(width:integer;height:integer;raster:integer;format:UINT;pimage:PChar):boolean;
function image_copy(bm:TBitmap):boolean;

var img:TGsImage;

implementation

function image_create(handle:Pointer;device:pointer):TGsImage;
begin
  //initializes the image record
  img.handle:=handle;
  img.device:=device;
  img.raster:=0;
  img.format:=0;
  img.image:=nil;
  img.bmih.biSize:=sizeof(img.bmih);
  img.bmih.biWidth:=0;
  img.bmih.biHeight:=0;
  img.palette:=0;
  result:=img;
end;

function image_size(width:integer;height:integer;raster:integer;format:UINT;pimage:PChar):boolean;
begin
  // sets the size and other infos
  result:=false;
  img.raster:=raster;
  img.image:=pimage;
  img.format:=format;

  img.bmih.biWidth:=width;
  img.bmih.biHeight:=height;
  img.bmih.biPlanes:=1;
  case (img.format and DISPLAY_COLORS_MASK) of
    DISPLAY_COLORS_NATIVE:
      case (img.format and DISPLAY_DEPTH_MASK) of
        DISPLAY_DEPTH_1:begin
          img.bmih.biBitCount:=1;
          img.bmih.biClrUsed:=2;
          img.bmih.biClrImportant:=2;
        end;
        DISPLAY_DEPTH_4:begin
          img.bmih.biBitCount:=4;
          img.bmih.biClrUsed:=16;
          img.bmih.biClrImportant:=16;
        end;
        DISPLAY_DEPTH_8:begin
          img.bmih.biBitCount:=8;
          img.bmih.biClrUsed:=96;
          img.bmih.biClrImportant:=96;
        end;
        DISPLAY_DEPTH_16:begin
          if (img.format and DISPLAY_ENDIAN_MASK)=DISPLAY_BIGENDIAN then
            begin
              img.bmih.biBitCount:=24;
              img.bmih.biClrUsed:=0;
              img.bmih.biClrImportant:=0;
            end
          else
            begin
              img.bmih.biBitCount:=16;
              img.bmih.biClrUsed:=0;
              img.bmih.biClrImportant:=0;
            end;
        end;
        else exit;
    end;
    DISPLAY_COLORS_GRAY:
      case (img.format and DISPLAY_DEPTH_MASK) of
        DISPLAY_DEPTH_1:begin
          img.bmih.biBitCount:=1;
          img.bmih.biClrUsed:=2;
          img.bmih.biClrImportant:=2;
        end;
        DISPLAY_DEPTH_4:begin
          img.bmih.biBitCount:=4;
          img.bmih.biClrUsed:=16;
          img.bmih.biClrImportant:=16;
        end;
        DISPLAY_DEPTH_8:begin
          img.bmih.biBitCount:=8;
          img.bmih.biClrUsed:=256;
          img.bmih.biClrImportant:=256;
        end;
        else exit;
    end;
    DISPLAY_COLORS_RGB:begin
      if (img.format and DISPLAY_DEPTH_MASK)<>DISPLAY_DEPTH_8 then
        exit;
      if (((img.format and DISPLAY_ALPHA_MASK)=DISPLAY_UNUSED_LAST)and
          ((img.format and DISPLAY_ENDIAN_MASK)=DISPLAY_LITTLEENDIAN)) then

        begin
          img.bmih.biBitCount:=32;
          img.bmih.biClrUsed:=0;
          img.bmih.biClrImportant:=0;
        end
      else
        begin
          img.bmih.biBitCount:=24;
          img.bmih.biClrUsed:=0;
          img.bmih.biClrImportant:=0;
        end;
    end;
    DISPLAY_COLORS_CMYK:begin
      img.bmih.biBitCount:=24;
      img.bmih.biClrUsed:=0;
      img.bmih.biClrImportant:=0;
    end;
  end;
  img.bmih.biCompression := 0;
  img.bmih.biSizeImage := 0;
  img.bmih.biXPelsPerMeter := 0;
  img.bmih.biYPelsPerMeter := 0;
  img.bytewidth := trunc(((img.bmih.biWidth * img.bmih.biBitCount + 31 ) and (65504)) / 8);

//  img->palette = create_palette(img);
  result:=true;
end;

function image_copy(bm:TBitmap):boolean;
var bmip:BITMAPINFO;
    pal:array[0..255]of RGBQUAD;
    which_colors:UINT;
    i:integer;
    bmi:record
      h:BITMAPINFOHEADER;
      pal:array[0..255]of dword;
    end;
    bmap:HBitmap;
    tmp:integer;
begin
  // Copies the information from the image record to the bitmap object
  bm.Width:=img.bmih.biWidth;
  bm.Height:=img.bmih.biHeight;
  // calculates the palette
//  img.palette
//  bm.Palette:=img.palette;
//  bm.IgnorePalette:=true;
  // Assigns the information
  bmip.bmiHeader:=img.bmih;
//  bmip.bmiColors:=[0];
  bmap:=bm.Handle;
  if SetDIBits(0,bmap,0,img.bmih.biHeight,img.image,bmip,DIB_RGB_COLORS)>0 then
    result:=true
  else
    result:=false;
end;

end.
