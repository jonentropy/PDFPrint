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

unit gsapi;

{$mode delphi}

interface

uses windows;

// {$HPPEMIT '#include <iminst.h>'}

const gsdll32 = 'gsdll32.dll';

  STDIN_BUF_SIZE = 128;
  STDOUT_BUF_SIZE = 128;
  STDERR_BUF_SIZE = 128;

  DISPLAY_VERSION_MAJOR = 1;
  DISPLAY_VERSION_MINOR = 0;

//* Define the color space alternatives */
    DISPLAY_COLORS_NATIVE = $01;
    DISPLAY_COLORS_GRAY   = $02;
    DISPLAY_COLORS_RGB    = $04;
    DISPLAY_COLORS_CMYK   = $08;

    DISPLAY_COLORS_MASK  = $000f;

//* Define whether alpha information, or an extra unused bytes is included */
//* DISPLAY_ALPHA_FIRST and DISPLAY_ALPHA_LAST are not implemented */
    DISPLAY_ALPHA_NONE   = $00;
    DISPLAY_ALPHA_FIRST  = $10;
    DISPLAY_ALPHA_LAST   = $20;
    DISPLAY_UNUSED_FIRST = $40;	 //* e.g. Mac xRGB */
    DISPLAY_UNUSED_LAST  = $80;	//* e.g. Windows BGRx */

    DISPLAY_ALPHA_MASK  = $0070;

// * Define the depth per component for DISPLAY_COLORS_GRAY,
// * DISPLAY_COLORS_RGB and DISPLAY_COLORS_CMYK,
// * or the depth per pixel for DISPLAY_COLORS_NATIVE
// * DISPLAY_DEPTH_2 and DISPLAY_DEPTH_12 have not been tested.
// *
    DISPLAY_DEPTH_1   = $0100;
    DISPLAY_DEPTH_2   = $0200;
    DISPLAY_DEPTH_4   = $0400;
    DISPLAY_DEPTH_8   = $0800;
    DISPLAY_DEPTH_12  = $1000;
    DISPLAY_DEPTH_16  = $2000;
    //* unused (1<<14) */
    //* unused (1<<15) */

    DISPLAY_DEPTH_MASK  = $ff00;


// * Define whether Red/Cyan should come first,
// * or whether Blue/Black should come first
// */
    DISPLAY_BIGENDIAN    = $00000;	//* Red/Cyan first */
    DISPLAY_LITTLEENDIAN = $10000;	//* Blue/Black first */

    DISPLAY_ENDIAN_MASK  = $00010000;

//* Define whether the raster starts at the top or bottom of the bitmap */
    DISPLAY_TOPFIRST    = $00000;	//* Unix, Mac */
    DISPLAY_BOTTOMFIRST = $20000;	//* Windows */

    DISPLAY_FIRSTROW_MASK = $00020000;


//* Define whether packing RGB in 16-bits should use 555
// * or 565 (extra bit for green)
// */
    DISPLAY_NATIVE_555 = $00000;
    DISPLAY_NATIVE_565 = $40000;
    DISPLAY_555_MASK  = $00040000;

type
  TGSAPIrevision = packed record
    product:PChar;
    copyright:PChar;
    revision:longint;
    revisiondat:longint;
  end;
  PGSAPIrevision = ^TGSAPIrevision;

  // I couldn't understand what exactly was in this structure so resolved
  // doing a pointer with space wide enough
  Pgs_main_instance = Pointer;

  TStdioFunction = function(caller_handle:Pointer;buf:PChar;len:integer):integer stdcall;
  TPollFunction = function(caller_handle:Pointer):integer stdcall;

  TDisplayEvent = function(handle:Pointer;device:Pointer):integer; cdecl;
  TDisplayPreResizeEvent = function(handle:Pointer;device:Pointer;
         width:integer;height:integer;raster:integer;format:UINT):integer;cdecl;
  TDisplayResizeEvent = function(handle:Pointer;device:Pointer;
         width:integer;height:integer;raster:integer;format:UINT;pimage:PChar):integer;cdecl;
  TDisplayPageEvent = function(handle:Pointer;device:Pointer;copies:integer;flush:integer):integer;cdecl;
  TDisplayUpdateEvent = function(handle:Pointer;device:Pointer;x:integer;y:integer;w:integer;h:integer):integer;cdecl;
  TDisplayMemAlloc = procedure(handle:Pointer;device:Pointer;size:ulong);cdecl;
  TDisplayMemFree = function(handle:Pointer;device:Pointer;mem:Pointer):integer;cdecl;

  PDisplayEvent = ^TDisplayEvent;
  PDisplayPreResizeEvent = ^TDisplayPreResizeEvent;
  PDisplayResizeEvent = ^TDisplayResizeEvent;
  PDisplayPageEvent = ^TDisplayPageEvent;
  PDisplayUpdateEvent = ^TDisplayUpdateEvent;
  PDisplayMemAlloc = ^TDisplayMemAlloc;
  PDisplayMemFree = ^TDisplayMemFree;

  TDisplayCallback = packed record
    size:integer;
    version_major:integer;
    version_minor:integer;
    // New device has been opened */
    // This is the first event from this device. */
    display_open:TDisplayEvent;
    // Device is about to be closed. */
    // Device will not be closed until this function returns. */
    display_preclose:TDisplayEvent;
    // Device has been closed. */
    // This is the last event from this device. */
    display_close:TDisplayEvent;
    // Device is about to be resized. */
    // Resize will only occur if this function returns 0. */
    // raster is byte count of a row. */
    display_presize:TDisplayPreResizeEvent;
    // Device has been resized. */
    // New pointer to raster returned in pimage */
    display_size:TDisplayResizeEvent;

    // flushpage */
    display_sync:TDisplayEvent;

    // showpage */
    // If you want to pause on showpage, then don't return immediately */
    display_page:TDisplayPageEvent;

    // Notify the caller whenever a portion of the raster is updated. */
    // This can be used for cooperative multitasking or for
    // progressive update of the display.
    // This function pointer may be set to NULL if not required.
    //
    display_update:TDisplayUpdateEvent;

    // Allocate memory for bitmap */
    // This is provided in case you need to create memory in a special
    // way, e.g. shared.  If this is NULL, the Ghostscript memory device
    // allocates the bitmap. This will only called to allocate the
    // image buffer. The first row will be placed at the address
    // returned by display_memalloc.
    //

    display_memalloc:TDisplayMemAlloc;

    // Free memory for bitmap */
    // If this is NULL, the Ghostscript memory device will free the bitmap */
    display_memfree:TDisplayMemFree;

  end;
  PDisplayCallback = ^TDisplayCallback;
  PPChar = array of PChar;

{$EXTERNALSYM gsapi_revision}
function gsapi_revision(pr:PGSAPIrevision; len:integer):integer; stdcall;
{$EXTERNALSYM gsapi_new_instance}
function gsapi_new_instance(pinstance:Pgs_main_instance;caller_handle:Pointer):Integer; stdcall;
{$EXTERNALSYM gsapi_delete_instance}
procedure gsapi_delete_instance(pinstance:Pgs_main_instance); stdcall;
{$EXTERNALSYM gsapi_set_stdio}
function gsapi_set_stdio(pinstance:Pgs_main_instance;
                         stdin_fn:TStdioFunction; stdout_fn:TStdioFunction;
                         stderr_fn:TStdioFunction):Integer; stdcall;
{$EXTERNALSYM gsapi_set_poll}
function gsapi_set_poll(pinstance:Pgs_main_instance;poll_fn:TPollFunction):Integer; stdcall;
{$EXTERNALSYM gsapi_set_display_callback}
function gsapi_set_display_callback(pinstance:Pgs_main_instance;callback:PDisplayCallback):Integer; stdcall;
{$EXTERNALSYM gsapi_init_with_args}
function gsapi_init_with_args(pinstance:Pgs_main_instance;argc:integer;argv:PPChar):integer; stdcall;
{$EXTERNALSYM gsapi_run_string_begin}
function gsapi_run_string_begin(pinstance:Pgs_main_instance;user_errors:integer;pexit_code:Pinteger):integer; stdcall;
{$EXTERNALSYM gsapi_run_string_continue}
function gsapi_run_string_continue(pinstance:Pgs_main_instance;str:PChar;len:integer;user_errors:integer;pexit_code:pinteger):integer; stdcall;
{$EXTERNALSYM gsapi_run_string_end}
function gsapi_run_string_end(pinstance:Pgs_main_instance;user_errors:integer;pexit_code:pinteger):integer; stdcall;
{$EXTERNALSYM gsapi_run_string_with_length}
function gsapi_run_string_with_length(pinstance:Pgs_main_instance;str:PChar;len:integer;user_errors:integer;pexit_code:pinteger):integer; stdcall;
{$EXTERNALSYM gsapi_run_string}
function gsapi_run_string(pinstance:Pgs_main_instance;str:PChar;user_errors:integer;pexit_code:pinteger):integer; stdcall;
{$EXTERNALSYM gsapi_run_file}
function gsapi_run_file(pinstance:Pgs_main_instance;file_name:PChar;user_errors:integer;pexit_code:pinteger):integer; stdcall;
{$EXTERNALSYM gsapi_exit}
function gsapi_exit(pinstance:Pgs_main_instance):integer; stdcall;


implementation

{$EXTERNALSYM gsapi_revision}
function gsapi_revision; stdcall; external gsdll32 name 'gsapi_revision';
{$EXTERNALSYM gsapi_new_instance}
function gsapi_new_instance; stdcall; external gsdll32 name 'gsapi_new_instance';
{$EXTERNALSYM gsapi_new_instance}
procedure gsapi_delete_instance; stdcall; external gsdll32 name 'gsapi_delete_instance';
{$EXTERNALSYM gsapi_set_stdio}
function gsapi_set_stdio; stdcall; external gsdll32 name 'gsapi_set_stdio';
{$EXTERNALSYM gsapi_set_poll}
function gsapi_set_poll; stdcall; external gsdll32 name 'gsapi_set_poll';
{$EXTERNALSYM gsapi_set_display_callback}
function gsapi_set_display_callback; stdcall; external gsdll32 name 'gsapi_set_display_callback';
{$EXTERNALSYM gsapi_init_with_args}
function gsapi_init_with_args; stdcall; external gsdll32 name 'gsapi_init_with_args';
{$EXTERNALSYM gsapi_run_string_begin}
function gsapi_run_string_begin; stdcall; external gsdll32 name 'gsapi_run_string_begin';
{$EXTERNALSYM gsapi_run_string_continue}
function gsapi_run_string_continue; stdcall; external gsdll32 name 'gsapi_run_string_continue';
{$EXTERNALSYM gsapi_run_string_end}
function gsapi_run_string_end; stdcall; external gsdll32 name 'gsapi_run_string_end';
{$EXTERNALSYM gsapi_run_string_with_length}
function gsapi_run_string_with_length; stdcall; external gsdll32 name 'gsapi_run_string_with_length';
{$EXTERNALSYM gsapi_run_string}
function gsapi_run_string; stdcall; external gsdll32 name 'gsapi_run_string';
{$EXTERNALSYM gsapi_run_file}
function gsapi_run_file; stdcall; external gsdll32 name 'gsapi_run_file';
{$EXTERNALSYM gsapi_exit}
function gsapi_exit; stdcall; external gsdll32 name 'gsapi_exit';

end.






