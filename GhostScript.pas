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

unit GhostScript;

{$mode delphi}

interface

uses windows,classes,sysutils,gsapi, dialogs, graphics;

type
  TgsStdioEvent = procedure (var Value:String; len:Integer) of object;
  TgsStandardEvent = procedure () of object;
  TgsPageGeneratedEvent = procedure (Page:integer) of object;

  TGhostScript = class(TComponent)
    private
      callback:TDisplayCallback;
      FActive:boolean;
      instance:Pointer;
      FRevision:TGSApiRevision;
      FResolution:Integer;
      FStatus:integer;
      FActualPage:Integer;
      FViewBitmap:TBitmap;
      FResize:TNotifyEvent;
    protected
      procedure setActive(Value:boolean);
      procedure setResolution(Value:Integer);
      function getPage(index:integer):TBitmap;
      function getPageCount:Integer;
      procedure setInputEvent(Value:TgsStdioEvent);
      function getInputEvent:TgsStdioEvent;
      procedure setOutputEvent(Value:TgsStdioEvent);
      function getOutputEvent:TgsStdioEvent;
      procedure setErrorEvent(Value:TgsStdioEvent);
      function getErrorEvent:TgsStdioEvent;
      procedure setPollEvent(Value:TgsStandardEvent);
      function getPollEvent:TgsStandardEvent;
      procedure setPageEvent(Value:TgsPageGeneratedEvent);
      function getPageEvent:TgsPageGeneratedEvent;
      function getPageWidth:integer;
      function getPageHeight:integer;
    public
      procedure gsOpen;
      procedure gsClose;
      procedure gsInit(Filename: string);
      procedure gsExit;
      constructor Create(AOwner:TComponent); override;
      destructor Destroy; override;
      function LoadFile(FileName:String;Clear:boolean):boolean;
      function gsRunStringBegin(UserErrors:integer;var ExitCode:Integer):Integer;
      function gsRunStringContinue(Command:String;len:integer;UserErrors:Integer;var ExitCode:integer):Integer;
      function gsRunStringEnd(UserErrors:integer;var ExitCode:Integer):Integer;
      function gsRunString(Command:String;UserErrors:Integer;var ExitCode:integer):Integer;
      function gsRunStringWithLength(Command:String;len:integer;UserErrors:Integer;var ExitCode:integer):Integer;
      procedure ClearPages;
      procedure First;
      procedure Next;
      procedure gotoPage(index:integer);
      procedure Previous;
      procedure Last;
      property Page[index:integer]:TBitmap read getPage;
      property Revision:TGSApiRevision read FRevision;
      property ActualPage:Integer read FActualPage;
      property PageCount:integer read getPageCount;
    published
      property Active:boolean read FActive write setActive;
      property Resolution:Integer read FResolution write SetResolution;
      property ViewBitmap:TBitmap read FViewBitmap write FViewBitmap;
      property PageWidth:integer read getPageWidth;
      property PageHeight:integer read getPageHeight;
      property OnPageGenerated:TgsPageGeneratedEvent read getPageEvent write setPageEvent;
      property OnInput:tgsStdioEvent read getInputEvent write setInputEvent;
      property OnOutput:tgsStdioEvent read getOutputEvent write setOutputEvent;
      property OnError:tgsStdioEvent read getErrorEvent write setErrorEvent;
      property OnResizePage:TNotifyEvent read FResize write FResize;
  end;

implementation

uses gsimage;

var FBitmap:TList;
    Count:Integer;
    FInput:TgsStdioEvent;
    FOutput:TgsStdioEvent;
    FError:TgsStdioEvent;
    FPoll:TgsStandardEvent;
    FPage:TgsPageGeneratedEvent;

// Returns the format string to pass as an argument
function setFormat:String;
var format:integer;
    dc:HDC;
    depth:integer;
begin
   format := DISPLAY_COLORS_NATIVE or DISPLAY_ALPHA_NONE or
		DISPLAY_DEPTH_1 or DISPLAY_LITTLEENDIAN or DISPLAY_BOTTOMFIRST;
   dc := GetDC(0);	//* get hdc for desktop */
   depth := GetDeviceCaps(dc, PLANES) * GetDeviceCaps(dc, BITSPIXEL);
   if (depth = 32) then
     format := DISPLAY_COLORS_RGB or DISPLAY_ALPHA_NONE or
               DISPLAY_DEPTH_8 or DISPLAY_LITTLEENDIAN or DISPLAY_BOTTOMFIRST
   else if (depth = 16) then
     format := DISPLAY_COLORS_NATIVE or DISPLAY_ALPHA_NONE or
               DISPLAY_DEPTH_16 or DISPLAY_LITTLEENDIAN or DISPLAY_BOTTOMFIRST or
	       DISPLAY_NATIVE_555
   else if (depth > 8) then
      format := DISPLAY_COLORS_RGB or DISPLAY_ALPHA_NONE or
               DISPLAY_DEPTH_8 or DISPLAY_LITTLEENDIAN or DISPLAY_BOTTOMFIRST
   else if (depth >= 8) then
      format := DISPLAY_COLORS_NATIVE or DISPLAY_ALPHA_NONE or
               DISPLAY_DEPTH_8 or DISPLAY_LITTLEENDIAN or DISPLAY_BOTTOMFIRST
   else if (depth >= 4) then
      format := DISPLAY_COLORS_NATIVE or DISPLAY_ALPHA_NONE or
               DISPLAY_DEPTH_4 or DISPLAY_LITTLEENDIAN or DISPLAY_BOTTOMFIRST;
  result:='-dDisplayFormat='+INtToStr(format);
//  ShowMessage(result);
end;

// -------------------------------- Callback functions ---------------------

function gsstdin(caller_handle:Pointer;buf:PChar;len:integer):integer;stdcall;
var S:String;
begin
  S:=buf;
  if Assigned(Finput) then
    FInput(S,len);
//  S := InputBox('Inserisci dato','Inserire dato:','');
//  buf:=PChar(S);
  buf:='';
  result:=length(buf);
end;

function gsstdout(caller_handle:Pointer;buf:PChar;len:integer):integer;stdcall;
var S:String;
begin
  S:=buf;
  setlength(S,len);
  if Assigned(FOutput) then
    FOutput(S,len);
  result:=length(S);
end;

function gsstderr(caller_handle:Pointer;buf:PChar;len:integer):integer;stdcall;
var S:String;
begin
  S:=buf;
  setlength(S,len);
  if Assigned(FError) then
    FError(S,len);
  result:=length(buf);
end;


//----------------------------------------------------------

function display_poll(handle:Pointer):integer; stdcall;
begin
  if Assigned(FPoll) then
    FPoll();
  result:=0;
end;

//----------------------------------------------------------

function display_open(handle:Pointer;device:Pointer):integer; cdecl;
begin
  image_create(handle,device);
  result:=0;
end;

function display_preclose(handle:Pointer;device:Pointer):integer; cdecl;
begin
  result:=0;
end;

function display_close(handle:Pointer;device:Pointer):integer; cdecl;
begin
  result:=0;
end;


function display_presize(handle:Pointer;device:Pointer;
                         width:integer;height:integer;raster:integer;format:UINT):integer;cdecl;
begin
  result:=0;
end;

function display_size(handle:Pointer;device:Pointer;
         width:integer;height:integer;raster:integer;format:UINT;pimage:PChar):integer;cdecl;
begin
  image_size(width,height,raster,format,pimage);
  result:=0;
end;

function display_page(handle:Pointer;device:Pointer;copies:integer;flush:integer):integer;cdecl;
var bmp:TBitmap;
begin
  inc(Count);
  if FBitmap.count<count then
    begin
      bmp:=TBitmap.Create;
      FBitmap.Add(bmp);
    end;
  image_copy(TBitmap(FBitmap[count-1]));
  if Assigned(FPage) then
    FPage(count);
  result:=0;
end;

function display_update(handle:Pointer;device:Pointer;x:integer;y:integer;w:integer;h:integer):integer;cdecl;
begin
  result:=0;
end;

function display_sync(handle:Pointer;device:Pointer):integer; cdecl;
begin
  result:=0;
end;
//----------------------------------------------------------



//initializes the information needed, and the default properties values
constructor TGhostScript.Create(AOwner:TComponent);
begin
  new(instance);

  callback.size:=sizeof(callback);
  callback.version_major:=DISPLAY_VERSION_MAJOR;
  callback.version_minor:=DISPLAY_VERSION_MINOR;
  callback.display_open:=display_open;
  callback.display_close:=display_close;
  callback.display_preclose:=display_preclose;
  callback.display_presize:=display_presize;
  callback.display_size:=display_size;
  callback.display_sync:=display_sync;
  callback.display_page:=display_page;
  callback.display_update:=display_update;
  callback.display_memalloc:=nil;//display_memalloc;
  callback.display_memfree:=nil;//display_memfree;

  FActive:=false;
  FResolution:=150; // standard video dpi
  FStatus:=0;
 //  FBitmap:=TBitmap.Create;
end;

// destroies everithing
destructor TGhostScript.Destroy;
begin
//  FBitmap.Free;
//  dispose(instance);
  // Initialize the revision
  if gsapi_revision(@FRevision, sizeof(FRevision))<>0 then
    begin
      raise Exception.Create('Impossible to get GhostScript revision');
    end;
end;


// Initializes the instance with arguments derived from the properties
procedure TGhostScript.gsInit(Filename: string);
var argv:PPChar;
    p1:string;
    dformat:String;
    ddevice:String;
    Arg: array [1..10] of string;
    code:integer;
begin
  setlength(argv,8);

  p1:=ParamStr(0); // program name
  argv[0]:=PChar(p1);
  Arg[1]:=PChar('-r'+IntToStr(FResolution));
  Arg[2] := '-sDEVICE=display';
  Arg[3] := '-dDisplayHandle=0';
  dformat:=setFormat;
  Arg[4] := dformat;
  Arg[5]:= '-I' + ExtractFilePath(ParamStr(0));
  Arg[6] := '-f';
  Arg[7] := Filename;

  argv[1]:=PChar(Arg[1]);
  argv[2]:=PChar(Arg[2]);
  argv[3]:=PChar(Arg[3]);
  argv[4]:=PChar(Arg[4]);
  argv[5]:=PChar(Arg[5]);
  argv[6]:=PChar(Arg[6]);
  argv[7]:=PChar(Arg[7]);

  try
    code:=gsapi_init_with_args(instance,length(argv),argv);
    if code<0 then
      raise Exception.Create('ERROR: init_args: '+IntToStr(code));
  except
      raise Exception.Create('ERROR: init_args: Access violation. Something went wrong.');
  end;
  FStatus:=2;
end;

// Exits current initialization
procedure TGhostScript.gsExit;
begin
  gsapi_exit(instance);
  FStatus:=1;
end;

// Creates the new instance of Ghostscript (gsapi_new_instance)
procedure TGhostScript.gsOpen;
var code:integer;
begin
  // Initializes the instance
  code:=gsapi_new_instance(@instance,nil);
  if code<>0 then
    begin
      raise Exception.Create('Impossible to open an instance of ghostscript. Error code: '+IntToStr(code));
    end;
  // sets the call back functions
  code:=gsapi_set_stdio(instance,@gsstdin,@gsstdout,@gsstderr);
  if code<0 then
    begin
      gsClose;
      raise Exception.Create('Impossible to assign callback stdio functions: '+IntToStr(code));
    end;
  code:=gsapi_set_poll(instance,@display_poll);
  if code<0 then
    begin
      gsClose;
      raise Exception.Create('Impossible to assign callback poll function: '+IntToStr(code));
    end;
  code:=gsapi_set_display_callback(instance,@callback);
  if code<0 then
    begin
      gsClose;
      raise Exception.Create('Impossible to assign callback display functions: '+IntToStr(code));
    end;
  FActive:=true;
  FStatus:=1;
end;

procedure TGhostScript.gsClose;
begin
  try
    gsapi_delete_instance(instance);
  except;
  end;
  FStatus:=0;
end;

procedure TGhostScript.First;
begin
  if Assigned(FViewBitmap) then
    if PageCount>0 then
      begin
        FActualPage:=1;
        if Assigned(FResize) then FResize(Self);
        FViewBitmap.Assign(Page[ActualPage]);
      end;
end;

procedure TGhostScript.Next;
begin
  if Assigned(FViewBitmap) then
    if PageCount>ActualPage then
      begin
        inc(FActualPage);
        if Assigned(FResize) then FResize(Self);
        FViewBitmap.Assign(Page[ActualPage]);
      end;
end;

procedure TGhostScript.Previous;
begin
  if Assigned(FViewBitmap) then
    if ActualPage>1 then
      begin
        dec(FActualPage);
        if Assigned(FResize) then FResize(Self);
        FViewBitmap.Assign(Page[ActualPage]);
      end;
end;

procedure TGhostScript.Last;
begin
  if Assigned(FViewBitmap) then
    if (PageCount>0)and(ActualPage<>PageCount) then
      begin
        FActualPage:=PageCount;
        if Assigned(FResize) then FResize(Self);
        FViewBitmap.Assign(Page[ActualPage]);
      end;
end;

procedure TGhostScript.gotoPage(index:integer);
begin
  if Assigned(FViewBitmap) then
    if (PageCount>0)and(ActualPage<>index)and
       (index<=PageCount) then
      begin
        FActualPage:=index;
        if Assigned(FResize) then FResize(Self);
        FViewBitmap.Assign(Page[ActualPage]);
      end;
end;

procedure TGhostScript.ClearPages;
var Obj:TObject;
begin
  while FBitmap.Count>0 do
    begin
      Obj:=FBitmap[0];
      FBitmap.Delete(0);
      Obj.Free;
    end;
  Count:=0;
  FActualPage:=0;
end;

function TGhostScript.LoadFile(FileName:String;Clear:boolean):boolean;
var exit_code:integer;
begin
  if Clear then
    ClearPages;
  if not FileExists(Filename) then
    raise Exception('File '+FileName+' does not exist.');
  chdir(ExtractFilePath(FileName));
  FileName:=ExtractFileName(FileName);
  gsapi_run_file(instance,PChar(FileName),0,@exit_code);
  if exit_code<>0 then
    result:=false
  else
    result:=true;
end;

procedure TGhostScript.setActive(Value:boolean);
begin
  if Value<>FActive then
    if FActive then
      gsClose
    else
      gsOpen;
end;

procedure TGhostScript.setResolution(Value:Integer);
begin
  if Value<>FResolution then
    if Value<=0 then
      ShowMessage('The resolution cannot be negative')
    else
      FResolution:=Value;
end;

function TGhostScript.getPage(index:integer):TBitmap;
begin
  if index>count then
    result:=nil
  else
    result:=TBitmap(FBitmap[index-1]);
end;

function TGhostScript.getPageCount:Integer;
begin
  result:=FBitmap.Count;
end;

function TGhostScript.getPageWidth:integer;
begin
  result:=Page[ActualPage].Width;
end;

function TGhostScript.getPageHeight:integer;
begin
  result:=Page[ActualPage].Height;
end;

// Events
procedure TGhostScript.setInputEvent(Value:TgsStdioEvent);
begin
  FInput:=Value;
end;

function TGhostScript.getInputEvent:TgsStdioEvent;
begin
  result:=FInput;
end;

procedure TGhostScript.setOutputEvent(Value:TgsStdioEvent);
begin
  FOutput:=Value;
end;

function TGhostScript.getOutputEvent:TgsStdioEvent;
begin
  result:=FOutput;
end;

procedure TGhostScript.setErrorEvent(Value:TgsStdioEvent);
begin
  FError:=Value;
end;

function TGhostScript.getErrorEvent:TgsStdioEvent;
begin
  result:=FError;
end;

procedure TGhostScript.setPollEvent(Value:TgsStandardEvent);
begin
  FPoll:=Value;
end;

function TGhostScript.getPollEvent:TgsStandardEvent;
begin
  result:=FPoll;
end;

procedure TGhostScript.setPageEvent(Value:TgsPageGeneratedEvent);
begin
  FPage:=Value;
end;

function TGhostScript.getPageEvent:TgsPageGeneratedEvent;
begin
  result:=FPage;
end;



// gsFunctions
function TGhostScript.gsRunStringBegin(UserErrors:integer;var ExitCode:Integer):Integer;
begin
  if FStatus<2 then
    raise Exception.Create('GhostScript not initialized')
  else
    result:=gsapi_run_string_begin(instance,UserErrors,@exitcode);
end;

function TGhostScript.gsRunStringContinue(Command:String;len:integer;UserErrors:Integer;var ExitCode:integer):Integer;
begin
  if FStatus<2 then
    raise Exception.Create('GhostScript not initialized')
  else
    result:=gsapi_run_string_continue(instance,PChar(Command),len,UserErrors,@ExitCode);
end;

function TGhostScript.gsRunStringEnd(UserErrors:integer;var ExitCode:Integer):Integer;
begin
   if FStatus<2 then
    raise Exception.Create('GhostScript not initialized')
  else
    result:=gsapi_run_string_end(instance,UserErrors,@ExitCode);
end;

function TGhostScript.gsRunString(Command:String;UserErrors:Integer;var ExitCode:integer):Integer;
begin
   if FStatus<2 then
    raise Exception.Create('GhostScript not initialized')
  else
    result:=gsapi_run_string(instance,PChar(Command),UserErrors,@exitcode);
end;

function TGhostScript.gsRunStringWithLength(Command:String;len:integer;UserErrors:Integer;var ExitCode:integer):Integer;
begin
   if FStatus<2 then
    raise Exception.Create('GhostScript not initialized')
  else
    result:=gsapi_run_string_with_length(instance,PChar(Command),len,UserErrors,@ExitCode);
end;


initialization
  FBitmap:=TList.Create;
end.
