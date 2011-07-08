{
  Copyright Shaun Simpson 2011
  Copyright Tristan Linnell 2011

  This file is part of PDFPrint.

  PDFPrint is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  PDFPrint is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with PDFPrint.  If not, see <http://www.gnu.org/licenses/>.
}

unit Main;

{$mode delphi}

interface

uses
  Windows,
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, GhostScript, Printers, Buttons, PrintersDlgs, WinSpool,
  IntfGraphics, gsapi, OSPrinters;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnPrint: TBitBtn;
    dlgOpenFile: TOpenDialog;
    Image1: TImage;
    Panel1: TPanel;
    dlgPrinterSetup: TPrinterSetupDialog;
    tmrPrint: TTimer;
    procedure btnPrintClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tmrPrintTimer(Sender: TObject);
  private
    { private declarations }
    gs: TGhostScript;
    FPDFPage: integer;
    FPrintedPages: integer;
    FFromPage, FToPage: integer;
    FPrinterDialog: boolean;
    FPrinterName, FFilename: string;
    FSilent: boolean;
    FResolution: integer;
    FDetectPostScript: boolean;

    procedure AutoPrint;
    procedure GreyScaleImage(Image: TBitmap);
    procedure gsOnPageGenerated(page:integer);
    function IsPsPrinter: Boolean;
    procedure PdfToPs(Input, Output: string);
    function IsColourPrinter(PrinterName: string): boolean;
    procedure Print(Filename: string);
    procedure PsPrint(Filename: string);
  public
    { public declarations }
  end; 

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.btnPrintClick(Sender: TObject);
begin
  if dlgOpenFile.Execute and dlgPrinterSetup.Execute then
  begin
    Print(dlgOpenFile.FileName);
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  i: integer;
  Param: string;
begin
  gs:=TGhostScript.Create(Self);
  gs.ViewBitmap:=nil;
  gs.OnPageGenerated:=gsOnPageGenerated;
  gs.OnInput:=nil;
  gs.OnOutput:=nil;
  gs.OnError:=nil;
  gs.OnResizePage:=nil;

  FPrinterDialog := False;
  FFromPage := 1;
  FToPage := 9999;
  FPrinterName := '';
  FSilent := False;
  FFilename := '';
  FResolution := 300;
  FDetectPostScript := True;

  i := 1;
  while i < ParamCount do
  begin
    Param := ParamStr(i);

    // First page number
    if Pos('-from',LowerCase(Param)) = 1 then
    begin
      try
        FFromPage := StrToInt(ParamStr(i+1));
        Inc(i);
      except
      end;
    end;

    // Last page number
    if Pos('-to',LowerCase(Param)) = 1 then
    begin
      try
        FToPage := StrToInt(ParamStr(i+1));
        Inc(i);
      except
      end;
    end;

    // Resolution to use, 0 uses native printer resolution
    if Pos('-resolution',LowerCase(Param)) = 1 then
    begin
      try
        FResolution := StrToInt(ParamStr(i+1));
        Inc(i);
      except
      end;
    end;

    // Printer name
    if Pos('-printer',LowerCase(Param)) = 1 then
    begin
      try
        FPrinterName := ParamStr(i+1);
        FPrinterName := StringReplace(FPrinterName, '"', '', [rfReplaceAll, rfIgnoreCase]);
        Inc(i);
      except
      end;
    end;

    // Show printer dialog
    if LowerCase(Param) = '-dialog' then
    begin
      try
        FPrinterDialog := True;
      except
      end;
    end;

    // HIde GUI
    if LowerCase(Param) = '-silent' then
    begin
      try
        FSilent := True;
      except
      end;
    end;

    // Force sending PostScript, bypass PostScript support detection.
    if LowerCase(Param) = '-force-ps' then
    begin
      try
        FDetectPostscript := False;
      except
      end;
    end;

    Inc(i);
  end;

  if ParamCount > 0 then
  begin
    FFilename := ParamStr(ParamCount);
    FFilename := StringReplace(FFilename, '"', '', [rfReplaceAll, rfIgnoreCase]);

    if FSilent then
    begin
      AutoPrint();
      Halt;
    end;
  end;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  gs.Free;
end;

procedure TfrmMain.AutoPrint;
begin
  if FPrinterName <> '' then
    Printer.SetPrinter(FPrinterName);

  if FPrinterDialog then
  begin
    if not dlgPrinterSetup.Execute then
      Halt;
  end;

  Print(FFilename);
  Halt;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  if FFilename <> '' then
  begin
    tmrPrint.Enabled := True;
    btnPrint.Visible := False;
    Self.Panel1.Align := alClient;
    Self.BorderStyle := bsNone;
    tmrPrint.Enabled := True;
  end;
end;

procedure TfrmMain.tmrPrintTimer(Sender: TObject);
begin
  tmrPrint.Enabled := False;
  AutoPrint();
  Halt();
end;

procedure TfrmMain.Print(Filename: string);
begin
  Printer.Title := ExtractFilename(Filename);

  FPDFPage := 0;
  FPrintedPages := 0;

  if not FDetectPostscript or IsPsPrinter then
  begin
    PsPrint(Filename)
  end
  else
  begin
    if FResolution < 75 then
      FResolution := Printer.XDPI;

    gs.Resolution := FResolution;

    Printer.BeginDoc;

    gs.gsOpen;
    gs.gsInit(Filename);
    gs.LoadFile(Filename,true);
    gs.gsExit;
    gs.gsClose;

    Printer.EndDoc;
  end;
end;

procedure TfrmMain.PsPrint(Filename: string);
var
  PrintFile: TFileStream;
  Buffer: array [0..4095] of byte;
  Bytes: integer;
  TempFilename: string;
  h, m, s, ms: word;
begin
  DecodeTime(Now, h, m, s, ms);
  TempFilename := GetTempDir(False);

  if (TempFilename = '') or not DirectoryExists(TempFilename) then
    TempFilename := GetAppConfigDir(False);

  TempFilename := TempFilename + Format('%d%d%d%d%d.ps', [h, m, s, ms, Random(100000)]);

  PdfToPs(Filename, TempFilename);

  try
    PrintFile := TFileStream.Create(TempFilename, fmOpenRead);
  except
    MessageDlg('Postscript Error', 'Error writing Postscript file '
      + TempFilename + '.', mtError, [mbOK], 0);
    Exit;
  end;

  try
    Printer.Title := ExtractFilename(Filename);

    Printer.RawMode := True;

    Printer.BeginDoc;

    repeat
      Bytes := PrintFile.Read(Buffer, Length(Buffer));
      Printer.Write(Buffer, Bytes, Bytes);
    until Bytes <> Length(Buffer);

    Printer.EndDoc;
    Printer.RawMode := False;

    DeleteFile(TempFilename);
  except
    MessageDlg('Postscript Error', 'Error writing Postscript file '
      + TempFilename + '.', mtError, [mbOK], 0);
  end;

  PrintFile.Free;
end;

procedure TfrmMain.gsOnPageGenerated(Page: integer);
var
  PWidth, PHeight: integer;
  Image: TBitmap;
begin
  Inc(FPDFPage);

  if (FFromPage <= FPDFPage) and (FToPage >= FPDFPage) then
  begin
    Image := gs.Page[page];

    if not IsColourPrinter(Printer.Printers[Printer.PrinterIndex]) then
      GreyScaleImage(Image);

    if not FSilent then
    begin
      if not Image1.Stretch then
        Image1.Stretch := True;

      Image1.Picture.Assign(Image);
      Application.ProcessMessages;
    end;

    if FPrintedPages > 0 then
      Printer.NewPage;

    PWidth := Printer.PageWidth;
    PHeight := Printer.PageHeight;

    Printer.PrinterType ;
    Printer.Canvas.StretchDraw(Rect(0, 0, PWidth - 1,
      PHeight - 1), Image);

    Inc(FPrintedPages);
  end;

  gs.ClearPages;
end;

// Convert image to grey scale image.
// Only supports 24 and 32 bit images.
procedure TfrmMain.GreyScaleImage(Image: TBitmap);
var
  PSrc, PDst: PByte;
  R, G, B, C: Integer;
  SrcIntfImg: TLazIntfImage;
  ImgHandle, ImgMaskHandle: HBitmap;
  i, j: Integer;
begin
  // Can not access bitmap pixel data directly,
  // need to use TLazIntfImage to get at it.
  if Image.PixelFormat in [pf24bit, pf32bit] then
  begin
    SrcIntfImg:=TLazIntfImage.Create(0,0);
    SrcIntfImg.LoadFromBitmap(Image.Handle,Image.MaskHandle);
    Image.FreeImage;
    PSrc := SrcIntfImg.PixelData;
    PDst := SrcIntfImg.PixelData;

    for i:=0 to SrcIntfImg.Height-1 do
    begin
      for j:=0 to SrcIntfImg.Width-1 do
      begin
        B := PSrc^;
        Inc(PSrc);
        G := PSrc^;
        Inc(PSrc);
        R := PSrc^;
        Inc(PSrc);

        // 32 bit images
        if Image.PixelFormat = pf32bit then
          Inc(PSrc);

        // Calculate grey level
        C := Round(R * 0.3 + G * 0.6 + B * 0.1);

        PDst^ := C;
        Inc(PDst);
        PDst^ := C;
        Inc(PDst);
        PDst^ := C;
        Inc(PDst);

        // 32 bit images
        if Image.PixelFormat = pf32bit then
          Inc(PSrc);
      end;
    end;

    SrcIntfImg.CreateBitmaps(ImgHandle, ImgMaskHandle, False);
    Image.BitmapHandle := ImgHandle;
    Image.MaskHandle := ImgMaskHandle;
    SrcIntfImg.Free;
  end;
end;

function TfrmMain.IsColourPrinter(PrinterName: string): boolean;
var
  PDev: PChar;
  PDevW: PWideChar;
  DM1: LPDEVMODEW;
  DM2: LPDEVMODEW;
  Sz: Integer;
begin
  Result := True;

  GetMem(PDev, 512);
  GetMem(PDevW, 1024);
  FillChar(PDev^, 512, 0);
  FillChar(PDevW^, 1024, 0);

  StrPCopy(PDev, PrinterName);
  UTF8ToUnicode(PDevW, PDev, 512);

  DM1 := nil;
  DM2 := nil;
  Sz  := DocumentProperties(0, 0, PDevW, DM1, DM2, 0);

  if Sz > 0 then
  begin
    GetMem(DM1, Sz);
    DocumentProperties(0, 0, PDevW, DM1, DM2, DM_OUT_BUFFER);

    if DM1^.dmFields and DM_Color <> 0 then Result := True
    else Result := False;

    FreeMem(DM1);
  end;

  FreeMem(PDevW);
  FreeMem(PDev);
end;

// Check if printer supports Postscript
// Does not seem reliable
function TfrmMain.IsPsPrinter: Boolean;
const
  POSTSCRIPT_PASSTHROUGH = 4115;
  POSTSCRIPT_IDENTIFY = 4117;
  POSTSCRIPT_DATA = 37;
  ENCAPSULATED_POSTSCRIPT = 4116;
  POSTSCRIPT_INJECTION = 4118;
  GET_PS_FEATURESETTING = 4121;


  Escapes: array[0..4] of Cardinal =
  (POSTSCRIPT_DATA, POSTSCRIPT_IDENTIFY, POSTSCRIPT_PASSTHROUGH, ENCAPSULATED_POSTSCRIPT, POSTSCRIPT_INJECTION);
var
  res: Integer;
  i, Output: Integer;

begin
  Result := False;
  for i := 1 to High(Escapes) do
  begin
    res := ExtEscape(TWinPrinter(Printer).Handle,
      QUERYESCSUPPORT, sizeof(Escapes[0]), @Escapes[i], 0, nil);

    if res <> 0 then
    begin
      Result := True;
      Break;
    end;
  end;
end;

procedure TfrmMain.PdfToPs(Input, Output: string);
var
  argv:PPChar;
  p1, p2:string;
  dformat:String;
  ddevice:String;
  Arg: array [1..9] of string;
  i, code:integer;
  instance:Pointer;
begin
  SetLength(argv,10);

  p1:=ParamStr(0); // program name
  argv[0] :=  PChar(p1);
  p2 := '-I' + ExcludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + '' + #0;
  Arg[1] := p2;
  Arg[2] := '-dNOPAUSE' + #0;
  Arg[3] := '-dBATCH' + #0;
  Arg[4] := '-dFirstPage=' + IntToStr(FFromPage) + #0;
  Arg[5] := '-dLastPage=' + intToStr(FToPage) + #0;
  Arg[6] := '-sDEVICE=pswrite' + #0;
  Arg[7] := '-sOutputFile=' + Output + #0;
  Arg[8] := '-f' + #0;
  Arg[9] := Input + #0;

  for i := 1 to High(Arg) do
  begin
    argv[i]:=PChar(Arg[i]);
  end;

  try
    // Initializes the instance
    code:=gsapi_new_instance(@instance,nil);
    if code<>0 then
      raise Exception.Create('Impossible to open an instance of ghostscript. Error code: '+IntToStr(code));

    code:=gsapi_init_with_args(instance, length(argv), argv);
    if code<0 then
      raise Exception.Create('ERROR: init_args: '+IntToStr(code));
  except
    raise Exception.Create('ERROR: init_args: Access violation. Something went wrong.');
  end;

  try
    gsapi_exit(instance);
    gsapi_delete_instance(instance);
  except;
  end;
end;

end.

