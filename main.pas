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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, GhostScript, Printers, Buttons, PrintersDlgs;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnPrint: TBitBtn;
    dlgOpenFile: TOpenDialog;
    dlgPageSetup: TPageSetupDialog;
    Image1: TImage;
    Panel1: TPanel;
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

    procedure AutoPrint;
    procedure gsOnPageGenerated(page:integer);
    procedure Print(Filename: string);
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
  if dlgOpenFile.Execute and dlgPageSetup.Execute then
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
  FToPage := 1000;
  FPrinterName := '';
  FSilent := False;
  FFilename := '';
  FResolution := 300;

  i := 1;
  while i < ParamCount do
  begin
    Param := ParamStr(i);

    if Pos('-from',LowerCase(Param)) = 1 then
    begin
      try
        FFromPage := StrToInt(ParamStr(i+1));
        Inc(i);
      except
      end;
    end;

    if Pos('-to',LowerCase(Param)) = 1 then
    begin
      try
        FToPage := StrToInt(ParamStr(i+1));
        Inc(i);
      except
      end;
    end;

    if Pos('-resolution',LowerCase(Param)) = 1 then
    begin
      try
        FResolution := StrToInt(ParamStr(i+1));
        Inc(i);
      except
      end;
    end;

    if Pos('-printer',LowerCase(Param)) = 1 then
    begin
      try
        FPrinterName := ParamStr(i+1);
        FPrinterName := StringReplace(FPrinterName, '"', '', [rfReplaceAll, rfIgnoreCase]);
        Inc(i);
      except
      end;
    end;

    if LowerCase(Param) = '-dialog' then
    begin
      try
        FPrinterDialog := True;
      except
      end;
    end;

    if LowerCase(Param) = '-silent' then
    begin
      try
        FSilent := True;
      except
      end;
    end;

    Inc(i);
  end;

  if ParamCount > 1 then
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
    if not dlgPageSetup.Execute then
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
    btnPrint.Visible := True;
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

  if FResolution < 75 then
    gs.Resolution := Printer.XDPI
  else
    gs.Resolution := FResolution;

  Printer.BeginDoc;

  gs.gsOpen;
  gs.gsInit(Filename);
  gs.LoadFile(Filename,true);
  gs.gsExit;
  gs.gsClose;

  Printer.EndDoc;
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

    Printer.Canvas.StretchDraw(Rect(0, 0, PWidth - 1,
      PHeight - 1), Image);

    Inc(FPrintedPages);
  end;

  gs.ClearPages;
end;

end.

