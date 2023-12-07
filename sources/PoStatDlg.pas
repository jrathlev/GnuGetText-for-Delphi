(* Dialog for GgtTranslate
   Show statistics of po file

   © 2023 Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de)
   based on the "dxgettext" programs by Lars B. Dybdahl

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.

   September 2023
   last modified: September 2023
   *)

unit PoStatDlg;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons,
  HListBox, PoParser, Vcl.ComCtrls;

type
  TPoStatDialog = class(TForm)
    bbExit: TBitBtn;
    OpenDialog: TOpenDialog;
    gbStat: TGroupBox;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    laEntries: TLabel;
    laNumTrans: TLabel;
    laNumNoTrans: TLabel;
    laNumFuzzy: TLabel;
    laNumChars: TLabel;
    lvHeader: TListView;
    Label6: TLabel;
    laPoFile: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure bbExitClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Private-Deklarationen }
    FIniName,FIniSection : string;
    PoList               : TPoEntryList;
    procedure ClearStat;
    procedure ShowStat;
    function LoadFile (const PoFile : string) : boolean;
    procedure SaveToIni;
  public
    { Public-Deklarationen }
    procedure LoadFromIni (const IniName, Section : string);
    procedure Execute (const PoFilename : string);
  end;

var
  PoStatDialog: TPoStatDialog;

implementation

{$R *.dfm}

uses System.IniFiles, System.StrUtils, Winapi.ShellApi, GnuGetText, InitProg,
  WinUtils, PathUtils, MsgDialogs;

{ ------------------------------------------------------------------- }
procedure TPoStatDialog.FormCreate(Sender: TObject);
begin
  TranslateComponent (self);
  PoList:=TPoEntryList.Create;
  end;

procedure TPoStatDialog.FormDestroy(Sender: TObject);
begin
  SaveToIni;
  PoList.Free;
  end;

procedure TPoStatDialog.FormResize(Sender: TObject);
begin
  with lvHeader do begin
    Columns[1].Width:=ClientRect.Right-Columns[0].Width;
    end;
  end;

procedure TPoStatDialog.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  end;

{ ------------------------------------------------------------------- }
const
  IniExt = 'ini';

  (* INI-Variables *)
  iniTop      = 'Top';
  iniLeft     = 'Left';
  IniWidth    = 'Width';
  IniHeight   = 'Height';

procedure TPoStatDialog.LoadFromIni (const IniName, Section : string);
begin
  FIniName:=IniName; FIniSection:=Section;
  with TIniFile.Create(FIniName) do begin
    Top:=ReadInteger(FIniSection,iniTop,Top);
    Left:=ReadInteger(FIniSection,iniLeft,Left);
    ClientWidth:=ReadInteger (FIniSection,IniWidth,ClientWidth);
    ClientHeight:=ReadInteger (FIniSection,IniHeight,ClientHeight);
    Free;
    end;
  end;

procedure TPoStatDialog.SaveToIni;
begin
  if (length(FIniName)>0) and (length(FIniSection)>0) then
      with TIniFile.Create(FIniName) do begin
    WriteInteger(FIniSection,iniTop,Top);
    WriteInteger(FIniSection,iniLeft,Left);
    WriteInteger (FIniSection,IniWidth,ClientWidth);
    WriteInteger (FIniSection,IniHeight,ClientHeight);
    Free;
    end;
  end;

{ ------------------------------------------------------------------- }
procedure TPoStatDialog.bbExitClick(Sender: TObject);
begin
  Close;
  end;

{ ------------------------------------------------------------------- }
procedure TPoStatDialog.ClearStat;
begin
  laEntries.Caption:='';
  laNumTrans.Caption:='';
  laNumNoTrans.Caption:='';
  laNumFuzzy.Caption:='';
  laNumChars.Caption:='';
  end;

procedure TPoStatDialog.ShowStat;
var
  ne,nt,nu,nf,
  cs,ct : integer;
  pe : TPoEntry;
  id : TPoHeaderIds;
begin
  with PoList do begin
    ne:=0; nt:=0; nu:=0; nf:=0; cs:=0; ct:=0;
    // get entries
    pe:=FindFirst;
    while pe<>nil do begin
      with pe do if not (AnsiStartsStr('##',MsgId) or MsgId.IsEmpty) then begin // skip history entries
        if length(MsgStr)=0 then inc(nu) else inc(nt);
        if Fuzzy then inc(nf);
        cs:=cs+length(MsgId); ct:=ct+length(MsgStr);
        inc(ne);
        end;
      pe:=FindNext(pe);
      end;
    laEntries.Caption:=IntToStr(ne);
    laNumTrans.Caption:=IntToStr(nt)+' ('+IntToStr(round(100*nt/ne))+'%)';
    laNumNoTrans.Caption:=IntToStr(nu)+' ('+IntToStr(round(100*nu/ne))+'%)';
    laNumFuzzy.Caption:=IntToStr(nf);
    laNumChars.Caption:=_('Template = ')+IntToStr(cs)+sLineBreak+_('Translation = ')+IntToStr(ct);
    lvHeader.Clear;
    for id:=Low(TPoHeaderIds) to High(TPoHeaderIds) do begin
      if not Header[id].IsEmpty then with lvHeader.Items.Add do begin
        Caption:=HeaderIds[id];
        SubItems.Add(Header[id])
        end;
      end;
    end;
  end;

function TPoStatDialog.LoadFile (const PoFile : string) : boolean;
var
  ne : integer;
begin
  laPoFile.Caption:=PoFile;
  Result:=false;
  if not FileExists(PoFile) then begin
    ErrorDialog(TopLeftPos(gbStat),Format(_('File not found: %s'),[PoFile]));
    Exit;
    end;
  ClearStat;
  try
    ne:=PoList.LoadFromFile(PoFile);
    if ne>0 then
      ErrorDialog(TopLeftPos(gbStat),Format(_('Error in line %u of po file!'),[ne]))
    else ShowStat;
  except
    ErrorDialog(TopLeftPos(gbStat),_('Error reading po file!'));
    end;
  end;

procedure TPoStatDialog.Execute (const PoFilename : string);
begin
  LoadFile(PoFilename);
  ShowModal;
  end;

end.

