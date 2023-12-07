(* Tool for GnuGetText for Delphi
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

unit PoStatMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons,
  HListBox, PoParser, Vcl.ComCtrls;

const
  Vers = ' - Vers. 3.1';

type
  TfrmMain = class(TForm)
    edTranslation: THistoryCombo;
    Label2: TLabel;
    bbInfo: TBitBtn;
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
    bbOpenPoFile: TBitBtn;
    btnHelp: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure bbOpenPoFileClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure bbExitClick(Sender: TObject);
    procedure bbInfoClick(Sender: TObject);
    procedure edTranslationCloseUp(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
  private
    { Private-Deklarationen }
    ProgVersName,
    ProgVersDate,
    AppPath,UserPath,
    IniName,ProgPath,
    PoFile             : string;
    PoList             : TPoEntryList;
    procedure ClearStat;
    procedure ShowStat;
    function LoadFile : boolean;
    function SelectPo: boolean;
    procedure WMDROPFILES (var Msg: TMessage); message WM_DROPFILES;
  public
    { Public-Deklarationen }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

uses System.IniFiles, System.StrUtils, Winapi.ShellApi, GnuGetText, InitProg,
  WinUtils, PathUtils, MsgDialogs, GgtConsts, GgtUtils;

{ ------------------------------------------------------------------- }
const
  (* INI-Variables *)
  iniPoName = 'LastPo';

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  TranslateComponent (self);
  DragAcceptFiles(frmMain.Handle, true);
  Application.Title:=_('Statistics of PO file');
  InitPaths(AppPath,UserPath,ProgPath);
  InitVersion(Application.Title,Vers,CopRgt,3,3,ProgVersName,ProgVersDate);
  Caption:=ProgVersName;
  if ParamCount>0 then PoFile:=ExpandFileName(ParamStr(1))
  else PoFile:='';
  IniName:=Erweiter(AppPath,PrgName,IniExt);
  with TIniFile.Create(IniName) do begin
    if length(PoFile)=0 then PoFile:=ReadString(CfGSekt,iniPoName,'');
    Top:=ReadInteger(CfgSekt,iniTop,Top);
    Left:=ReadInteger(CfgSekt,iniLeft,Left);
    ClientWidth:=ReadInteger (CfgSekt,IniWidth,ClientWidth);
    ClientHeight:=ReadInteger (CfgSekt,IniHeight,ClientHeight);
    Free;
    end;
  with edTranslation do begin
    LoadFromIni(IniName,TransSekt);
    if Items.Count=0 then Style:=csSimple else Style:=csDropDown;
    Text:=PoFile;
    end;
  PoList:=TPoEntryList.Create;
  end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  if FileExists(PoFile) then LoadFile
  else if SelectPo then LoadFile
  else Close;
  end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  PoList.Free;
  end;

procedure TfrmMain.FormResize(Sender: TObject);
begin
  with lvHeader do begin
    Columns[1].Width:=ClientRect.Right-Columns[0].Width;
    end;
  end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  with TIniFile.Create(IniName) do begin
    WriteString(CfGSekt,iniPoName,PoFile);
    WriteInteger(CfgSekt,iniTop,Top);
    WriteInteger(CfgSekt,iniLeft,Left);
    WriteInteger (CfgSekt,IniWidth,ClientWidth);
    WriteInteger (CfgSekt,IniHeight,ClientHeight);
    Free;
    end;
  try HtmlHelp(0,nil,HH_CLOSE_ALL,0); except end;
  end;

procedure TfrmMain.WMDROPFILES (var Msg: TMessage);
var
   n,size: integer;
   Filename: PChar;
begin
  inherited;
  Filename:=nil;
  n:=DragQueryFile(Msg.WParam,$FFFFFFFF,nil,0);
  if n>0 then begin
    size:=DragQueryFile(Msg.WParam,0,nil,0)+1;
    Filename:=StrAlloc(size);
    DragQueryFile(Msg.WParam,0,Filename,size);
    if AnsiSameText(GetExt(Filename),'po') then begin
      PoFile:=Filename;
      with edTranslation do begin
        Text:=PoFile; AddItem(PoFile);
        end;
      Application.BringToFront;
      LoadFile;
      end
    else ErrorDialog(TopLeftPos(gbStat),Format(_('This application only allows dropping of %s files!'),[PoExt]));
    StrDispose(Filename);
    end;
  DragFinish(Msg.WParam);
  end;

procedure TfrmMain.bbExitClick(Sender: TObject);
begin
  Close;
  end;

procedure TfrmMain.bbInfoClick(Sender: TObject);
begin
  InfoDialog(TopLeftPos(gbStat),ProgVersName+' - '+ProgVersDate+#13+CopRgt
           +#13'E-Mail: '+EmailAdr);
  end;

procedure TfrmMain.ClearStat;
begin
  laEntries.Caption:='';
  laNumTrans.Caption:='';
  laNumNoTrans.Caption:='';
  laNumFuzzy.Caption:='';
  laNumChars.Caption:='';
  end;

procedure TfrmMain.ShowStat;
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

function TfrmMain.LoadFile : boolean;
var
  ne : integer;
begin
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

function TfrmMain.SelectPo: boolean;
begin
  with OpenDialog do begin
    if length(PoFile)>0 then InitialDir:=ExtractFilePath(PoFile)
    else InitialDir:=UserPath;
    Filename:='';
    Title:=_('Select po file');
    Filter:=Format(_('po files|*.%s|all|*.*'),[PoExt]);
    if Execute then with edTranslation do begin
      PoFile:=Filename;
      Text:=Filename; AddItem(Filename);
      Result:=true;
      end
    else Result:=false;
    end;
  end;

procedure TfrmMain.edTranslationCloseUp(Sender: TObject);
begin
  with edTranslation do PoFile:=Items[ItemIndex];
  LoadFile;
  end;

procedure TfrmMain.bbOpenPoFileClick(Sender: TObject);
begin
  if SelectPo then LoadFile;
  end;

procedure TfrmMain.btnHelpClick(Sender: TObject);
begin
  ShowHelp('tools.html#stat');
  end;

end.
