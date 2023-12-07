(* Tool for GnuGetText for Delphi
   Convert strings from po file for use in InnoSetup scripts

   © Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.

   Command line:
   -------------
   Calling:  PoToIss <poname> [options]
     <poname>         - Name of po file with translation
     /prefix:<langid> - Language identifier (two letter, e.g. en, de, ..)
     /out:<OutName>   - Use this file for output (instead of poname.txt)

   Feb. 2014
   last modified: November 2023
   *)

unit PoToIssMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons,
  HListBox, PoParser, GgtConsts, Vcl.ExtCtrls, Vcl.ComCtrls;

const
  Vers = ' - Vers. 1.1';

type
  TfrmMain = class(TForm)
    Label2: TLabel;
    edPoFile: THistoryCombo;
    bbInfo: TBitBtn;
    bbExit: TBitBtn;
    bbConvert: TBitBtn;
    OpenDialog: TOpenDialog;
    edLanguage: THistoryCombo;
    Label1: TLabel;
    edOutName: TLabeledEdit;
    bbPoFile: TBitBtn;
    bbCopyName: TBitBtn;
    StatusBar: TStatusBar;
    laTitle: TLabel;
    btnHelp: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure bbExitClick(Sender: TObject);
    procedure bbInfoClick(Sender: TObject);
    procedure bbPoFileClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure edPoFileCloseUp(Sender: TObject);
    procedure bbConvertClick(Sender: TObject);
    procedure edLanguageCloseUp(Sender: TObject);
    procedure bbCopyNameClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
  private
    { Private-Deklarationen }
    ProgVersName,
    ProgVersDate,
    AppPath,UserPath,
    IniName,ProgPath,
    PoFile,LangId,
    OutName            : string;
    PoList             : TPoEntryList;
    Loaded,Changed     : boolean;
    function SelectPo  : boolean;
    function LoadPoFile : boolean;
  public
    { Public-Deklarationen }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

uses System.IniFiles, Winapi.ShlObj, System.StrUtils, gnugettext, ggtutils, WinUtils,
  LangUtils, InitProg, WinApiUtils, WinShell, PathUtils, NumberUtils, MsgDialogs;

{ ------------------------------------------------------------------- }
const
  (* INI-Sections *)
  PoSekt = 'PoFiles';
  LangSekt = 'Languages';

  (* INI-Variables *)
  iniLast = 'LastFile';
  îniLang = 'LastLanguage';

procedure TfrmMain.FormCreate(Sender: TObject);
var
  i : integer;
  sn,sp : string;
begin
  TranslateComponent (self);
  Application.Title:=_('Convert strings from po to iss');
  InitPaths(AppPath,UserPath,ProgPath);
  InitVersion(Application.Title,Vers,CopRgt,3,3,ProgVersName,ProgVersDate);
  Caption:=ProgVersName;
  IniName:=Erweiter(AppPath,PrgName,IniExt);
  with TIniFile.Create(IniName) do begin
    Top:=ReadInteger(CfGSekt,iniTop,Top);
    Left:=ReadInteger(CfGSekt,iniLeft,Left);
    PoFile:=ReadString(CfGSekt,iniLast,'');
    sn:=ExtractLastDir(ExtractFilePath(PoFile));
    if length(sn)>2 then sn:='de';
    LangId:=ReadString(CfGSekt,îniLang,sn);
    Free;
    end;
  OutName:='';
  if ParamCount>0 then begin
    sp:='';
    for i:=1 to ParamCount do begin
      sn:=ParamStr(i);
      if (sn[1]='/') or (sn[1]='-') then begin
        Delete(sn,1,1);
        if ReadOptionValue(sn,'prefix') then LangId:=sn
        else if ReadOptionValue(sn,'out') then OutName:=sn
        end
      else sp:=sn;
      end;
    if length(sp)>0 then begin
      PoFile:=ExpandFileName(sp);
      sn:=ExtractLastDir(ExtractFilePath(PoFile));
      if length(sn)=2 then LangId:=sn;
      end;
    end;
  if length(OutName)=0 then OutName:=ChangeFileExt(ExtractFilename(PoFile),'.txt');
  with edPoFile do begin
    LoadFromIni(IniName,PoSekt);
    if Items.Count=0 then Style:=csSimple else Style:=csDropDown;
    Text:=PoFile;
    end;
  with edLanguage do begin
    LoadFromIni(IniName,LangSekt);
    if Items.Count=0 then Style:=csSimple else Style:=csDropDown;
    Text:=LangId;
    end;
  edOutName.Text:=OutName;
  PoList:=TPoEntryList.Create;
  Loaded:=false; Changed:=false;
  end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  if FileExists(PoFile) then LoadPoFile else edPoFile.Text:='';
  end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  with TIniFile.Create(IniName) do begin
    WriteInteger(CfGSekt,iniTop,Top);
    WriteInteger(CfGSekt,iniLeft,Left);
    WriteString(CfGSekt,iniLast,PoFile);
    WriteString(CfGSekt,îniLang,LangId);
    Free;
    end;
  PoList.Free;
  try HtmlHelp(0,nil,HH_CLOSE_ALL,0); except end;
  end;

procedure TfrmMain.bbCopyNameClick(Sender: TObject);
begin
  OutName:=ChangeFileExt(ExtractFilename(PoFile),'.txt');
  edOutName.Text:=OutName;
  end;

procedure TfrmMain.bbExitClick(Sender: TObject);
begin
  Close;
  end;

procedure TfrmMain.bbInfoClick(Sender: TObject);
begin
  InfoDialog(ProgVersName+' - '+ProgVersDate+#13+CopRgt
           +#13'E-Mail: '+EmailAdr);
  end;

procedure TfrmMain.edLanguageCloseUp(Sender: TObject);
var
  s : string;
begin
  with edLanguage do s:=Items[ItemIndex];
  PoFile:=ReplaceStr(PoFile,LangId,s);
  LangId:=s;
  edPoFile.Text:=poFile;
  end;

procedure TfrmMain.edPoFileCloseUp(Sender: TObject);
begin
  with edPoFile do begin
    PoFile:=HistoryList[ItemIndex];
    end;
  end;

function TfrmMain.SelectPo : boolean;
var
  s : string;
begin
  with OpenDialog do begin
    if length(PoFile)>0 then InitialDir:=ExtractFilePath(PoFile)
    else InitialDir:=UserPath;
    Filename:='';
    Title:=_('Select po file with translation');
    Filter:=Format(_('po files|*.%s|all|*.*'),[PoExt]);
    if Execute then with edPoFile do begin
      PoFile:=FileName;
      Text:=Filename; AddItem(Filename);
      s:=ExtractLastDir(ExtractFilePath(PoFile));
      if length(s)=2 then edLanguage.Text:=s;
      Result:=true;
      end
    else Result:=false;
    end;
  end;

procedure TfrmMain.bbPoFileClick(Sender: TObject);
begin
  if SelectPo then LoadPoFile;
  end;

procedure TfrmMain.btnHelpClick(Sender: TObject);
begin
  ShowHelp('tools.html#isstrans');
  end;

function TfrmMain.LoadPoFile : boolean;
var
  ne : integer;
begin
  Result:=false;
  if FileExists(PoFile) then begin
    ne:=PoList.LoadFromFile(PoFile);
    Result:=ne=0;
    if not Result then ErrorDialog(Format(_('Error in line %u of po file!'),[ne]));
    end
  else InfoDialog(Format(_('File not found: %s'),[PoFile]));
  end;

procedure TfrmMain.bbConvertClick(Sender: TObject);
var
  pe     : TPoEntry;
  so,s   : string;
  sl     : TStringList;
  i,j,k,n,m  : integer;
begin
  LangId:=Trim(edLanguage.Text);
  LoadPoFile;
  sl:=TStringList.Create;
  sl.Sorted:=true;
  with PoList do begin
    pe:=FindFirst;
    while pe<>nil do with pe do begin
      if (length(MsgId)>0) and (AutoCommentList.Count>0) then begin
        j:=0;
        with AutoCommentList do begin
          while (j<Count) and not AnsiStartsText('#:',Strings[j]) do inc(j);
          if (j<Count) then k:=posex(':',AutoCommentList[j],3)
          else k:=0;
          end;
        i:=0;
        with AutoCommentList do begin
          while (i<Count) and not AnsiStartsText('#.',Strings[i]) do inc(i);
          if (i<Count) then n:=pos(':',AutoCommentList[i])
          else n:=0;
          end;
        if (k>0) and (n>0) then begin
          if TryStrToInt(copy(AutoCommentList[j],k+1,1024),m) then begin
            s:=ZStrInt(m,3)+':'+LangId+'.'+Trim(copy(AutoCommentList[i],n+1,1024))+'=';
            if not Fuzzy then s:=s+MsgStr;
            sl.Add(s);
            end;
          end;
        end;
      pe:=FindNext(pe);
      end;
    end;
  so:=ExtractFilePath(PoFile)+AddExt(edOutName.Text,'txt');
  with sl do begin
    Sorted:=false;
    for i:=0 to Count-1 do Strings[i]:=copy(Strings[i],5,length(Strings[i]));
    StatusBar.SimpleText:=Format(_('%u strings written to "%s"'),[Count,so]);
    end;
  s:=NewExt(PoFile,'~'+GetExt(so));
  if FileExists(s) then DeleteFile(s);
  if FileExists(so) then RenameFile(so,s);
  sl.SaveToFile(so);
  sl.Free;
  end;


end.
