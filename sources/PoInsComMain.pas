(*  Tool for GnuGetText for Delphi
   Copy the translated strings belonging to the same id from one po file ínto
   another one as comments
   Purpose: help for people who are not able to translate from English

   © Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.

   Feb. 2014
   last modified: April 2024
   *)

unit PoInsComMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, PoParser;

const
  Vers = ' - Vers. 3.1';
  CopRgt = '© 2014-2024 Dr. J. Rathlev, 24222 Schwentinental';
  EmailAdr = 'kontakt(a)rathlev-home.de';

  PoExt = 'po';

type
  TfrmMain = class(TForm)
    Label2: TLabel;
    Label5: TLabel;
    bbInfo: TBitBtn;
    bbExit: TBitBtn;
    bbSave: TBitBtn;
    OpenDialog: TOpenDialog;
    bbRef: TBitBtn;
    bbEdit: TBitBtn;
    btnHelp: TBitBtn;
    edRef: TComboBox;
    edEdit: TComboBox;
    laStatus: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure bbExitClick(Sender: TObject);
    procedure bbInfoClick(Sender: TObject);
    procedure bbRefClick(Sender: TObject);
    procedure bbEditClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure edRefCloseUp(Sender: TObject);
    procedure edEditCloseUp(Sender: TObject);
    procedure bbSaveClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
  private
    { Private-Deklarationen }
    ProgVersName,
    ProgVersDate,
    AppPath,UserPath,
    IniName,ProgPath,
    RefFile,EdFile     : string;
    RefList,EdList     : TPoEntryList;
    IdList             : TStringList;
    Loaded,Changed     : boolean;
    function SelectEdit : boolean;
    function SelectRef : boolean;
    function LoadRefFile : boolean;
    function LoadEditFile : boolean;
  public
    { Public-Deklarationen }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

uses System.IniFiles, Winapi.ShlObj, System.StrUtils, WinUtils,  ListUtils, LangUtils,
  InitProg, WinApiUtils, WinShell,gnugettext, PathUtils, MsgDialogs, GgtConsts, GgtUtils;

{ ------------------------------------------------------------------- }
procedure TfrmMain.FormCreate(Sender: TObject);
var
  IniFile  : TMemIniFile;
begin
  TranslateComponent (self);
  Application.Title:=_('Copy translated strings as comments');
  InitPaths(AppPath,UserPath,ProgPath);
  InitVersion(Application.Title,Vers,CopRgt,3,3,ProgVersName,ProgVersDate);
  Caption:=ProgVersName;
  IniName:=Erweiter(AppPath,PrgName,IniExt);
  IniFile:=TMemIniFile.Create(IniName);
  with IniFile do begin
    RefFile:=ReadString(CfGSekt,iniTempl,'');
    EdFile:=ReadString(CfGSekt,iniTrans,'');
    LoadHistory(IniFile,TemplSekt,edRef);
    LoadHistory(IniFile,TransSekt,edEdit);
    Free;
    end;
  AddToHistory(edRef,RefFile);
  AddToHistory(edEdit,EdFile);
  RefList:=TPoEntryList.Create;
  EdList:=TPoEntryList.Create;
  IdList:=TStringList.Create;
  Loaded:=false; Changed:=false;
  end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  if FileExists(RefFile) then LoadRefFile else edRef.Text:='';
  if FileExists(EdFile) then LoadEditFile else edEdit.Text:='';
  laStatus.Caption:='';
  end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
var
  IniFile  : TMemIniFile;
begin
  IniFile:=TMemIniFile.Create(IniName);
  with IniFile do begin
    WriteString(CfGSekt,iniTempl,RefFile);
    WriteString(CfGSekt,iniTrans,EdFile);
    SaveHistory(IniFile,TemplSekt,edRef);
    SaveHistory(IniFile,TransSekt,edEdit);
    UpdateFile;
    Free;
    end;
  IdList.Free;
  RefList.Free;
  EdList.Free;
  try HtmlHelp(0,nil,HH_CLOSE_ALL,0); except end;
  end;

procedure TfrmMain.bbExitClick(Sender: TObject);
begin
  Close;
  end;

procedure TfrmMain.bbInfoClick(Sender: TObject);
begin
  InfoDialog(BottomLeftPos(bbInfo,0,10),ProgVersName+' - '+ProgVersDate+#13+CopRgt
           +#13'E-Mail: '+EmailAdr);
  end;

procedure TfrmMain.edEditCloseUp(Sender: TObject);
begin
  with edEdit do begin
    EdFile:=Items[ItemIndex];
    LoadEditFile;
    end;
  end;

procedure TfrmMain.edRefCloseUp(Sender: TObject);
begin
  with edRef do begin
    RefFile:=Items[ItemIndex];
    LoadRefFile;
    end;
  end;

function TfrmMain.SelectRef : boolean;
begin
  with OpenDialog do begin
    if length(RefFile)>0 then InitialDir:=ExtractFilePath(RefFile)
    else InitialDir:=UserPath;
    Filename:='';
    Title:=_('Select po file for comments');
    Filter:=Format(_('po files|*.%s|all|*.*'),[PoExt]);
    if Execute then begin
      RefFile:=FileName;
      AddToHistory(edRef,Filename);
      Result:=true;
      end
    else Result:=false;
    end;
  end;

function TfrmMain.SelectEdit : boolean;
begin
  with OpenDialog do begin
    if length(EdFile)>0 then InitialDir:=ExtractFilePath(EdFile)
    else InitialDir:=UserPath;
    Filename:='';
    Title:=_('Select po file to be edited');
    Filter:=Format(_('po files|*.%s|all|*.*'),[PoExt]);
    if Execute then begin
      EdFile:=Filename;
      AddToHistory(edEdit,Filename);
      Result:=true;
      end
    else Result:=false;
    end;
  end;

procedure TfrmMain.bbEditClick(Sender: TObject);
begin
  if SelectEdit then LoadEditFile;
  end;

procedure TfrmMain.bbRefClick(Sender: TObject);
begin
  if SelectRef then LoadRefFile;
  end;

function TfrmMain.LoadRefFile : boolean;
var
  ne : integer;
begin
  IdList.Clear;
  Result:=false;
  if FileExists(RefFile) then begin
    ne:=RefList.LoadFromFile(RefFile);
    if ne>0 then
      ErrorDialog(Format(_('Error in line %u of file "%s"'),[ne,RefFile]));
    end
  else InfoDialog(Format(_('File not found: %s'),[RefFile]));
  laStatus.Caption:='';
  end;

function TfrmMain.LoadEditFile : boolean;
var
  ne : integer;
begin
  Result:=false;
  if FileExists(EdFile) then begin
    ne:=EdList.LoadFromFile(EdFile);
    if ne>0 then
      ErrorDialog(Format(_('Error in line %u of file "%s"'),[ne,EdFile]));
    end
  else InfoDialog(Format(_('File not found: %s'),[EdFile]));
  laStatus.Caption:='';
  end;

procedure TfrmMain.bbSaveClick(Sender: TObject);
var
  pr,pe  : TPoEntry;
  sn,s   : string;
  ok     : boolean;
  i,n    : integer;

  function Convert (msg : string) : string;
  var
    p : integer;
    part : string;
  begin
    Result:='';
    repeat
      p:=FindBestBreak (msg,70);
      part:=copy(msg,1,p);
      delete (msg,1,length(part));
      Result:=Result+'# '+String2PO(part,false)+sLineBreak;
      until length(msg)=0;
    end;

begin
  with EdList do begin
    pe:=FindFirst;
    while pe<>nil do begin
      s:=pe.MsgId;
      if length(s)>0 then begin
        pr:=RefList.FindEntry(s);
        if pr<>nil then begin
          pe.UserCommentList.Text:=Convert(pr.MsgStr);
          inc(n);
          end;
        end;
      pe:=FindNext(pe);
      end;
    end;
  laStatus.Caption:=Format(_('%u comments inserted'),[n]);
  sn:=EdFile+'.tmp';
  EdList.SaveToFile(sn,true);
  s:=EdFile+'.bak';
  if FileExists(s) then DeleteFile(s);
  RenameFile(EdFile,s);
  RenameFile(sn,EdFile);
  end;

procedure TfrmMain.btnHelpClick(Sender: TObject);
begin
  ShowHelp('tools.html#comment');
  end;

end.
