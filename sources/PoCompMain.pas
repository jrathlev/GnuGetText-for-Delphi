(* Tool for GnuGetText for Delphi
   Compare two po-files for differences in "msgstr"
   and update translation file with new id's

   © 2015-2024 Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de)
   based on the "dxgettext" programs by Lars B. Dybdahl

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.

   Feb. 2014
   last modified: September 2024
   *)

unit PoCompMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons,
  Vcl.ExtCtrls, Vcl.Menus, PoParser;

const
  Vers = ' - Vers. 3.2';

type
  TText = class (TObject)
  private
    FText : string;
  public
    constructor Create (const AText : string);
    property Text : string read FText write FText;
    end;

  TfrmMain = class(TForm)
    meRef: TMemo;
    meEdit: TMemo;
    bbUp: TBitBtn;
    bbFirst: TBitBtn;
    bbDown: TBitBtn;
    bbLast: TBitBtn;
    pnMain: TPanel;
    bbExit: TBitBtn;
    SaveDialog: TSaveDialog;
    bbInfo: TBitBtn;
    pnTools: TPanel;
    Label1: TLabel;
    meID: TMemo;
    laRefLine: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    laEdLine: TLabel;
    laEntry: TLabel;
    bbCopyId: TBitBtn;
    bbSave: TBitBtn;
    laEditFile: TLabel;
    Label5: TLabel;
    OpenDialog: TOpenDialog;
    bbCopyAll: TBitBtn;
    bbReload: TBitBtn;
    bbSaveChanges: TBitBtn;
    bbMUp: TBitBtn;
    bbMDn: TBitBtn;
    bbUndo: TBitBtn;
    cbEdit: TComboBox;
    bbEditList: TBitBtn;
    bbEditPo: TBitBtn;
    bbEditFile: TBitBtn;
    bbComp: TBitBtn;
    bbCopyName: TBitBtn;
    btnHelp: TBitBtn;
    cbComp: TComboBox;
    pmFileList: TPopupMenu;
    pmiEdit: TMenuItem;
    pmiClear: TMenuItem;
    N21: TMenuItem;
    pmiCancel: TMenuItem;
    bbCopyAndNext: TBitBtn;
    bbHeader: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure bbExitClick(Sender: TObject);
    procedure bbInfoClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure bbDownClick(Sender: TObject);
    procedure bbUpClick(Sender: TObject);
    procedure bbFirstClick(Sender: TObject);
    procedure bbLastClick(Sender: TObject);
    procedure bbCopyIdClick(Sender: TObject);
    procedure bbSaveClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure bbEditFileClick(Sender: TObject);
    procedure bbCompClick(Sender: TObject);
    procedure cbCompCloseUp(Sender: TObject);
    procedure bbCopyAllClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure cbEditCloseUp(Sender: TObject);
    procedure bbReloadClick(Sender: TObject);
    procedure bbSaveChangesClick(Sender: TObject);
    procedure meRefChange(Sender: TObject);
    procedure bbMDnClick(Sender: TObject);
    procedure bbMUpClick(Sender: TObject);
    procedure bbUndoClick(Sender: TObject);
    procedure bbCopyNameClick(Sender: TObject);
    procedure bbEditListClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure bbEditPoClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure pmiEditClick(Sender: TObject);
    procedure pmiClearClick(Sender: TObject);
    procedure bbCopyAndNextClick(Sender: TObject);
    procedure bbHeaderClick(Sender: TObject);
  private
    { Private-Deklarationen }
    ProgVersName,
    ProgVersDate,
    AppPath,UserPath,
    IniName,ProgPath,
    EditFile,CompFile,
    LastMsg,LastPrj    : string;
    RefList,EdList     : TPoEntryList;
    PeRef              : TPoEntry;
    IdList             : TStringList;
    Loaded,Changed     : boolean;
    ElWidth,
    IdIndex            : integer;
    procedure UpdateButtons;
    procedure SaveChangedFile;
    procedure SaveFile;
    procedure ShowEntry(AIndex : integer);
    procedure CopyEntry (AIndex : integer; UpdateRef : boolean);
    function LoadFiles (LoadEdit : boolean) : boolean;
    procedure GetCompFile (Replace : boolean = true);
    procedure SetCompFile;
    function SelectComp (const AFilename : string) : boolean;
    function SelectEdit : boolean;
  public
    { Public-Deklarationen }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

uses System.IniFiles, Winapi.ShellApi, Winapi.ShlObj, System.StrUtils,
  WinUtils, ListUtils, MsgDialogs, LangUtils, InitProg, WinApiUtils, WinShell,
  gnugettext, PathUtils, EditStringListDlg, GgtConsts, GgtUtils, EditHistListDlg,
  TextDlg;

{ ------------------------------------------------------------------- }
constructor TText.Create (const AText : string);
begin
  inherited Create;
  FText:=AText;
  end;

{ ------------------------------------------------------------------- }
const
  (* INI-Sections *)
  CompSekt = 'CompareFiles';

  (* INI-Variables *)
  iniLast = 'LastName';
  iniCount = 'FileCount';
  iniFilename = 'FileName';
  iniComp = 'CompareName';
  iniPrj = 'Project';

procedure TfrmMain.FormCreate(Sender: TObject);
var
  IniFile  : TMemIniFile;
  i,n : integer;
  s,t : string;
begin
  TranslateComponent (self);
  Application.Title:=_('Compare and merge translations in two po files');
  InitPaths(AppPath,UserPath,ProgPath);
  InitVersion(Application.Title,Vers,CopRgt,3,3,ProgVersName,ProgVersDate);
  Caption:=ProgVersName;
  CompFile:='';
  IniName:=Erweiter(AppPath,PrgName,IniExt);
  IniFile:=TMemIniFile.Create(IniName);
  with IniFile do begin
    Top:=ReadInteger(CfGSekt,iniTop,Top);
    Left:=ReadInteger(CfGSekt,iniLeft,Left);
    EditFile:=ReadString(CfGSekt,iniLast,'');
    ElWidth:=ReadInteger(CfgSekt,iniWidth,0);
    LastPrj:=ReadString(CfgSekt,iniPrj,'');
    n:=ReadInteger(FileSekt,iniCount,0);
    for i:=0 to n-1 do begin
      s:=ReadString(FileSekt,iniFilename+IntToStr(i),'');
      if (length(s)>0) and FileExists(s) then begin
        t:=ReadString(FileSekt,iniComp+IntToStr(i),'');
        cbEdit.AddItem(s,TText.Create(t));
        end;
      end;
    LoadHistory(IniFile,CompSekt,cbComp);
    Free;
    end;
  if ParamCount>0 then begin
    EditFile:=ParamStr(1);
    if ParamCount>1 then CompFile:=ParamStr(2);
    end;
  with cbEdit do begin
    if Items.Count>0 then ItemIndex:=0;
    if Items.Count<=1 then Style:=csSimple else Style:=csDropDown;
    end;
  with cbEdit do if length(EditFile)=0 then EditFile:=Text
  else if Items.IndexOf(EditFile)>=0 then AddToHistory(cbEdit,EditFile)
  else Items.InsertObject(0,EditFile,TText.Create(''));
  GetCompFile(false);
  cbComp.Text:=CompFile;
  LastMsg:='';
  RefList:=TPoEntryList.Create;
  EdList:=TPoEntryList.Create;
  IdList:=TStringList.Create;
  Loaded:=false; Changed:=false;
  end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FreeListObjects(cbEdit.Items);
  IdList.Free;
  RefList.Free;
  EdList.Free;
  end;

procedure TfrmMain.FormShow(Sender: TObject);
var
  ok : boolean;
begin
  if ElWidth>0 then EditStringListDialog.Width:=ElWidth;
  if FileExists(EditFile) and FileExists(CompFile) then LoadFiles(true)
  else begin
    if not FileExists(EditFile) then ok:=SelectEdit else ok:=true;
    ok:=ok and SelectComp(CompFile);
    if ok then LoadFiles(true) else Close;
    end;
  end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
var
  IniFile  : TMemIniFile;
  i : integer;
begin
  IniFile:=TMemIniFile.Create(IniName);
  with IniFile do begin
    Clear; Encoding:=TEncoding.UTF8;
    WriteInteger(CfGSekt,iniTop,Top);
    WriteInteger(CfGSekt,iniLeft,Left);
    WriteString(CfGSekt,iniLast,EditFile);
    WriteInteger(CfgSekt,iniWidth,EditStringListDialog.Width);
    WriteString(CfgSekt,iniPrj,LastPrj);
    EraseSection(FileSekt);
    with cbEdit.Items do begin
      WriteInteger(FileSekt,iniCount,Count);
      for i:=0 to Count-1 do begin
        WriteString(FileSekt,iniFilename+IntToStr(i),Strings[i]);
        WriteString(FileSekt,iniComp+IntToStr(i),(Objects[i] as TText).Text);
        end;
      end;
    SaveHistory(IniFile,CompSekt,true,cbComp);
    UpdateFile;
    Free;
    end;
  try HtmlHelp(0,nil,HH_CLOSE_ALL,0); except end;
  end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  SaveChangedFile;
  end;

procedure TfrmMain.GetCompFile (Replace : boolean);
var
  n : integer;
begin
  with cbEdit do begin
    n:=Items.IndexOf(EditFile);
    if n>=0 then begin
      ItemIndex:=n;
      if Replace or (length(CompFile)=0) then
        CompFile:=(Items.Objects[n] as TText).Text;
      end
    else begin
      Text:=EditFile;
      CompFile:='';
      end;
    end;
  cbComp.Text:=CompFile;
  end;

procedure TfrmMain.SetCompFile;
begin
  with cbEdit do if (length(CompFile)>0) and (ItemIndex>=0) then
    (Items.Objects[ItemIndex] as TText).Text:=CompFile;
  end;

procedure TfrmMain.SaveChangedFile;
begin
  if Changed and ConfirmDialog(BottomLeftPos(cbEdit,0,10),_('Save changed po file?')) then SaveFile;
  end;

procedure TfrmMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ssAlt in Shift then begin
    case Key of
    VK_Left  : bbCopyIdClick(Sender);
    VK_Up    : bbUpClick(Sender);
    VK_Down  : bbDownClick(Sender);
    VK_Prior : bbMUpClick(Sender);
    VK_Next  : bbMDnClick(Sender);
    VK_Home  : bbFirstClick(Sender);
    VK_End   : bbLastClick(Sender);
      end;
    end;
  end;

procedure TfrmMain.bbEditListClick(Sender: TObject);
var
  n  : integer;
begin
  n:=cbEdit.ItemIndex;
  if EditStringListDialog.Execute(BottomLeftPos(cbEdit,0,10),_('Edit file list'),_('po files:'),
      cbEdit.Items,[eoDelete],n) then begin
    with cbEdit do begin
      ItemIndex:=n;
      EditFile:=Items[ItemIndex];
      end;
    GetCompFile;
    end;
  end;

procedure TfrmMain.bbEditPoClick(Sender: TObject);
begin
  SaveFile;
  ShellExecute (0,'open',PChar(EditFile),nil,nil,SW_SHOWNORMAL);
  LoadFiles(true);
  end;

procedure TfrmMain.bbExitClick(Sender: TObject);
begin
  Close;
  end;

procedure TfrmMain.bbInfoClick(Sender: TObject);
begin
  InfoDialog(BottomLeftPos(meEdit,0,10),ProgVersName+' - '+ProgVersDate+#13+CopRgt
           +#13'E-Mail: '+EmailAdr);
  end;

procedure TfrmMain.bbCopyNameClick(Sender: TObject);
begin
  EditFile:=cbEdit.Text;
  if SelectComp(EditFile) then LoadFiles(false);
  end;

procedure TfrmMain.bbCompClick(Sender: TObject);
begin
  CompFile:=cbComp.Text;
  if SelectComp(CompFile) then LoadFiles(false);
  end;

function TfrmMain.SelectComp (const AFilename : string) : boolean;
begin
  with OpenDialog do begin
    if length(AFilename)>0 then begin
      InitialDir:=ExtractFilePath(AFilename);
      end
    else InitialDir:=ExtractFilePath(EditFile);
    if not DirectoryExists(InitialDir) then InitialDir:=UserPath;
    Filename:='';
    Title:=_('Select po file used for comparison with '+EditFile);
    Filter:=Format(_('po files|*.%s|all|*.*'),[PoExt]);
    if Execute then begin
      CompFile:=Filename;
      Text:=Filename; AddToHistory(cbComp,Filename);
      SetCompFile;
      Result:=true;
      end
    else Result:=false;
    end;
  end;

procedure TfrmMain.bbEditFileClick(Sender: TObject);
begin
  if Changed and ConfirmDialog(BottomLeftPos(cbEdit,0,10),_('Save changed po file?')) then bbSaveClick(Sender);
  EditFile:=cbEdit.Text;
  if SelectEdit then LoadFiles(true);
  end;

function TfrmMain.SelectEdit : boolean;
begin
  with OpenDialog do begin
    if length(EditFile)>0 then InitialDir:=ExtractFilePath(EditFile)
    else InitialDir:=UserPath;
    Filename:='';
    Title:=_('Select po file to be edited');
    Filter:=Format(_('po files|*.%s|all|*.*'),[PoExt]);
    if Execute then with cbEdit do begin
      EditFile:=FileName;
      if Items.IndexOf(EditFile)>=0 then AddToHistory(cbEdit,EditFile)
      else begin
        Items.InsertObject(0,EditFile,TText.Create(''));
        ItemIndex:=0;
        end;
      GetCompFile;
      end
    end;
  Result:=FileExists(CompFile) or SelectComp(EditFile);
  end;

procedure TfrmMain.cbCompCloseUp(Sender: TObject);
begin
  with cbComp do begin
    SetCompFile;
    if FileExists(CompFile) or SelectComp(EditFile) then LoadFiles(false)
    else laEntry.Caption:=_('No file for comparison specified!');
    end;
  end;

procedure TfrmMain.cbEditCloseUp(Sender: TObject);
begin
  with cbEdit do begin
    EditFile:=Items[ItemIndex];
    AddToHistory(cbEdit,EditFile);
    GetCompFile;
    if FileExists(CompFile) or SelectComp(EditFile) then LoadFiles(true)
    else laEntry.Caption:=_('No file for comparison specified!');
    end;
  end;

procedure TfrmMain.bbReloadClick(Sender: TObject);
begin
  EditFile:=cbEdit.Text;
  CompFile:=cbComp.Text;
  LoadFiles(true);
  end;

procedure TfrmMain.bbDownClick(Sender: TObject);
begin
  if IdIndex<IdList.Count-1 then inc(IdIndex);
  LastMsg:='';
  ShowEntry(IdIndex);
  end;

procedure TfrmMain.bbUpClick(Sender: TObject);
begin
  if IdIndex>0 then dec(IdIndex);
  LastMsg:='';
  ShowEntry(IdIndex);
  end;

procedure TfrmMain.btnHelpClick(Sender: TObject);
begin
  ShowHelp('tools.html#compare');
  end;

procedure TfrmMain.bbMUpClick(Sender: TObject);
begin
  if IdIndex>10 then dec(IdIndex,10) else IdIndex:=0;
  LastMsg:='';
  ShowEntry(IdIndex);
  end;

procedure TfrmMain.bbMDnClick(Sender: TObject);
begin
  if IdIndex<IdList.Count-11 then inc(IdIndex,10) else IdIndex:=IdList.Count-1;
  LastMsg:='';
  ShowEntry(IdIndex);
  end;

procedure TfrmMain.bbFirstClick(Sender: TObject);
begin
  IdIndex:=0;
  LastMsg:='';
  ShowEntry(IdIndex);
  end;

procedure TfrmMain.bbHeaderClick(Sender: TObject);
var
  sn : string;
begin
  sn:=ExtractLastDir(ExtractFilePath(cbEdit.Text));   // language shortcut
  if TextDialog(BottomRightPos(bbHeader),
      Format(_('%s - Language: %s'),[ExtractFileName(cbEdit.Text),AnsiUpperCase(sn)]),
      'Project-ID:',LastPrj) then begin
    with RefList do begin
      Header[hiProjectId]:=LastPrj;
      Header[hiRevisionDate]:=CurrentTimestamp;
      Header[hiLastTranslator]:=EdList.Header[hiLastTranslator];
      Header[hiLanguageTeam]:=EdList.Header[hiLanguageTeam];
      Header[hiLanguage]:=EdList.Header[hiLanguage];
      end;
    bbSave.Enabled:=true; Changed:=true;
    end;
  end;

procedure TfrmMain.bbLastClick(Sender: TObject);
begin
  IdIndex:=IdList.Count-1;
  LastMsg:='';
  ShowEntry(IdIndex);
  end;

procedure TfrmMain.UpdateButtons;
var
  i : integer;
begin
  with pnTools do for i:=0 to ControlCount-1 do if (Controls[i] is TBitBtn) and (Tag=1) then
    Controls[i].Enabled:=IdList.Count>1;
  end;

procedure TfrmMain.ShowEntry(AIndex : integer);
var
  si : string;
  pe : TPoEntry;
begin
  laEntry.Caption:=Format(_('Entry %u of %u different strings'),[IdIndex+1,IdList.Count]);
  meRef.Clear; meEdit.Clear;
  if (AIndex>=0) and (AIndex<IdList.Count) then begin
    si:=IdList[AIndex];
    meID.Text:=si;
    PeRef:=RefList.FindEntry(si);
    pe:=EdList.FindEntry(si);
    if PeRef<>nil then begin
      with meRef do begin
        Text:=ReplaceStr(PeRef.MsgStr,#10,sLineBreak);
        with Font do if PeRef.Fuzzy then begin
          Color:=clTeal; Style:=[fsBold];
          end
        else if IdList.Objects[AIndex]<>nil then begin
          Color:=clGreen; Style:=[fsBold];
          end
        else begin
          Color:=clBlack; Style:=[];
          end;
        end;
      laRefLine.Caption:=_('Line number: ')+IntToStr(PeRef.IdLine);
      end;
    if pe<>nil then begin
      with meEdit do begin
        Tag:=AIndex;
        Text:=ReplaceStr(Pe.MsgStr,#10,sLineBreak);
        with Font do if Pe.Fuzzy then begin
          Color:=clRed; Style:=[fsBold];
          end
        else begin
          Color:=clBlack; Style:=[];
          end;
        end;
      laEdLine.Caption:=_('Line number: ')+IntToStr(Pe.IdLine);
      end;
    end;
  bbUndo.Enabled:=length(LastMsg)>0;
  bbSaveChanges.Enabled:=false;
  end;

procedure TfrmMain.bbCopyIdClick(Sender: TObject);
begin
  CopyEntry (IdIndex,false);
  ShowEntry(IdIndex);
  end;

procedure TfrmMain.bbCopyAndNextClick(Sender: TObject);
begin
  CopyEntry (IdIndex,false);
  ShowEntry(IdIndex);
  bbDownClick(Sender);
  end;

procedure TfrmMain.CopyEntry (AIndex : integer; UpdateRef : boolean);
var
  si : string;
  pe : TPoEntry;
begin
  si:=IdList[AIndex];
  meID.Text:=si;
  if UpdateRef then PeRef:=RefList.FindEntry(si);
  pe:=EdList.FindEntry(si);
  if (pe<>nil) then with PeRef do begin
    LastMsg:=MsgStr;
    MsgStr:=pe.MsgStr;
    Fuzzy:=false;
    end;
  IdList.Objects[AIndex]:=pointer(1);
  bbSave.Enabled:=true; Changed:=true;
  end;

procedure TfrmMain.bbUndoClick(Sender: TObject);
begin
  if LastMsg<>'' then with PeRef do MsgStr:=LastMsg;
  LastMsg:='';
  IdList.Objects[IdIndex]:=nil;
  ShowEntry(IdIndex);
  end;


procedure TfrmMain.bbCopyAllClick(Sender: TObject);
var
  i : integer;
begin
  with IdList do begin
    for i:=0 to Count-1 do CopyEntry(i,true);
    IdIndex:=Count-1;
    end;
  ShowEntry(IdIndex);
  end;

procedure TfrmMain.meRefChange(Sender: TObject);
begin
  bbSaveChanges.Enabled:=true;
  end;

procedure TfrmMain.pmiClearClick(Sender: TObject);
begin
  if ConfirmDialog(CursorPos,'Clear whole list of files?') then with (Sender as TComboBox) do begin
    Clear; FreeListObjects(Items); Style:=csSimple;
    end;
  end;

procedure TfrmMain.pmiEditClick(Sender: TObject);
var
  n : integer;
  cbx : TComboBox;
begin
  cbx:=pmFileList.PopupComponent as TComboBox;
  EditHistList(CursorPos,'',cbx.Hint,cbx,true,true,n);
  end;

procedure TfrmMain.bbSaveChangesClick(Sender: TObject);
begin
  with PeRef do begin
    LastMsg:=MsgStr;
    MsgStr:=ReplaceStr(meRef.Text,sLineBreak,#10);
    Fuzzy:=false;
    end;
  IdList.Objects[IdIndex]:=pointer(1);
  bbSave.Enabled:=true; Changed:=true;
  ShowEntry(IdIndex);
  end;

function TfrmMain.LoadFiles (LoadEdit : boolean) : boolean;
var
  pr,pe : TPoEntry;
  si : string;
  ne : integer;
begin
  if LoadEdit then SaveChangedFile;
  meRef.Clear; meEdit.Clear; meID.Clear; IdList.Clear;
  laRefLine.Caption:='';
  laEdLine.Caption:='';
  Result:=false;
  if LoadEdit then begin
    if not FileExists(EditFile) then begin
      laEntry.Caption:=Format(_('File not found: %s'),[EditFile]);
      Exit;
      end;
    Changed:=false;
    end;
  try
    if LoadEdit then begin
      ne:=RefList.LoadFromFile(EditFile);
      if ne>0 then begin
        laEntry.Caption:=Format(_('Error in line %u of edit file!'),[ne]);
        Exit;
        end;
      end;
    ne:=EdList.LoadFromFile(CompFile);
    if ne>0 then begin
      laEntry.Caption:=Format(_('Error in line %u of comparison file!'),[ne]);
      Exit;
      end;
    with RefList do begin
      pr:=FindFirst;
      while (pr<>nil) do begin
        if (pr.MsgId<>'') and (copy(pr.MsgId,1,2)<>'##') then begin
          si:=pr.MsgId;
          pe:=EdList.FindEntry(si);
          if (pe<>nil) and (length(pe.MsgStr)>0) then begin
            if not pe.Fuzzy and (pr.MsgStr<>pe.MsgStr) then IdList.Add(si);
            end;
          end;
        pr:=FindNext(pr);
        end;
      end;
    UpdateButtons;
    IdIndex:=0;
    if IdList.Count>0 then ShowEntry(0)
    else laEntry.Caption:=_('No different strings found!');
    bbSave.Enabled:=false;
    Result:=true;
  except
    laEntry.Caption:=_('Error reading po files!');
    end;
  with laEditFile do begin
    Caption:=_('po file to be edited:');
    if Result then Caption:=Caption+' ('+Format(_('%u strings'),[RefList.TotalEntries])+')';
    end;
  end;

procedure TfrmMain.bbSaveClick(Sender: TObject);
begin
  SaveFile;
  end;

procedure TfrmMain.SaveFile;
var
  sn,s   : string;
begin
  sn:=EditFile+'.tmp';
  RefList.SaveToFile(sn,true);
  s:=EditFile+'.bak';
  if FileExists(s) then DeleteFile(s);
  RenameFile(EditFile,s);
  RenameFile(sn,EditFile);
  bbSave.Enabled:=false; Changed:=false;
  end;

end.
