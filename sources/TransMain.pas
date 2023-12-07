
(* Process translations using GunGetText for Delphi
   replaces context menu startable programs
   - ggdxgettext
   - ggmerge

   © 2015 - 2023 Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.

   J. Rathlev, Oct. 2010
     June 2013: added single file selection
     February 2021 :  template stored as relative path in ini file
   Vers. 3.0   - July 2023 : template creation also from c/cpp files
       requires XGetText.exe Windows binary from
       https://mlocati.github.io/articles/gettext-iconv-windows.html
   Vers. 3.1   - August 2023: enhanced po header management
                              highlighting of languages which need revision
   last modified: September 2023
   *)

unit TransMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  Vcl.ComCtrls, Vcl.Buttons, System.ImageList, Vcl.ImgList, Vcl.Menus,
  GgtConsts, HListBox, xgettext;

const
  ProgName = 'Process GnuGetText translations';
  Vers = '3.1';

  defOutput = 'default';
  defCopyDir ='locale\%s\LC_MESSAGES\';
  defLang = '4'; // German

type
  TfrmTransMain = class(TForm)
    Label1: TLabel;
    cbProjDir: THistoryCombo;
    btProjDir: TSpeedButton;
    edLangSubDir: TLabeledEdit;
    cbLanguage: TComboBox;
    EditMask: TLabeledEdit;
    cbRecurse: TCheckBox;
    cbCreateIgnore: TCheckBox;
    cbRemoveIgnore: TCheckBox;
    gbDomain: TGroupBox;
    rbDefault: TRadioButton;
    rbOther: TRadioButton;
    OutputName: TLabeledEdit;
    paFiles: TPanel;
    paTop: TPanel;
    meProgress: TMemo;
    pnStatus: TPanel;
    laProgress: TLabel;
    btExtract: TBitBtn;
    EndeBtn: TBitBtn;
    cbOverwrite: TCheckBox;
    btMerge: TBitBtn;
    cbBackup: TCheckBox;
    gbCopy: TGroupBox;
    rbSingle: TRadioButton;
    rbMulti: TRadioButton;
    edTargetDir: TLabeledEdit;
    btCopyDir: TSpeedButton;
    btCopy: TBitBtn;
    cbPoEdit: TCheckBox;
    btPoEdit: TBitBtn;
    btAssemble: TBitBtn;
    btnInfo: TBitBtn;
    btEditIgnore: TBitBtn;
    OpenDialog: TOpenDialog;
    rbMask: TRadioButton;
    rbFiles: TRadioButton;
    pcMode: TPageControl;
    tsMask: TTabSheet;
    tsFiles: TTabSheet;
    cbFiles: THistoryCombo;
    Label2: TLabel;
    btnNew: TBitBtn;
    btnEdit: TBitBtn;
    ExcludeDirs: TLabeledEdit;
    pcOptions: TPageControl;
    tsLang: TTabSheet;
    tsOptions: TTabSheet;
    lbLang: TListBox;
    btSelNone: TBitBtn;
    btSelAll: TBitBtn;
    bbAdd: TBitBtn;
    bbRem: TBitBtn;
    bbUp: TBitBtn;
    bbDown: TBitBtn;
    Label4: TLabel;
    ilMerge: TImageList;
    pmDirectorytList: TPopupMenu;
    pmiEdit: TMenuItem;
    pmiClear: TMenuItem;
    N21: TMenuItem;
    pmiCancel: TMenuItem;
    btnClipBoard: TBitBtn;
    laLineNr: TLabel;
    btDefMask: TSpeedButton;
    tsSaveOptions: TTabSheet;
    cbOrder: TCheckBox;
    bbExclude: TSpeedButton;
    FileOpenDialog: TFileOpenDialog;
    btStatus: TBitBtn;
    rgEncoding: TRadioGroup;
    btnHelp: TBitBtn;
    btnManual: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btProjDirClick(Sender: TObject);
    procedure EndeBtnClick(Sender: TObject);
    procedure rbDefaultClick(Sender: TObject);
    procedure btExtractClick(Sender: TObject);
    procedure cbProjDirCloseUp(Sender: TObject);
    procedure btMergeClick(Sender: TObject);
    procedure btCopyDirClick(Sender: TObject);
    procedure btCopyClick(Sender: TObject);
    procedure btPoEditClick(Sender: TObject);
    procedure btAssembleClick(Sender: TObject);
    procedure rbSingleClick(Sender: TObject);
    procedure btnInfoClick(Sender: TObject);
    procedure btEditIgnoreClick(Sender: TObject);
    procedure rbMultiClick(Sender: TObject);
    procedure rbMaskClick(Sender: TObject);
    procedure rbFilesClick(Sender: TObject);
    procedure btnNewClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure cbRecurseClick(Sender: TObject);
    procedure bbAddClick(Sender: TObject);
    procedure bbRemClick(Sender: TObject);
    procedure lbLangClick(Sender: TObject);
    procedure btSelNoneClick(Sender: TObject);
    procedure btSelAllClick(Sender: TObject);
    procedure bbUpClick(Sender: TObject);
    procedure bbDownClick(Sender: TObject);
    procedure cbPoEditClick(Sender: TObject);
    procedure pmiClearClick(Sender: TObject);
    procedure pmiEditClick(Sender: TObject);
    procedure btnClipBoardClick(Sender: TObject);
    procedure btDefMaskClick(Sender: TObject);
    procedure cbComboBoxDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure bbExcludeClick(Sender: TObject);
    procedure btStatusClick(Sender: TObject);
    procedure lbLangDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
      State: TOwnerDrawState);
    procedure btnHelpClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnManualClick(Sender: TObject);
  private
    { Private-Deklarationen }
    ProgVersName,
    ProgVersDate,
    AppPath,UserPath,
    IniName,ProgPath,
    LastDir,
    TextEditor        : string;
    ErrorOutput       : TStringList;
    WarnCount         : integer;
    procedure GetSelectedIndex (n : integer);
    function GetFileFilter : string;
    function GetGenerator : string;
    function GetLangId (AIndex : integer) : string;
    function GetLangName (AIndex : integer) : string;
    function GetLangSubDir (AIndex : integer) : string;
    procedure SetLangMarker (AIndex : integer; Mark : boolean);
    function CheckPoFile (const PoFile : string) : boolean;
    function GetTemplName : string;
    procedure LoadGetTextSettings (const ADir : string);
    procedure SaveGetTextSettings;
    procedure LoadMergeSettings(const AFilename : string);
    procedure SaveMergeSettings(const AFilename : string);
    procedure RemoveBOM(const PoName : string);
    function CompileToMo (const PoFile,MergePath : string) : boolean;
    procedure Merge (AIndex : integer);
    procedure CopyMo (AIndex : integer);
    procedure Progress (const CurrentTask,CurrentFileName:string; LineNumber:Integer);
    procedure Warning (WarningType:TWarningType; const Msg,Line,Filename:string;LineNumber:Integer);
    procedure OverwriteQuestion (sender: TObject; const aFileName: string; var Overwrite: boolean);
  public
    { Public-Deklarationen }
{$IFDEF HDPI}   // scale glyphs and images for High DPI
    procedure AfterConstruction; override;
{$EndIf}
  end;

var
  frmTransMain: TfrmTransMain;

implementation

{$R *.dfm}

uses System.IniFiles, System.Win.Registry, System.Types, Winapi.ShellApi,
  System.DateUtils, System.TimeSpan, System.UITypes, GgtUtils,
  StringUtils, WinUtils, MsgDialogs, GnuGetText, LangUtils, InitProg, ShellDirDlg,
  ExtSysUtils, WinApiUtils, PathUtils, FileUtils, StrUtils, PoParser, SelectListItems,
  AssembleEngine, FileListDlg, SelectDlg, ShowText, ExecuteApp, EditHistListDlg,
  ListSelectDlg, PoStatDlg;

{ ---------------------------------------------------------------- }
const
// external programs
//  MsgMerge = 'msgmerge.exe';
  MsgFmt = 'msgfmt.exe';

  (* INI-Sektionen *)
  StatSekt = 'PoStat';

  (* INI-Variablen *)
  iniLangDir  = 'LanguageDir';
  iniLanguage = 'Language';
  iniLast     = 'LastDir';
  IniEditor   = 'Editor';

procedure TfrmTransMain.FormCreate(Sender: TObject);
var
  IniFile  : TIniFile;
  n        : integer;
begin
  TranslateComponent(self);
  Application.Title:=_('Process GnuGetText translations for Delphi');
  InitPaths(AppPath,UserPath,ProgPath);
  InitVersion(Application.Title,Vers,CopRgt,3,3,ProgVersName,ProgVersDate);
  Caption:=ProgVersName;
  IniName:=Erweiter(AppPath,PrgName,IniExt);
  IniFile:=TIniFile.Create(IniName);
  with IniFile do begin
    edLangSubdir.Text:=ReadString(CfgSekt,iniLangDir,'languages');
    with cbLanguage do begin
      n:=ReadInteger(CfgSekt,iniLanguage,ItemIndex);
      if (n>=0) and (n<Items.Count) then ItemIndex:=n;
      end;
    LastDir:=ReadString(CfgSekt,IniLast,'');
    Top:=ReadInteger(CfgSekt,iniTop,Top);
    Left:=ReadInteger(CfgSekt,iniLeft,Left);
    ClientWidth:=ReadInteger (CfgSekt,IniWidth,ClientWidth);
    ClientHeight:=ReadInteger (CfgSekt,IniHeight,ClientHeight);
    TextEditor:=ReadString(CfgSekt,IniEditor,'');
    Free;
    end;
  cbProjDir.LoadFromIni(IniName,DirSekt);
  lbLang.Items.NameValueSeparator:='-';
  ErrorOutput:=TStringList.Create;
  end;

{$IFDEF HDPI}   // scale glyphs and images for High DPI
procedure TfrmTransMain.AfterConstruction;
begin
  inherited;
  ScaleImageList(ilMerge,PixelsPerInchOnDesign,Monitor.PixelsPerInch);
  ScaleButtonGlyphs(self,PixelsPerInchOnDesign,Monitor.PixelsPerInch);
  end;
{$EndIf}

procedure TfrmTransMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  try HtmlHelp(0,nil,HH_CLOSE_ALL,0); except end;
  end;

procedure TfrmTransMain.FormDestroy(Sender: TObject);
begin
  with TIniFile.Create(IniName) do begin
    WriteString(CfgSekt,iniLangDir,edLangSubdir.Text);
    WriteInteger(CfgSekt,iniLanguage,cbLanguage.ItemIndex);
    WriteString(CfgSekt,IniLast,cbProjDir.Text);
    WriteInteger(CfgSekt,iniTop,Top);
    WriteInteger(CfgSekt,iniLeft,Left);
    WriteInteger (CfgSekt,IniWidth,ClientWidth);
    WriteInteger (CfgSekt,IniHeight,ClientHeight);
    WriteString(CfgSekt,IniEditor,TextEditor);
    Free;
    end;
  ErrorOutput.Free;
  end;

procedure TfrmTransMain.FormShow(Sender: TObject);
var
  i : integer;
  s  : string;
begin
  PoStatDialog.LoadFromIni(IniName,StatSekt);
  with ListSelectDialog do begin
    Clear; Sorted:=false;
    AddItem(_('Delphi files'),0);
    AddItem(_('Lazarus files'),1);
    AddItem(_('Cpp Builder files'),2);
    OnSelect:=GetSelectedIndex;
    end;
  pcOptions.ActivePageIndex:=0;
  with btMerge do begin
    Glyph:=nil;
    if cbPoedit.Checked then ilMerge.GetBitmap(1,Glyph)
    else ilMerge.GetBitmap(0,Glyph);
    end;
  ShowTextDialog.LoadFromIni(IniName,'ShowErrors');
  if (length(TextEditor)=0) or not FileExists(TextEditor) then begin
    with TRegistry.Create do begin
      Access:=KEY_READ;
      RootKey:=HKEY_CLASSES_ROOT;
      s:='';
      try
        if OpenKey('.txt',false) then begin
          s:=ReadString('');
          CloseKey;
          if OpenKey(s+'\shell\open\Command',false) then s:=ReadString('')
          else s:='';
          s:=ReadNxtQuotedStr(s,#32,'"');
          end;
      finally
        Free;
        end;
      end;
    if (length(s)>0) and FileExists(s) then TextEditor:=s
    else with OpenDialog do begin
      Title:=_('Select text editor');
      InitialDir:=ProgPath;
      Filter:=_('Programs')+'|*.exe|'+_('all')+'|*.*';
      Filename:='';
      if Execute then begin
        TextEditor:=Filename;
        btEditIgnore.Enabled:=true;
        end
      else btEditIgnore.Enabled:=false;
      end;
    end;
  with cbProjDir do begin
    if not DirectoryExists(LastDir) then begin
      for i:=0 to Items.Count-1 do if DirectoryExists(Items[i]) then Break;
      if i<Items.Count then LastDir:=Items[i]
      else begin
        LastDir:='';
        if not ShellDirDialog.Execute(_('Select project directory'),true,true,false,UserPath,LastDir) then Close
        else AddItem(LastDir);
        end;
      end;
    ItemIndex:=Items.IndexOf(LastDir);
    if ItemIndex<0 then ItemIndex:=Items.Add(LastDir);
    LoadGetTextSettings(Text);
    end;
  end;

procedure TfrmTransMain.cbComboBoxDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
begin
  with Control as TComboBox,Canvas do begin
    if (odComboBoxEdit in State) then Font.Color:=clWindowText;
    with Brush do if (odSelected in State) and not (odComboBoxEdit in State) then Color:=clHighlight
    else Color:=clWindow;
    FillRect(Rect);
    TextOut(Rect.Left, Rect.Top,' '+Items[Index]);
    end;
  end;

procedure TfrmTransMain.GetSelectedIndex (n : integer);
begin
  with EditMask do begin
    case n of
    1:  Text:=defLazarusMask;
    2 : Text:=defCppMask;
    else Text:=defDelphiMask;
      end;
    end;
  end;

procedure TfrmTransMain.pmiClearClick(Sender: TObject);
begin
  if ConfirmDialog(CursorPos,'Clear whole directory list?') then with cbProjDir do begin
    HistoryList.ClearAll; UpdateList;
    end;
  end;

procedure TfrmTransMain.pmiEditClick(Sender: TObject);
var
  n : integer;
begin
  with cbProjDir do if EditHistListDialog.Execute(CursorPos,'',_('Project directories'),HistoryList,true,true,n)
    then UpdateList;
  end;

procedure TfrmTransMain.btnInfoClick(Sender: TObject);
begin
  InfoDialog(ProgVersName+' - '+ProgVersDate+sLineBreak+
           VersInfo.CopyRight+sLineBreak+'E-Mail: '+EmailAdr);
  end;

procedure TfrmTransMain.btnManualClick(Sender: TObject);
begin
  ShowManual(Application.Handle);
  end;

procedure TfrmTransMain.EndeBtnClick(Sender: TObject);
begin
  SaveGetTextSettings;
  Close;
  end;

const
// "ggdxgettext" inifile
  ggtSect = 'ggdxgettext';
  dxSect = 'dxgettext';
  trSect = 'ggttranslate';

  iniUseMask = 'usemask';
  iniFilenames   = 'files';
  iniPoedit = 'StartEditor';
  iniMulti  = 'MultiCopy';
  iniCopyPath = 'CopyPath';
  iniSelLang  = 'SelectedLanguages';

  ReqTrans = $1000;   // requires editing of translation
  LangMask = $FFF;

procedure TfrmTransMain.LoadGetTextSettings (const ADir : string);
var
  n,i : integer;
  s : string;
begin
  // dxgettext.ini einlesen
  with TIniFile.Create(IncludeTrailingPathDelimiter(ADir)+DxGetTextIni) do begin
    cbRecurse.Checked:=ReadBool(ggtSect,iniRecurse,false);
    ExcludeDirs.Enabled:=cbRecurse.Checked;
    ExcludeDirs.Text:=ReadString(ggtSect,iniExclude,'');
    if ReadBool(ggtSect,iniUseMask,true) then rbMask.checked:=true
    else rbFiles.checked:=true;
    cbCreateIgnore.Checked:=ReadBool(ggtSect,iniUpdate,false);
    cbRemoveIgnore.Checked:=ReadBool(ggtSect,iniIgnore,false);
//    cbCheckNonAscii.Checked:=not ReadBool(ggtSect,iniAAscii,true);
    rgEncoding.ItemIndex:=ReadInteger(ggtSect,iniCode,0);
    if ReadBool(ggtSect,iniDefault,False) then begin
      rbOther.Checked:=true;
      with OutputName do begin
        Text:=ReadString(dxSect,iniOutput,defOutput);
        Enabled:=true;
        end;
      end
    else begin
      rbDefault.Checked:=true;
      with OutputName do begin
        Text:=''; Enabled:=false;
        end;
      end;
    EditMask.Text:=ReadString(dxSect,iniMask,defDelphiMask);
    cbOrder.Checked:=ReadBool(dxSect,iniOrder,false);
    with cbFiles do begin
      Text:=ReadString(ggtSect,iniFilenames,'');
      if Canvas.TextWidth(Text)<=Width then Hint:=_('Insert filenames separated by commas')
      else Hint:=Text;
      end;
    if rbMask.Checked then pcMode.ActivePageIndex:=0
    else pcMode.ActivePageIndex:=1;
    cbOverwrite.Checked:=ReadBool(dxSect,iniOvwr,false);
    with edLangSubdir do Text:=ReadString(trSect,iniLangDir,Text);
    s:=ReadString(trSect,iniSelLang,defLang);
    if length(s)=0 then s:=defLang;
    lbLang.Clear;
    repeat
      n:=ReadNxtInt(s,',',0);
      if n>0 then lbLang.AddItem(cbLanguage.Items[n-1],pointer(n));
      until length(s)=0;
    lbLang.Selected[0]:=true;
    btPoEdit.Enabled:=true;
    btMerge.Caption:=_('Merge translation with template');
    cbPoedit.Checked:=ReadBool(trSect,iniPoedit,true);
    if ReadBool(trSect,iniMulti,false) then rbMulti.Checked:=true
    else rbSingle.Checked:=true;
    edTargetDir.Text:=ReadString(trSect,iniCopyPath,ADir);
    Free;
    end;
//  btMerge.Enabled:=false;
  with lbLang do for i:=0 to Count-1 do begin
    s:=IncludeTrailingPathDelimiter(ADir)+IncludeTrailingPathDelimiter(GetLangSubDir(i));
    s:=NewExt(s+GetTemplName,PoExt);
    SetLangMarker(i,CheckPoFile(s));
    end;
  meProgress.Clear;
  laProgress.Caption:=_('Settings loaded for this project');
  end;

procedure TfrmTransMain.SaveGetTextSettings;
var
  s : string;
  i : integer;
begin
  // dxgettext.ini schreiben
  with TIniFile.Create(IncludeTrailingPathDelimiter(cbProjDir.Text)+DxGetTextIni) do begin
    WriteBool(ggtSect,iniRecurse,cbRecurse.Checked);
    WriteString(ggtSect,iniExclude,ExcludeDirs.Text);
    WriteBool(ggtSect,iniUseMask,rbMask.checked);
    WriteBool(ggtSect,iniUpdate,cbCreateIgnore.Checked);
    WriteBool(ggtSect,iniIgnore,cbRemoveIgnore.Checked);
//    WriteBool(ggtSect,iniAAscii,not cbCheckNonAscii.Checked);
    WriteInteger(ggtSect,iniCode,rgEncoding.ItemIndex);
    WriteBool(ggtSect,iniDefault,rbOther.Checked);
    if rbOther.Checked then WriteString(dxSect,iniOutput,OutputName.Text)
    else WriteString(dxSect,iniOutput,'');
    WriteString(dxSect,iniMask,EditMask.Text);
    WriteBool(dxSect,iniOrder,cbOrder.Checked);
    WriteString(ggtSect,iniFilenames,cbFiles.Text);
    WriteBool(dxSect,iniOvwr,cbOverwrite.Checked);
    WriteString(trSect,iniLangDir,edLangSubdir.Text);
    s:='';
    with lbLang.Items do for i:=0 to Count-1 do s:=s+','+IntToStr(integer(Objects[i]) and LangMask);
    if length(s)>0 then Delete(s,1,1);
    WriteString(trSect,iniSelLang,s);
    WriteBool(trSect,iniPoedit,cbPoedit.Checked);
    WriteBool(trSect,iniMulti,rbMulti.Checked);
    WriteString(trSect,iniCopyPath,edTargetDir.Text);
    Free;
    end;
  end;

const
  ggmSect = 'ggmerge';

  iniTemplate = 'template';
  iniBackup = 'createbackup';

procedure TfrmTransMain.LoadMergeSettings(const AFilename : string);
begin
  with TIniFile.Create(NewExt(AFilename,IniExt)) do begin
//    TemplPath:=ReadString(ggmSect,iniTemplate,NewExt(IncludeTrailingPathDelimiter(cbProjDir.Text)+GetTemplName,PoExt));
//    TemplPath:=ExtractRelativePath(ExtractFilePath(AFilename),TemplPath);
    cbBackup.Checked:=ReadBool(ggmSect,iniBackup,true);
    Free;
    end;
  end;

procedure TfrmTransMain.SaveMergeSettings(const AFilename : string);
begin
  with TIniFile.Create(NewExt(AFilename,IniExt)) do begin
//    WriteString(ggmSect,iniTemplate,TemplPath);
    WriteBool(ggmSect,iniBackup,cbBackup.Checked);
    Free;
    end;
  end;

procedure TfrmTransMain.lbLangClick(Sender: TObject);
begin
  with btPoEdit do begin
    Enabled:=lbLang.SelCount=1;
    if Enabled then btMerge.Caption:=_('Merge translation with template')
    else btMerge.Caption:=_('Merge translations with template');
    if Enabled then btCopy.Caption:=_('Copy MO file to binary folder of project')
    else btCopy.Caption:=_('Copy MO files to binary folder of project');
    bbUp.Enabled:=Enabled;
    bbDown.Enabled:=Enabled;
    end;
  end;

procedure TfrmTransMain.lbLangDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var
  rt : boolean;
begin
  with (Control as TListBox),Canvas do begin
    rt:=integer(Items.Objects[Index]) and ReqTrans <>0;
    if odSelected in State then begin
      with Brush do if rt then Color:=clSkyblue
      else Color:=clHighlight;
      with Font do if rt then Color:=TColors.Red
      else Color:=clHighlightText;
      end
    else begin
      Brush.Color:=clWindow;
      with Font do if rt then Color:=TColors.Orangered
      else Color:=clWindowText;
      end;
    FillRect(Rect);
    TextOut(Rect.Left+5,Rect.Top,Items[Index]);
    end;
  end;

procedure TfrmTransMain.bbAddClick(Sender: TObject);
begin
  with lbLang do begin
    ClearSelection;
    Selected[Items.AddObject(cbLanguage.Text,pointer(cbLanguage.ItemIndex+1))]:=true;
    end;
  end;

procedure TfrmTransMain.bbRemClick(Sender: TObject);
var
  i : integer;
begin
  with lbLang do begin
    for i:=Count-1 downto 0 do if Selected[i] then Items.Delete(i);
    if Count>0 then Selected[0]:=true;
    end;
  end;

procedure TfrmTransMain.bbUpClick(Sender: TObject);
var
  n : integer;
begin
  with lbLang,Items do if (Count>0) and (ItemIndex>0) then begin
    n:=ItemIndex;
    Exchange(n,n-1);
    Selected[n-1]:=true;
    end;
  end;

procedure TfrmTransMain.bbDownClick(Sender: TObject);
var
  n : integer;
begin
  with lbLang,Items do if (Count>0) and (ItemIndex<Count-1) then begin
    n:=ItemIndex;
    Exchange(n,n+1);
    Selected[n+1]:=true;
    end;
  end;

procedure TfrmTransMain.bbExcludeClick(Sender: TObject);
var
  sd : string;
  i  : integer;
begin
  sd:=cbProjDir.Text;
  if length(sd)=0 then sd:=UserPath;
  with FileOpenDialog do begin
    Title:=_('Select subdirectories to be excluded');
    Options := [fdoPickFolders,fdoAllowMultiSelect,fdoForceFileSystem];
    OkButtonLabel:=_('Select');
    DefaultFolder:=sd;
    FileName:=sd;
    if Execute then begin
      sd:=MakeRelativePath(cbProjDir.Text,Files[0]);
      for i:=1 to Files.Count-1 do sd:=sd+','+MakeRelativePath(cbProjDir.Text,Files[i]);
      if length(ExcludeDirs.Text)>0 then ExcludeDirs.Text:=ExcludeDirs.Text+',';
      ExcludeDirs.Text:=ExcludeDirs.Text+sd;
      end;
    end;
  end;

procedure TfrmTransMain.btProjDirClick(Sender: TObject);
var
  s : string;
begin
  s:=cbProjDir.Text;
  if ShellDirDialog.Execute(_('Select project source directory'),false,true,false,'',s) then begin
    SaveGetTextSettings;
    with cbProjDir do begin
      AddItem(s);
      ItemIndex:=0;
      LoadGetTextSettings(Text);
      end;
    end;
  end;

procedure TfrmTransMain.btSelAllClick(Sender: TObject);
begin
  lbLang.SelectAll;
  end;

procedure TfrmTransMain.btSelNoneClick(Sender: TObject);
begin
  with lbLang do begin
    ClearSelection;
    Selected[0]:=true;
    end;
  end;

procedure TfrmTransMain.btStatusClick(Sender: TObject);
var
  PoFile,ss : string;
begin
  with lbLang do ss:=GetLangSubDir(ItemIndex);
  ss:=IncludeTrailingPathDelimiter(cbProjDir.Text)+IncludeTrailingPathDelimiter(ss);
  PoFile:=NewExt(ss+GetTemplName,PoExt);
  PoStatDialog.Execute(PoFile);
  end;

procedure TfrmTransMain.btnNewClick(Sender: TObject);   // new file list
var
  s : string;
begin
  s:=FileListDialog.Execute(_('Create new file list'),GetFileFilter,
       cbProjDir.Text,'','','',false);
  if length(s)>0 then with cbFiles do begin
    Text:=s; AddItem(s);
    if Canvas.TextWidth(Text)<=Width then Hint:=_('Insert filenames separated by commas')
    else Hint:=Text;
    SaveGetTextSettings;
    end;
  end;

procedure TfrmTransMain.btnEditClick(Sender: TObject);
var
  s,st : string;
begin
  if length(cbFiles.Text)=0 then st:=_('Create new file list')
  else st:=_('Edit file list');
  s:=FileListDialog.Execute(st,GetFileFilter,
       cbProjDir.Text,cbFiles.Text,'','',false);
  if length(s)>0 then with cbFiles do begin
    Text:=s; AddItem(s);
    if Canvas.TextWidth(Text)<=Width then Hint:=_('Insert filenames separated by commas')
    else Hint:=Text;
    SaveGetTextSettings;
    end;
  end;

procedure TfrmTransMain.btnHelpClick(Sender: TObject);
begin
  ShowHelp('tools.html#translate');
  end;

procedure TfrmTransMain.btnClipBoardClick(Sender: TObject);
begin
  with meProgress do begin
    if SelLength=0 then SelectAll;
    CopyToClipboard;
    SelLength:=0;
    end;
  end;

function TfrmTransMain.GetFileFilter : string;
begin
  Result:=_('Delphi files')+'|'+ReplChars(defDelphiMask,#32,';')+
       '|'+_('Lazarus files')+'|'+ReplChars(defLazarusMask,#32,';')+
       '|'+_('Cpp Builder files')+'|'+ReplChars(defCppMask,#32,';')+
       '|'+_('all')+'|*.*'
  end;

procedure TfrmTransMain.btDefMaskClick(Sender: TObject);
begin
  ListSelectDialog.ShowList(BottomLeftPos(btDefMask,0,2));
  end;

procedure TfrmTransMain.cbPoEditClick(Sender: TObject);
begin
  with btMerge do begin
    Glyph:=nil;
    if cbPoedit.Checked then ilMerge.GetBitmap(1,Glyph)
    else ilMerge.GetBitmap(0,Glyph);
    end;
  end;

procedure TfrmTransMain.cbProjDirCloseUp(Sender: TObject);
begin
  SaveGetTextSettings;
  with cbProjDir do LoadGetTextSettings (Items[ItemIndex]);
  end;

procedure TfrmTransMain.cbRecurseClick(Sender: TObject);
begin
  ExcludeDirs.Enabled:=cbRecurse.Checked;
  end;

procedure TfrmTransMain.btCopyDirClick(Sender: TObject);
var
  s : string;
begin
  s:=edTargetDir.Text;
  if length(s)=0 then s:=ExtractFilePath(cbProjDir.Text);
  if ShellDirDialog.Execute(_('Select program directory of project'),
      false,true,false,cbProjDir.Text,s) then begin
    edTargetDir.Text:=s;
    end;
  end;

procedure TfrmTransMain.rbDefaultClick(Sender: TObject);
begin
  OutputName.Enabled:=rbOther.Checked;
  end;

procedure TfrmTransMain.rbFilesClick(Sender: TObject);
begin
  pcMode.ActivePageIndex:=1;
  end;

procedure TfrmTransMain.rbMaskClick(Sender: TObject);
begin
  pcMode.ActivePageIndex:=0;
  end;

procedure TfrmTransMain.rbMultiClick(Sender: TObject);
begin
  btAssemble.Enabled:=rbSingle.Checked;
  edTargetDir.EditLabel.Caption:=_('Parent directory holding the project executables:');
  end;

procedure TfrmTransMain.rbSingleClick(Sender: TObject);
begin
  btAssemble.Enabled:=rbSingle.Checked;
  edTargetDir.EditLabel.Caption:=_('Directory of project executables:');
  end;

procedure TfrmTransMain.Progress(const CurrentTask, CurrentFileName: string;
  LineNumber: Integer);
begin
  if length(CurrentTask)>0 then begin
    laProgress.Caption:=CurrentTask;
    laProgress.Update;
    meProgress.Lines.Add(CurrentTask);
//    laLineNr.Caption:=_('Line:')+' 1';
    end;
  if LineNumber>0 then with laLineNr do begin
    Caption:=Format(_('Line:')+' %u',[LineNumber]);
    Update;
    end;
  Application.ProcessMessages;
  end;

procedure TfrmTransMain.Warning(WarningType: TWarningType; const Msg, Line, Filename: string; LineNumber: Integer);
begin
  with meProgress do begin
    Lines.Add('* '+Msg);
    if LineNumber>0 then Lines.Add('  '+Format(_('Line: %u'),[Linenumber]));
    if length(Line)>0 then Lines.Add('  '+Format(_('Last line read: %s'),[Line]));
//    Lines.Add('');
    end;
  inc(WarnCount);
  end;

procedure TfrmTransMain.OverwriteQuestion(sender: TObject; const aFileName: string; var Overwrite: boolean);
begin
  Overwrite:=cbOverwrite.Checked
  or ConfirmDialog(Format(_('Do you want to overwrite the file named %s?'),[aFilename]));
  end;

function TfrmTransMain.GetGenerator : string;
begin
  Result:=DelExt(ExtractFileName(Application.ExeName))+' '
       +ChangeFileExt(VersInfo.Version,''); // version without build
  end;

{ ---------------------------------------------------------------- }
procedure TfrmTransMain.btExtractClick(Sender: TObject);
var
  xgt  : TXGetText;
  si   : string;

  procedure Explode (line : string; sl : TStrings);
  var
    i,last : integer;
    item   : string;
  begin
    last:=1;
    line:=line+' ';
    for i:=1 to length(line) do begin
      if line[i]<=#32 then begin
        item:=trim(copy(line,last,i-last));
        if item<>'' then sl.Add (item);
        last:=i;
        end;
      end;
    end;

begin
  SaveGetTextSettings;
  btMerge.Enabled:=false;
  xgt:=TXGetText.Create;
  meProgress.Clear;
  WarnCount:=0;
  with xgt do begin
    ExePath:=PrgPath;
    Recurse:=cbRecurse.Checked;
    OrderbyMsgid:=cbOrder.Checked;
    SetExcludeDirs(ExcludeDirs.Text);
    UpdateIgnore:=cbCreateIgnore.Checked;
    UseIgnoreFile:=cbRemoveIgnore.Checked;
    DestinationPath:=IncludeTrailingPathDelimiter(cbProjDir.Text);
    Generator:=GetGenerator;
    AddBaseDirectory(DestinationPath);
    AllowNonAscii:=rgEncoding.ItemIndex>0; //not cbCheckNonAscii.Checked;
    case rgEncoding.ItemIndex of
    1 : CodePage:=cpLatin1;
    2 : CodePage:=cpUtf8;
    else CodePage:=0;
      end;
    if rbOther.Checked then defaultDomain:=OutputName.Text;
    NoWildcards:=rbFiles.Checked;
    if NoWildcards then filemasks.CommaText:=cbFiles.Text
    else Explode(EditMask.Text,filemasks);
    si:=DestinationPath+sIgnoreList;
//    if not FileExists(si) then begin
//      if FileExists(sIgnoreList) then CopyFileTS(sIgnoreList,si) // copy from install dir
//      else si:='';
//      end;
    IgnoreListFile:=si;
    OnProgress:=Progress;
    OnWarning:=Warning;
    OnOverwrite:=OverwriteQuestion;
    Application.ProcessMessages;
    try
      Execute;
    finally
      Free;
      end;
    end;
  with meProgress.Lines do begin
    if WarnCount>0 then Add('==> '+Format(_('%u warnings or errors'),[WarnCount]));
    Add('');
    end;
  laProgress.Caption:=_('Template was created');
  laLineNr.Caption:='';
  btMerge.Enabled:=true;
  end;

procedure TfrmTransMain.btMergeClick(Sender: TObject);
var
  i    : integer;
begin
  with lbLang do if SelCount=1 then Merge(ItemIndex)
  else for i:=0 to Count-1 do if Selected[i] then Merge(i);
  end;

{******************************************************************************}
function StartAndWait(const ExecuteFile, ParamString: string): boolean;
//http://delphi.about.com/library/weekly/aa040803a.htm
{******************************************************************************}
var
   SEInfo : TShellExecuteInfo;
   ExitCode : DWORD;
begin
   Result:=False;
   if not FileExists(ExecuteFile) then Exit;
   FillChar(SEInfo,SizeOf(TShellExecuteInfo), 0);
   with SEInfo do begin
     cbSize:=SizeOf(TShellExecuteInfo);
     fMask:=SEE_MASK_NOCLOSEPROCESS;
     Wnd:=Application.Handle;
     lpFile:=PChar(ExecuteFile);
     lpParameters:=PChar(ParamString);
     nShow:=SW_SHOWNORMAL;
     end;
   if ShellExecuteEx(@SEInfo) then begin
     repeat
       Application.ProcessMessages;
       // Damit die Prozessorauslastung sinkt :-)
       Sleep(100);
       GetExitCodeProcess(SEInfo.hProcess, ExitCode);
       until (ExitCode <> STILL_ACTIVE) or Application.Terminated;
     Result:=True;
     end;
  end;

procedure TfrmTransMain.RemoveBOM(const PoName : string);
begin
  // Check if translation starts with BOM - remove it because msgmerge/msgfmt cannot process it
  if CheckUtf8Bom(PoName)<0 then with TStringList.Create do begin
    Sorted:=false; WriteBOM:=false;
    LoadFromFile(PoName); SaveToFile(PoName);
    Free;
    end;
  end;

function TfrmTransMain.CompileToMo (const PoFile,MergePath : string) :  boolean;
var
  res : integer;
begin
  RemoveBOM(PoFile);
  res:=StartProgram(PrgPath+MsgFmt,PoFile+' -o '+NewExt(PoFile,MoExt),MergePath,ErrorOutput);
  if res=0 then meProgress.Lines.Add(Format(_('Compiled to MO file: %s'),[PoFile]))
  else begin
    if res>0 then ErrorDialog(Format(_('Compiling to MO file failed:'
        +sLineBreak+'"%s" reports exitcode %u'),['msgfmt.exe',res]))
    else ErrorDialog(Format(_('Execution of "%s" failed:'),['msgfmt.exe'])
                 +sLineBreak+SystemErrorMessage(abs(res)));
    meProgress.Lines.Add(Format(_('  *** Error compiling to MO file: %s'),[PoFile]));
    end;
  Result:=res=0;
  end;

function TfrmTransMain.GetTemplName : string;
begin
  if rbOther.Checked then Result:=OutputName.Text
  else Result:='default';
  end;

function TfrmTransMain.GetLangId (AIndex : integer) : string;
begin
  Result:=Trim(lbLang.Items.Names[AIndex]);
  end;

function TfrmTransMain.GetLangName (AIndex : integer) : string;
var
  n : integer;
begin
  with lbLang.Items do begin
    Result:=Trim(Strings[AIndex]);
    n:=AnsiPos(NameValueSeparator,Result);
    if n>0 then Result:=Trim(copy(Result,n+1,length(Result)));
    end;
  end;

function TfrmTransMain.GetLangSubDir (AIndex : integer) : string;
begin
  Result:=edLangSubDir.Text;
  if TextPos('%s',Result)=0 then Result:=IncludeTrailingPathDelimiter(Result)+'%s';
  Result:=Format(Result,[GetLangId(AIndex)]);
  end;

procedure TfrmTransMain.SetLangMarker (AIndex : integer; Mark : boolean);
var
  n : integer;
begin
  with lbLang.Items do begin
    if Mark then n:=ReqTrans else n:=0;
    Objects[AIndex]:=pointer(integer(Objects[AIndex]) and LangMask +n);
    end;
  end;

// returns true if translation is complete
function TfrmTransMain.CheckPoFile (const PoFile : string) : boolean;
var
  pe : TPoEntry;
begin
  Result:=false;
  if FileExists(PoFile) then with TPoEntryList.Create do if LoadFromFile(PoFile)=0 then begin  // check for new entries
    pe:=FindFirst;
    while not Result and (pe<>nil) do begin
      with pe do if not (AnsiStartsStr('##',MsgId) or MsgId.IsEmpty) then begin // skip history entries
        if length(MsgStr)=0 then Result:=true   // new msgstr
        else if Fuzzy then Result:=true;
        end;
      pe:=FindNext(pe);
      end;
    Free;
    end;
  end;

procedure TfrmTransMain.Merge (AIndex : integer);
var
  translist : TPoEntryList;
  pe,petr   : TPoEntry;
  parser    : TPoParser;
  PoHeader  : TPoHeader;
  tf        : TextFile;
  fs        : TFileStream;
  MergePath,PoFile,
  sv,sx,scd,st      : string;
  res,i     : integer;
  ok,cphd   : boolean;
begin
  st:=GetTemplName;
  MergePath:=IncludeTrailingPathDelimiter(cbProjDir.Text)+IncludeTrailingPathDelimiter(GetLangSubDir(AIndex));
  PoFile:=NewExt(MergePath+st,PoExt);
  LoadMergeSettings(MergePath+st);
  // Template
  sv:=NewExt(IncludeTrailingPathDelimiter(cbProjDir.Text)+st,PoExt);
  // Translation
  if not DirectoryExists(MergePath) then ForceDirectories(MergePath);
  cphd:=FileExists(PoFile);  // copy header after merging
  if not cphd then CopyFileTS(sv,PoFile); // Init translation
  sx:=NewExt(PoFile,PoxExt);
  meProgress.Lines.Add(Format(_('Merging with %s template'),[GetLangName(AIndex)]));
  ok:=true;
// Always use internal merge function
  FileMode:=fmOpenRead;
//  if rgEncoding.ItemIndex=0 then cp:=cpUtf8 else cp:=cpLatin1;
  translist:=TPoEntryList.Create;
  try
    i:=translist.LoadFromFile(PoFile);
    if i>0 then begin
      ErrorDialog(Format(_('Error in line %u of file "%s"'),[i,PoFile]));
      ok:=false;
      end;
    if ok then begin
      scd:=CurrentTimestamp;
      AssignFile (tf,sv,cpUtf8);    // read template always as Utf-8
      Reset (tf);
      try
        parser:=TPoParser.Create;
        try
          fs:=TFileStream.Create (sx,fmCreate);
          try
            while true do begin
              pe:=parser.ReadNextEntry(tf);
              if pe=nil then break;
              if pe.MsgId.IsEmpty then with PoHeader do begin
                GetFromString(pe.MsgStr);
                if not Items[hiCreationDate].IsEmpty then scd:=Items[hiCreationDate];
                end;
              petr:=translist.FindEntry(pe.MsgId);
              if petr<>nil then begin
                if pe.MsgId.IsEmpty then with translist do begin  // header
                  Header[hiCreationDate]:=scd;
                  Header[hiRevisionDate]:=CurrentTimestamp;
                  Header[hiLanguage]:=GetLangId(AIndex);
                  Header[hiXGenerator]:=GetGenerator;
                  UpdateHeader(petr);
                  pe.Assign(petr);
                  end
                else begin  // normal entry
                  pe.MsgStr:=petr.MsgStr;
                  pe.UserCommentList.Text:=petr.UserCommentList.Text;
                  end;
                end;
              pe.WriteToStream(fs);
              end;
          finally
            FreeAndNil (fs);
            end;
        finally
          FreeAndNil (parser);
          end;
      finally
        CloseFile (tf);
        end;
      end;
  finally
    FreeAndNil (translist);
    end;
  if ok then begin
    sv:=NewExt(PoFile,'~'+PoExt);
    if FileExists(sv) then DeleteFile(sv);
    if cbBackup.Checked then begin
      if FileExists(PoFile) then ok:=RenameFile(PoFile,sv)
      else ok:=true;
      if not ok then ErrorDialog(Format(_('Cannot rename %s to %s'),[PoFile,sv]));
      end
    else begin
      ok:=deletefile (PoFile);
      if not ok then ErrorDialog(Format(_('Cannot delete %s'),[PoFile]));
      end;
    if ok then begin
      ok:=FileExists(sx);
      if ok then begin
        ok:=RenameFile(sx,PoFile);
        if not ok then ErrorDialog(Format(_('Cannot rename %s to %s'),[sx,PoFile]))
        end
      end;
    end;
  if ok then begin
    SetLangMarker(AIndex,CheckPoFile(PoFile));
    lbLang.Invalidate;
    SaveMergeSettings(MergePath+st);
    laProgress.Caption:=_('The template was merged into the translation file');
    if cbPoEdit.Checked then begin // and (lbLang.SelCount=1) then
      StartAndWait(PoFile,'');
      SetLangMarker(AIndex,CheckPoFile(PoFile));
//      ShellExecute (Application.Handle,'open',PChar(PoFile),nil,nil,SW_SHOWNORMAL)
      end
    else CompileToMo(PoFile,MergePath);
    end
  else laProgress.Caption:=_('Merging of template failed');
  meProgress.Lines.Add('');
  end;

{ ---------------------------------------------------------------- }
procedure TfrmTransMain.btCopyClick(Sender: TObject);
var
  s    : string;
  i,n  : integer;
begin
  if length(edTargetDir.Text)=0 then btCopyDirClick(Sender);
  if DirectoryExists(edTargetDir.Text) then with lbLang do begin
    n:=0;
    s:=GetLangSubDir(ItemIndex);
    if SelCount=1 then begin
      CopyMo(ItemIndex);
      inc(n);
      end
    else for i:=0 to Count-1 do if Selected[i] then begin
      CopyMo(i);
      inc(n);
      end;
    if n>0 then laProgress.Caption:=Format(_('%u files processed'),[n])
    else laProgress.Caption:=_('No files processed');
    end
  else ErrorDialog(Format(_('Folder for executables not found: '),[edTargetDir.Text]));
  meProgress.Lines.Add('');
  SaveGetTextSettings;
  end;

procedure TfrmTransMain.CopyMo (AIndex : integer);
var
  MergePath,PoFile,
  sm,sd : string;
  ok    : boolean;

  function CopyToDirs (const Dir,SubFolder,Filename : string) : boolean;
  var
    DirInfo    : TSearchRec;
    Findresult : integer;
    s          : string;
  begin
    Result:=true;
    FindResult:=FindFirst (Erweiter(Dir,'*',''),faAnyFile,DirInfo);
    while FindResult=0 do with DirInfo do begin
      if NotSpecialDir(Name) then begin
        if((Attr and faDirectory)<>0) then
          Result:=CopyToDirs(Erweiter(Dir,Name,''),SubFolder,Filename) and Result;
        end;
      FindResult:=FindNext (DirInfo);
      end;
    FindClose(DirInfo);
    if AnsiEndsText(SubFolder,IncludeTrailingPathDelimiter(Dir)) then begin
      meProgress.Lines.Add('  ==> '+Dir);
      s:=IncludeTrailingPathDelimiter(Dir)+ExtractFilename(sm);
      if not CopyFile(pchar(sm),pchar(s),false) then begin
        meProgress.Lines.Add(_('  *** Error copying file:'));
        meProgress.Lines.Add  ('      '+s);
        Result:=false;
        end;
      end;
    end;

begin
  MergePath:=IncludeTrailingPathDelimiter(cbProjDir.Text)+IncludeTrailingPathDelimiter(GetLangSubDir(AIndex));
  sm:=MergePath+GetTemplName;
  PoFile:=NewExt(sm,PoExt);
  LoadMergeSettings(sm);
  sm:=NewExt(PoFile,MoExt);
  if (GetFileLastWriteDateTime(PoFile))>GetFileLastWriteDateTime(sm) then ok:=CompileToMo(PoFile,MergePath)
  else ok:=true;
  if ok then begin
    meProgress.Lines.Add(Format(_('Copying MO file %s'),[sm]));
    sd:=Format(defCopyDir,[GetLangId(AIndex)]);
    if rbSingle.Checked then begin
      ForceDirectories(IncludeTrailingPathDelimiter(edTargetDir.Text)+sd);
      if FileExists(sm) then begin
        sd:=IncludeTrailingPathDelimiter(edTargetDir.Text)+sd+ExtractFilename(sm);
        try
          meProgress.Lines.Add('  ==> '+sd);
          CopyFileTS(sm,sd);
          ok:=true;
        except
          on E:EInOutError do begin
            meProgress.Lines.Add(_('  *** Error copying file:'));
            meProgress.Lines.Add('  '+E.Message);
            ok:=false;
            end;
          end;
        end
      else begin
        meProgress.Lines.Add(_('  *** Error file not found:'));
        meProgress.Lines.Add  ('      '+sm);
        ok:=false;
        end;
      end
    else begin  // multi copy
      ok:=CopyToDirs(edTargetDir.Text,sd,sm);
      end;
    end;
  if ok then laProgress.Caption:=_('MO file was copied')
  else laProgress.Caption:=_('Copying of MO file failed');
  end;

{ ---------------------------------------------------------------- }
procedure TfrmTransMain.btPoEditClick(Sender: TObject);
var
  PoFile,ss : string;
begin
  with lbLang do ss:=GetLangSubDir(ItemIndex);
  ss:=IncludeTrailingPathDelimiter(cbProjDir.Text)+IncludeTrailingPathDelimiter(ss);
  PoFile:=NewExt(ss+GetTemplName,PoExt);
  StartAndWait(PoFile,'');
  SetLangMarker(lbLang.ItemIndex,CheckPoFile(PoFile));
//  ShellExecute (0,'open',PChar(PoFile),nil,nil,SW_SHOWNORMAL);
  end;

procedure TfrmTransMain.btEditIgnoreClick(Sender: TObject);
begin
  ShellExecute (0,'open',PChar(TextEditor),
    PChar(NewExt(IncludeTrailingPathDelimiter(cbProjDir.Text)+'Ignore',PoExt)),nil,SW_SHOWNORMAL);
  ShellExecute (0,'open',PChar(TextEditor),
    PChar(NewExt(IncludeTrailingPathDelimiter(cbProjDir.Text)+GetTemplName,PoExt)),nil,SW_SHOWNORMAL);
  end;

{ ---------------------------------------------------------------- }
procedure TfrmTransMain.btAssembleClick(Sender: TObject);
var
  DirInfo    : TSearchRec;
  Findresult : integer;
  sl         : TStringList;
  Ext        : string;
  i,n        : integer;
  AssEng     : TAssembleEngine;
  ok         : boolean;
begin
  if DirectoryExists(edTargetDir.Text) then begin
    AssEng:=TAssembleEngine.Create(edTargetDir.Text);
    sl:=TStringList.Create;
    ok:=false;
    with AssEng do begin
      SetGnuGettextPatchCode;
      filemask:='*.mo';
      PrepareFileList;
      if FileList.Count=0 then ErrorDialog(_('No translations (.mo files) found'))
      else begin
        with Filelist do for i:=0 to Count-1 do sl.AddObject(Strings[i],pointer(true));
        if SelectListItemsDialog.Execute(TopRightPos(btAssemble,Point(0,-100)),_('Embed translations'),
            _('Select the translations you want to embed into your executables'),
            edTargetDir.Text,sl) then begin
          with sl do for i:=0 to Count-1 do if not boolean(Objects[i]) then SkipFile (Strings[i]);
          ok:=true;
          end;
        end;
      end;
    n:=-1;
    if ok then begin
      sl.Clear;
      FindResult:=FindFirst (Erweiter(edTargetDir.Text,'*',''),faAnyFile,DirInfo);
      while FindResult=0 do with DirInfo do begin
        if NotSpecialDir(Name) then begin
          Ext:=GetExt(Name);
          if((Attr and faDirectory)=0) and (AnsisameText(Ext,'exe') or AnsiSameText(Ext,'dll')) then
            sl.AddObject(Name,pointer(true));
          end;
        FindResult:=FindNext (DirInfo);
        end;
      FindClose(DirInfo);
      if SelectListItemsDialog.Execute(TopRightPos(btAssemble,Point(0,-100)),_('Executable files'),
                  _('Select all executables where you want to embed the translations'),
                  edTargetDir.Text,sl) then with sl do begin
        n:=0;
        for i:=0 to Count-1 do if boolean(Objects[i]) then begin
          meProgress.Lines.Add(Format(_('Embedding translation in %s'),[Strings[i]]));
          with AssEng do begin
            exefilename:=IncludeTrailingPathDelimiter(edTargetDir.Text)+Strings[i];
            try
              Execute;
              inc(n);
            except
              on E: Exception do begin
                meProgress.Lines.Add(_('  *** Error on embedding translations:'));
                meProgress.Lines.Add('  '+E.Message);
                end;
              end;
            end;
          end;
        end;
      end;
    sl.Free;
    AssEng.Free;
    if n>0 then laProgress.Caption:=Format(_('%u files processed'),[n])
    else laProgress.Caption:=_('No files processed');
    meProgress.Lines.Add('');
    end
  else ErrorDialog(Format(_('Folder for executables not found: '),[edTargetDir.Text]));
  end;

end.
