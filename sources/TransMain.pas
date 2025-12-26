(* Process translations using GunGetText for Delphi
   replaces context menu startable programs
   - ggdxgettext
   - ggmerge

   © 2015 - 2025 Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de)

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
   Vers. 3.1   - August 2023:   enhanced po header management
                                highlighting of languages which need revision
               - April 2024:    TMemInFile instead aof TIniFile
   Vers. 3.2   - August 2024:   external loadable language list
                                language selections are saved by shortcuts instead of index
   Vers. 3.3   - October 2024:  optional merging of AutoComments and HistComments
                                optional merging with similar msgids
   Vers. 3.4   - December 2025: Entry for Project-Id-Version added

   last modified: December 2025
   *)

unit TransMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  Vcl.ComCtrls, Vcl.Buttons, System.ImageList, Vcl.ImgList, Vcl.Menus,
  GgtConsts, xgettext;

const
  ProgName = 'Process GnuGetText translations';
  Vers = '3.3';

  defOutput = 'default';
  defCopyDir ='locale\%s\LC_MESSAGES\';
  defLang = 'de'; // German

type
  TfrmTransMain = class(TForm)
    Label1: TLabel;
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
    ilGlyphs: TImageList;
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
    cbProjDir: TComboBox;
    cbFiles: TComboBox;
    btUpdate: TBitBtn;
    tsMerge: TTabSheet;
    gbLangMerge: TGroupBox;
    cbMergeAutoComments: TCheckBox;
    cbMergeHistory: TCheckBox;
    laMergeLanguage: TLabel;
    cbMergeSimilar: TCheckBox;
    edSimLength: TEdit;
    udSimLength: TUpDown;
    gbSaveLanguage: TGroupBox;
    laSaveLanguage: TLabel;
    sbSetAllBu: TSpeedButton;
    bbImport: TBitBtn;
    btEdit: TBitBtn;
    gbEdit: TGroupBox;
    ProjectName: TLabeledEdit;
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
    procedure btUpdateClick(Sender: TObject);
    procedure MergeOptionClick(Sender: TObject);
    procedure sbSetAllBuClick(Sender: TObject);
    procedure bbImportClick(Sender: TObject);
    procedure btEditClick(Sender: TObject);
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
    XGetText          : TXGetText;
    procedure GetSelectedIndex (n : integer);
    function GetFileFilter : string;
    function GetGenerator : string;
    function GetLangId (AIndex : integer) : string;
    function GetLangName (AIndex : integer) : string;
    function GetLangSubDir (AIndex : integer) : string;
    procedure ShowLangOptions;
    procedure SetLangMarker (AIndex : integer; Mark : boolean);
    function CheckPoFile (const PoFile : string) : boolean;
    function GetTemplName : string;
    procedure LoadGetTextSettings (const ADir : string);
    procedure SaveGetTextSettings;
    procedure LoadMergeSettings(const AFilename : string);
    procedure SaveMergeSettings(const AFilename : string; ac,hc,bu : boolean);
    procedure RemoveBOM(const PoName : string);
    function CompileToMo (const PoFile,MergePath : string) : boolean;
    function Merge (AIndex : integer) : boolean;
    procedure CopyMo (AIndex : integer; var FCount,ECount : integer);
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
  System.DateUtils, System.TimeSpan, System.UITypes, GgtUtils, ListUtils,
  StringUtils, WinUtils, MsgDialogs, GnuGetText, LangUtils, InitProg, ShellDirDlg,
  ExtSysUtils, WinApiUtils, PathUtils, FileUtils, StrUtils, PoParser, SelectListItems,
  AssembleEngine, FileListDlg, SelectDlg, ShowText, ExecuteApp, EditHistListDlg,
  PoStatDlg, ListSelectDlg;

{ ---------------------------------------------------------------- }
const
// external programs
//  MsgMerge = 'msgmerge.exe';
  MsgFmt = 'msgfmt.exe';
  LangSubDir = 'Lang';
  LangName = 'languages';

  (* INI-Sektionen *)
  StatSekt = 'PoStat';

  (* INI-Variablen *)
  iniLangDir  = 'LanguageDir';
  iniLanguage = 'Language';
  iniLast     = 'LastDir';
  IniEditor   = 'Editor';

// old language indexes (before Vers. 3.2) to shortcut
  MaxLang = 29;
  LangShortNames : array [0..MaxLang-1] of string = (
    'bg','cs','da','de','el','en','es','et','fi','fr','hr','hu','it','lt','lv',
    'nl','no','pl','pt','ro','ru','sk','sl','sq','sr','sv','tr','uk','zh');
  defLanguages = '"bg - Bulgarian","cs - Czech","da - Danish","de - German","el - Greek","en - English",'+
      '"es - Spanish","et - Estonian","fi - Finnish","fr - French","hr - Croatian","hu - Hungarian",'+
      '"it - Italian","lt - Lithuanian","lv - Latvian","nl - Dutch","no - Norwegian","pl - Polish",'+
      '"pt - Portuguese","ro - Romanian","ru - Russian","sk - Slovak","sl - Slovenian",'+
      '"sq - Albanian","sr - Serbian","sv - Swedish","tr - Turkish","uk - Ukrainian","zh - Chinese"';

procedure TfrmTransMain.FormCreate(Sender: TObject);
var
  IniFile  : TMemIniFile;
  n        : integer;
  s,sl     : string;
begin
  TranslateComponent(self);
  Application.Title:=_('Process GnuGetText translations for Delphi');
  InitPaths(AppPath,UserPath,ProgPath);
  InitVersion(Application.Title,Vers,CopRgt,3,3,ProgVersName,ProgVersDate);
  Caption:=ProgVersName;
  // prepare language combobox
  s:=AddPath(PrgPath,LangSubdir);
  s:=AddPath(s,NewExt(LangName,SelectedLanguage));
  if not FileExists(s) then s:=NewExt(s,'en');
  with cbLanguage do begin
    if FileExists(s) then Items.LoadFromFile(s)
    else Items.CommaText:=defLanguages;
    TrimEndOfList(Items);
    end;
  IniName:=Erweiter(AppPath,PrgName,IniExt);
  IniFile:=TMemIniFile.Create(IniName);
  with IniFile do begin
    edLangSubdir.Text:=ReadString(CfgSekt,iniLangDir,'languages');
    s:=ReadString(CfgSekt,iniLanguage,'');
    with cbLanguage do begin
      Items.NameValueSeparator:='-';
      if not TryStrToInt(s,n) then n:=-1;
      if (n>=0) and (n<MaxLang) then sl:=LangShortNames[n]  // n is index for old language list
      else sl:=s;
      if length(sl)>0 then begin
        n:=GetIndexOfName(Items,sl);
        if n>=0 then ItemIndex:=n else ItemIndex:=0;
        end
      else ItemIndex:=0;
      end;
    LastDir:=ReadString(CfgSekt,IniLast,'');
    Top:=ReadInteger(CfgSekt,iniTop,Top);
    Left:=ReadInteger(CfgSekt,iniLeft,Left);
    ClientWidth:=ReadInteger (CfgSekt,IniWidth,ClientWidth);
    ClientHeight:=ReadInteger (CfgSekt,IniHeight,ClientHeight);
    TextEditor:=ReadString(CfgSekt,IniEditor,'');
    LoadHistory(IniFile,DirSekt,cbProjDir);
    LoadHistory(IniFile,FileSekt,cbFiles);
    Free;
    end;
  lbLang.Items.NameValueSeparator:='-';
  ErrorOutput:=TStringList.Create;
  XGetText:=nil;
  end;

{$IFDEF HDPI}   // scale glyphs and images for High DPI
procedure TfrmTransMain.AfterConstruction;
begin
  inherited;
  ScaleImageList(ilGlyphs,PixelsPerInchOnDesign,Monitor.PixelsPerInch);
  ScaleButtonGlyphs(self,PixelsPerInchOnDesign,Monitor.PixelsPerInch);
  end;
{$EndIf}

procedure TfrmTransMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  try HtmlHelp(0,nil,HH_CLOSE_ALL,0); except end;
  end;

procedure TfrmTransMain.FormDestroy(Sender: TObject);
var
  IniFile  : TMemIniFile;
begin
  IniFile:=TMemIniFile.Create(IniName);
  with IniFile do begin
    WriteString(CfgSekt,iniLangDir,edLangSubdir.Text);
//    WriteInteger(CfgSekt,iniLanguage,cbLanguage.ItemIndex);
    with cbLanguage do WriteString(CfgSekt,iniLanguage,GetName(Items,ItemIndex));
    WriteString(CfgSekt,IniLast,cbProjDir.Text);
    WriteInteger(CfgSekt,iniTop,Top);
    WriteInteger(CfgSekt,iniLeft,Left);
    WriteInteger (CfgSekt,IniWidth,ClientWidth);
    WriteInteger (CfgSekt,IniHeight,ClientHeight);
    WriteString(CfgSekt,IniEditor,TextEditor);
    SaveHistory(IniFile,DirSekt,true,cbProjDir);
    SaveHistory(IniFile,FileSekt,true,cbFiles);
    UpdateFile;
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
    if cbPoedit.Checked then ilGlyphs.GetBitmap(1,Glyph)
    else ilGlyphs.GetBitmap(0,Glyph);
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
  if not DirectoryExists(LastDir) then begin
    with cbProjDir do begin
      for i:=0 to Items.Count-1 do if DirectoryExists(Items[i]) then Break;
      if i<Items.Count then LastDir:=Items[i]
      else begin
        LastDir:='';
        if not ShellDirDialog.Execute(_('Select project directory'),true,true,false,UserPath,LastDir) then Close
        else AddToHistory(cbProjDir,LastDir);
        end;
      end;
    end
  else begin
    with cbProjDir do ItemIndex:=Items.IndexOf(LastDir);
    if cbProjDir.ItemIndex<0 then AddToHistory(cbProjDir,LastDir);
    LoadGetTextSettings(LastDir);
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
    Clear; Style:=csSimple;
    end;
  end;

procedure TfrmTransMain.pmiEditClick(Sender: TObject);
var
  n : integer;
begin
  EditHistList(CursorPos,'',_('Project directories'),cbProjDir,true,true,n);
  end;

procedure TfrmTransMain.btnInfoClick(Sender: TObject);
begin
  InfoDialog(BottomLeftPos(btExtract,0,10),ProgVersName+' - '+ProgVersDate+sLineBreak+
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
  iniSimLen = 'SimMeasure';
  iniSimilar = 'MergeSimilar';
  iniSelLang  = 'SelectedLanguages';
  iniPrjName  = 'ProjectName';
  iniLangCnt = 'LanguageCount';
  iniLang = 'Language';

  AutoComm = $2000;
  KeepHist = AutoComm shl 1;
  MergBack = KeepHist shl 1;
  ReqTrans = $1000;   // requires editing of translation
  LangMask = $FFF;

procedure TfrmTransMain.LoadGetTextSettings (const ADir : string);
var
  n,i,nl : integer;
  s,sl : string;
begin
  // dxgettext.ini einlesen
  with TMemIniFile.Create(IncludeTrailingPathDelimiter(ADir)+DxGetTextIni) do begin
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

    ProjectName.Text:=ReadString(trSect,iniPrjName,'');
    with edLangSubdir do Text:=ReadString(trSect,iniLangDir,Text);
    nl:=ReadInteger(trSect,iniLangCnt,0);
    lbLang.Clear;
    if nl=0 then begin // old setting
      s:=ReadString(trSect,iniSelLang,defLang);
      if length(s)=0 then s:=defLang;
      repeat
        sl:=ReadNxtStr(s,',','');
        if not TryStrToInt(sl,n) then n:=0;
        if (n>0) and (n<=MaxLang) then sl:=LangShortNames[n-1];  // n-1 is index for old language list
        if length(sl)>0 then begin
          n:=GetIndexOfName(cbLanguage.Items,sl);
          if n>0 then lbLang.AddItem(cbLanguage.Items[n],pointer(n));
          end;
        until length(s)=0;
      end
    else begin
      for i:=0 to nl-1 do begin
        s:=ReadString(trSect,iniLang+IntToStr(i),'');
        if length(s)>0 then begin
          sl:=ReadNxtStr(s,',','');
          if length(sl)>0 then begin
            n:=GetIndexOfName(cbLanguage.Items,sl);
            if n>0 then lbLang.AddItem(cbLanguage.Items[n],pointer(n+AutoComm*ReadNxtInt(s,',',4)));
            end;
          end;
        end;
      if lbLang.Count=0 then lbLang.AddItem(defLang,pointer(GetIndexOfName(cbLanguage.Items,defLang)));
      end;
    lbLang.Selected[0]:=true;
    ShowLangOptions;
    btPoEdit.Enabled:=true;
    btMerge.Caption:=_('Merge translation with template');
    cbPoedit.Checked:=ReadBool(trSect,iniPoedit,true);
    if ReadBool(trSect,iniMulti,false) then rbMulti.Checked:=true
    else rbSingle.Checked:=true;
    udSimLength.Position:=ReadInteger(trSect,iniSimLen,defSimMeasure);
    cbMergeSimilar.Checked:=ReadBool(trSect,iniSimilar,false);
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
  i,n : integer;
begin
  // dxgettext.ini schreiben
  with TMemIniFile.Create(IncludeTrailingPathDelimiter(cbProjDir.Text)+DxGetTextIni) do begin
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

    WriteString(trSect,iniPrjName,ProjectName.Text);
    WriteString(trSect,iniLangDir,edLangSubdir.Text);
    WriteInteger(trSect,iniLangCnt,lbLang.Count);
    with lbLang do for i:=0 to Count-1 do begin
      n:=integer(Items.Objects[i]);
      s:=GetName(cbLanguage.Items,n and LangMask)+','+IntToStr(n div AutoComm);
      WriteString(trSect,iniLang+IntToStr(i),s);
      end;
    DeleteKey(trSect,iniSelLang);     // old value
//    s:='';                // old
//    with lbLang do for i:=0 to Count-1 do begin
//      n:=integer(Items.Objects[i]) and LangMask;
//      s:=s+','+GetName(cbLanguage.Items,n);
//      end;
//    if length(s)>0 then Delete(s,1,1);
//    WriteString(trSect,iniSelLang,s);
    WriteBool(trSect,iniPoedit,cbPoedit.Checked);
    WriteBool(trSect,iniMulti,rbMulti.Checked);
    WriteInteger(trSect,iniSimLen,udSimLength.Position);
    WriteBool(trSect,iniSimilar,cbMergeSimilar.Checked);
    WriteString(trSect,iniCopyPath,edTargetDir.Text);
    UpdateFile;
    Free;
    end;
  end;

const
  ggmSect = 'ggmerge';

  iniTemplate = 'template';
  iniBackup = 'createbackup';
  iniAuto = 'MergeAutoComments';
  iniHist = 'ObsoleteTranslations';

procedure TfrmTransMain.LoadMergeSettings(const AFilename : string);
begin
  with TMemIniFile.Create(NewExt(AFilename,IniExt)) do begin
//    TemplPath:=ReadString(ggmSect,iniTemplate,NewExt(IncludeTrailingPathDelimiter(cbProjDir.Text)+GetTemplName,PoExt));
//    TemplPath:=ExtractRelativePath(ExtractFilePath(AFilename),TemplPath);
    cbBackup.Checked:=ReadBool(ggmSect,iniBackup,true);
    Free;
    end;
  end;

procedure TfrmTransMain.SaveMergeSettings(const AFilename : string; ac,hc,bu : boolean);
begin
  with TMemIniFile.Create(NewExt(AFilename,IniExt)) do begin
//    WriteString(ggmSect,iniTemplate,TemplPath);
    WriteBool(ggmSect,iniBackup,bu);
    WriteBool(ggmSect,iniAuto,ac);
    WriteBool(ggmSect,iniHist,hc);
    UpdateFile;
    Free;
    end;
  end;

procedure TfrmTransMain.sbSetAllBuClick(Sender: TObject);
var
  i : integer;
begin
  with lbLang do for i:=0 to Items.Count-1 do
    Items.Objects[i]:=pointer(integer(Items.Objects[i]) and LangMask or MergBack);
  ShowLangOptions;
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
  ShowLangOptions;
  end;

procedure TfrmTransMain.ShowLangOptions;
var
  n : integer;
begin
  with lbLang do begin
    laMergeLanguage.Caption:=Items[ItemIndex];
    laSaveLanguage.Caption:=laMergeLanguage.Caption;
    end;
  with lbLang do n:=integer(Items.Objects[ItemIndex]);
  cbMergeAutoComments.Checked:=n and AutoComm <>0;
  cbMergeHistory.Checked:=n and KeepHist <>0;
  cbBackup.Checked:=n and MergBack <>0;
  end;

procedure TfrmTransMain.MergeOptionClick(Sender: TObject);
var
  n : integer;
begin
  if cbMergeAutoComments.Checked then n:=AutoComm else n:=0;
  if cbMergeHistory.Checked then n:=n or KeepHist;
  if cbBackup.Checked then n:=n or MergBack;
  with lbLang do begin
    Items.Objects[ItemIndex]:=pointer(integer(Items.Objects[ItemIndex]) and LangMask or n);
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
    Selected[Items.AddObject(cbLanguage.Text,pointer(cbLanguage.ItemIndex))]:=true;
    end;
  end;

procedure TfrmTransMain.bbImportClick(Sender: TObject);
var
   sx,sl,s : string;
   ok : boolean;
   nl,i,n : integer;
begin
  sx:=ExtractParentPath(cbProjDir.Text); sl:=sx;
  repeat
    ok:=ShellDirDialog.Execute(_('Select project directory for the import'),
          false,true,false,sl,sx);
    if ok then begin
      s:=AddPath(sx,DxGetTextIni);
      ok:=FileExists(s);
      if not ok then ErrorDialog(Format(_('File not found: "%s"'),[s]));
      end
    else Break;
    until ok;
  if ok then with TMemIniFile.Create(s) do begin
    lbLang.Clear;
    nl:=ReadInteger(trSect,iniLangCnt,0);
    for i:=0 to nl-1 do begin
      s:=ReadString(trSect,iniLang+IntToStr(i),'');
      if length(s)>0 then begin
        sl:=ReadNxtStr(s,',','');
        if length(sl)>0 then begin
          n:=GetIndexOfName(cbLanguage.Items,sl);
          if n>0 then lbLang.AddItem(cbLanguage.Items[n],pointer(n+AutoComm*ReadNxtInt(s,',',4)));
          end;
        end;
      end;
    if lbLang.Count=0 then lbLang.AddItem(defLang,pointer(GetIndexOfName(cbLanguage.Items,defLang)));
    Free;
    lbLang.Selected[0]:=true;
    ShowLangOptions;
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
    AddToHistory(cbProjDir,s);
    LoadGetTextSettings(s);
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
  ss:=GetLangSubDir(lbLang.ItemIndex);
  ss:=IncludeTrailingPathDelimiter(cbProjDir.Text)+IncludeTrailingPathDelimiter(ss);
  PoFile:=NewExt(ss+GetTemplName,PoExt);
  SetLangMarker(lbLang.ItemIndex,PoStatDialog.Execute(PoFile));
  end;

procedure TfrmTransMain.btUpdateClick(Sender: TObject);
var
  i : integer;
  s : string;
begin
  with lbLang do begin
    for i:=0 to Count-1 do begin
      s:=IncludeTrailingPathDelimiter(cbProjDir.Text)+IncludeTrailingPathDelimiter(GetLangSubDir(i));
      s:=NewExt(s+GetTemplName,PoExt);
      SetLangMarker(i,CheckPoFile(s));
      end;
    Invalidate;
    end;
  end;

procedure TfrmTransMain.btnNewClick(Sender: TObject);   // new file list
var
  s : string;
begin
  s:=FileListDialog.Execute(_('Create new file list'),GetFileFilter,
       cbProjDir.Text,'','','',false);
  if length(s)>0 then begin
    AddToHistory(cbFiles,s);
    with cbFiles do begin
      if Canvas.TextWidth(Text)<=Width then Hint:=_('Insert filenames separated by commas')
      else Hint:=Text;
      end;
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
  if length(s)>0 then begin
    AddToHistory(cbFiles,s);
    with cbFiles do begin
      if Canvas.TextWidth(Text)<=Width then Hint:=_('Insert filenames separated by commas')
      else Hint:=Text;
      end;
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
    if cbPoedit.Checked then ilGlyphs.GetBitmap(1,Glyph)
    else ilGlyphs.GetBitmap(0,Glyph);
    end;
  end;

procedure TfrmTransMain.cbProjDirCloseUp(Sender: TObject);
var
  s : string;
begin
  SaveGetTextSettings;
  with cbProjDir do s:=Items[ItemIndex];
  AddToHistory(cbProjDir,s);
  pcOptions.ActivePageIndex:=0;
  LoadGetTextSettings(s);
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
var
  sl : TStringList;
  i  : integer;
begin
  sl:=TStringList.Create;
  sl.Text:=Msg;
  with meProgress do begin
    Lines.Add('* '+sl[0]);
    for i:=1 to sl.Count-1 do Lines.Add('  '+sl[i]);
    if LineNumber>0 then Lines.Add('  '+Format(_('Line: %u'),[Linenumber]));
    if length(Line)>0 then Lines.Add('  '+Format(_('Last line read: %s'),[Line]));
//    Lines.Add('');
    end;
  inc(WarnCount);
  sl.Free;
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
  si   : string;
  ok   : boolean;

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
  if assigned(XGetText) then begin
    XGetText.Cancel;
    end
  else begin
    SaveGetTextSettings;
    with btExtract do begin
      Glyph:=nil;
      ilGlyphs.GetBitmap(3,Glyph);
      end;
    btMerge.Enabled:=false;
    XGetText:=TXGetText.Create;
    meProgress.Clear;
    WarnCount:=0;
    with XGetText do begin
      ProjectId:=ProjectName.Text;
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
        ok:=Execute;
      finally
        Free;
        end;
      end;
    XGetText:=nil;
    with meProgress.Lines do if ok then begin
      if WarnCount>0 then Add('==> '+Format(_('%u warnings or errors'),[WarnCount]));
      Add('');
      laProgress.Caption:=_('Template was created');
      end
    else Add('Canceled by user');
    laLineNr.Caption:='';
    btMerge.Enabled:=true;
    with btExtract do begin
      Glyph:=nil;
      ilGlyphs.GetBitmap(2,Glyph);
      end;
    end;
  end;

procedure TfrmTransMain.btMergeClick(Sender: TObject);
var
  i    : integer;
  ok   : boolean;
begin
  laProgress.Caption:=_('The former translations are merged with the current template');
  Application.ProcessMessages;
  with lbLang do if SelCount=1 then begin
    if Merge(ItemIndex) then laProgress.Caption:=_('Merging with template was successful')
    else laProgress.Caption:=_('Merging with template failed');
    end
  else begin
    ok:=true;
    for i:=0 to Count-1 do if Selected[i] then begin
      ok:=Merge(i) and ok;
      if not ok then Break;
      end;
    if ok then laProgress.Caption:=Format(_('%u translations were successfully merged with template'),[Count])
    else laProgress.Caption:=_('Merging with template failed');
    end;
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
    n:=integer(Objects[AIndex]);
    if Mark then n:=n or ReqTrans else n:=n and not ReqTrans;
    Objects[AIndex]:=pointer(n);
    end;
  end;

// returns true if translation is incomplete
function TfrmTransMain.CheckPoFile (const PoFile : string) : boolean;
var
  pe : TPoEntry;
begin
  Result:=false;
  if FileExists(PoFile) then with TPoEntryList.Create do if LoadFromFile(PoFile)=0 then begin  // check for new entries
    pe:=FindFirst;
    while not Result and (pe<>nil) do begin
      with pe do if not (AnsiStartsStr(HistMarker,MsgId) or MsgId.IsEmpty) then begin // skip history entries
        if length(MsgStr)=0 then Result:=true   // new msgstr
        else if Fuzzy then Result:=true;
        end;
      pe:=FindNext(pe);
      end;
    Free;
    end;
  end;

function TfrmTransMain.Merge (AIndex : integer) : boolean;
var
  translist : TPoEntryList;
  pe,petr   : TPoEntry;
  parser    : TPoParser;
  PoHeader  : TPoHeader;
  tf        : TextFile;
  fs        : TFileStream;
  MergePath,PoFile,
  sv,sx,scd,
  st,s      : string;
  i,n       : integer;
  ac,hc,bu,
  cphd      : boolean;
begin
  st:=GetTemplName;
  MergePath:=IncludeTrailingPathDelimiter(cbProjDir.Text)+IncludeTrailingPathDelimiter(GetLangSubDir(AIndex));
  PoFile:=NewExt(MergePath+st,PoExt);
//  LoadMergeSettings(MergePath+st);
  with lbLang do begin
    n:=integer(Items.Objects[AIndex]);
    hc:=n and KeepHist <>0;
    ac:=n and AutoComm <>0;
    bu:=n and MergBack <>0;
    end;
  // Template
  sv:=NewExt(IncludeTrailingPathDelimiter(cbProjDir.Text)+st,PoExt);
  // Translation
  if not DirectoryExists(MergePath) then ForceDirectories(MergePath);
  cphd:=FileExists(PoFile);  // copy header after merging
  if not cphd then CopyFileTS(sv,PoFile); // Init translation
  sx:=NewExt(PoFile,PoxExt);
  meProgress.Lines.Add(Format(_('Merging with %s template'),[GetLangName(AIndex)]));
  Result:=true;
// Always use internal merge function
  FileMode:=fmOpenRead;
//  if rgEncoding.ItemIndex=0 then cp:=cpUtf8 else cp:=cpLatin1;
  translist:=TPoEntryList.Create;
  translist.SimMeasure:=udSimLength.Position;
  try
    i:=translist.LoadFromFile(PoFile);
    if i>0 then begin
      ErrorDialog(Format(_('Error in line %u of file "%s"'),[i,PoFile]));
      Result:=false;
      end;
    if Result then begin
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
                  if (Header[hiProjectId]=defId) and (length(ProjectName.Text)>0) then
                    Header[hiProjectId]:=ProjectName.Text;
                  Header[hiCreationDate]:=scd;
                  Header[hiRevisionDate]:=CurrentTimestamp;
                  Header[hiLanguage]:=GetLangId(AIndex);
                  Header[hiXGenerator]:=GetGenerator;
                  UpdateHeader(petr);
                  pe.Assign(petr);
                  end
                else begin  // normal entry
                  pe.MsgStr:=petr.MsgStr;
                  pe.Fuzzy:=petr.Fuzzy;
                  if not ac then begin
                    pe.AutoCommentList.Clear; pe.HistCommentList.Clear;
                    end;
                  pe.UserCommentList.Text:=petr.UserCommentList.Text;
                  end;
                petr.Merged:=true;
                end
              else if not pe.MsgId.IsEmpty then begin
                if cbMergeSimilar.Checked then begin
                  petr:=translist.FindSoundEntry(pe.MsgId); // check for case independent msgid
                  if petr<>nil then begin
                    pe.MsgStr:=petr.MsgStr;
                    pe.Fuzzy:=true;
                    if not ac then begin
                      pe.AutoCommentList.Clear; pe.HistCommentList.Clear;
                      end;
                    pe.UserCommentList.Text:=petr.UserCommentList.Text;
                    petr.Merged:=true;
                    end;
                  end;
                end;
              pe.WriteToStream(fs);
              end;
            if hc then begin
            // check for obsolete history comments
              petr:=translist.FindFirst;
              while (petr<>nil) do begin
                with petr do if (copy(MsgId,1,2)=HistMarker) then begin
                  if (HistCommentList.Count>0) then begin
                    s:=Trim(HistCommentList[0].Substring(3));
                    if AnsiStartsText('MSGID',s) then begin
                      delete(s,1,5);
                      s:=AnsiDequotedStr(Trim(s),'"');
                      if translist.IsEntry(s) then Merged:=true;
                      end;
                    end;
                  end;
                petr:=translist.FindNext(petr);
                end;
            // copy history comments
              petr:=translist.FindFirst;
              while (petr<>nil) do begin
                with petr do if not Merged then begin   // check for entries no longer used in template
                  if (copy(MsgId,1,2)=HistMarker) then WriteToStream(fs)
                  else begin
                    HistCommentList.Add('#~ msgid '+String2PO(MsgId));
                    HistCommentList.Add('#~ msgstr '+String2PO(MsgStr));
                    s:=MsgId;
                    MsgId:=HistMarker+MsgId;
                    WriteToStream(fs,false);
                    MsgId:=s;
                    end;
                  end;
                petr:=translist.FindNext(petr);
                end;
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
  if Result then begin
    sv:=NewExt(PoFile,'~'+PoExt);
    if FileExists(sv) then DeleteFile(sv);
    if bu then begin
      if FileExists(PoFile) then Result:=RenameFile(PoFile,sv)
      else Result:=true;
      if not Result then ErrorDialog(Format(_('Cannot rename %s to %s'),[PoFile,sv]));
      end
    else begin
      Result:=deletefile (PoFile);
      if not Result then ErrorDialog(Format(_('Cannot delete %s'),[PoFile]));
      end;
    if Result then begin
      Result:=FileExists(sx);
      if Result then begin
        Result:=RenameFile(sx,PoFile);
        if not Result then ErrorDialog(Format(_('Cannot rename %s to %s'),[sx,PoFile]))
        end
      end;
    end;
  if Result then begin
    SetLangMarker(AIndex,CheckPoFile(PoFile));
    lbLang.Invalidate;
    SaveMergeSettings(MergePath+st,ac,hc,bu);
    if cbPoEdit.Checked then begin // and (lbLang.SelCount=1) then
      StartAndWait(PoFile,'');
      SetLangMarker(AIndex,CheckPoFile(PoFile));
//      ShellExecute (Application.Handle,'open',PChar(PoFile),nil,nil,SW_SHOWNORMAL)
      end
//    else CompileToMo(PoFile,MergePath);
    end;
  meProgress.Lines.Add('');
  end;

{ ---------------------------------------------------------------- }
procedure TfrmTransMain.btCopyClick(Sender: TObject);
var
  s    : string;
  i,n,nf,ne : integer;
begin
  if length(edTargetDir.Text)=0 then btCopyDirClick(Sender);
  if DirectoryExists(edTargetDir.Text) then with lbLang do begin
    n:=0; nf:=0; ne:=0;
    s:=GetLangSubDir(ItemIndex);
    if SelCount=1 then begin
      CopyMo(ItemIndex,nf,ne);
      inc(n);
      end
    else for i:=0 to Count-1 do if Selected[i] then begin
      inc(n);
      laProgress.Caption:=Format(_('Copying mo file (%u)'),[n]);
      Application.ProcessMessages;
      CopyMo(i,nf,ne);
      Application.ProcessMessages;
      end;
    if n>0 then laProgress.Caption:=Format(_('%s processed (%s - %s)'),
      [GetPluralString('',_('1 file'),_('%u files'),n),
       GetPluralString(_('No copy'),_('1 copy'),_('%u copies'),nf),
       GetPluralString(_('No error'),_('1 error'),_('%u errors'),ne)])
    else laProgress.Caption:=_('No files processed');
    end
  else ErrorDialog(Format(_('Folder for executables not found: '),[edTargetDir.Text]));
  meProgress.Lines.Add('');
  SaveGetTextSettings;
  end;

procedure TfrmTransMain.CopyMo (AIndex : integer; var FCount,ECount : integer);
var
  MergePath,PoFile,
  sm,sd : string;
  ok    : boolean;

  function CopyToDirs (const Dir,SubFolder,Filename : string; var fc,ec : integer) : boolean;
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
          Result:=CopyToDirs(Erweiter(Dir,Name,''),SubFolder,Filename,fc,ec) and Result;
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
        inc(ec);
        Result:=false;
        end
      else inc(fc);
      end;
    end;

begin
  MergePath:=IncludeTrailingPathDelimiter(cbProjDir.Text)+IncludeTrailingPathDelimiter(GetLangSubDir(AIndex));
  sm:=MergePath+GetTemplName;
  PoFile:=NewExt(sm,PoExt);
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
          inc(FCount);
          ok:=true;
        except
          on E:EInOutError do begin
            meProgress.Lines.Add(_('  *** Error copying file:'));
            meProgress.Lines.Add('  '+E.Message);
            inc(ECount);
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
      ok:=CopyToDirs(edTargetDir.Text,sd,sm,FCount,ECount);
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
  ss:=IncludeTrailingPathDelimiter(cbProjDir.Text)+IncludeTrailingPathDelimiter(GetLangSubDir(lbLang.ItemIndex));
  PoFile:=NewExt(ss+GetTemplName,PoExt);
  StartAndWait(PoFile,'');
  SetLangMarker(lbLang.ItemIndex,CheckPoFile(PoFile));
  lbLang.Invalidate;
//  ShellExecute (0,'open',PChar(PoFile),nil,nil,SW_SHOWNORMAL);
  end;

procedure TfrmTransMain.btEditClick(Sender: TObject);
var
  PoFile,ss : string;
begin
  ss:=IncludeTrailingPathDelimiter(cbProjDir.Text)+IncludeTrailingPathDelimiter(GetLangSubDir(lbLang.ItemIndex));
  PoFile:=NewExt(ss+GetTemplName,PoExt);
  StartAndWait(TextEditor,PoFile);
  SetLangMarker(lbLang.ItemIndex,CheckPoFile(PoFile));
  lbLang.Invalidate;
//  ShellExecute (0,'open',PChar(TextEditor),
//    PChar(NewExt(IncludeTrailingPathDelimiter(cbProjDir.Text)+GetTemplName,PoExt)),nil,SW_SHOWNORMAL);
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
