(* File List
   Erstellen von Dateilisten
   - Auswahl, ein Verzeichnis oder Verzeichnisbaum
   - Formateinstellungen
   - sichern Datei oder Zwischenablage
   J. Rathlev, 24222 Schwentinental

     Vers. 2.2  - schnellere Anzeige
     Vers. 2.3  - mit Shell-Komponenten
     Vers. 2.4  - einstellbare Anzahl der Verzeichnisebenen
     Vers. 2.5  - Startverzeichnis aus Befehlszeile
     Vers. 2.6  - alphabet. Sortierung der Dateien
                  keine Anzeige von Zip-Dateien im Verzeichisbaum (ab XP)
     Vers. 3.0 (Okt. 2021) - Anpassung an Delphi 10
*)

unit FListMain;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ClipBrd, FileCtrl,
  StdCtrls, Buttons, IniFiles, Spin, ComCtrls,
  ExtCtrls, ShellCtrls, Menus, laz.VirtualTrees, DRuler, LazLangUtils;

const
  Prog = 'FileList ';
  Vers = '(Vers. 3.0)';
  CopRgt = '© 2026 - Dr. J.  Rathlev, D-24222 Schwentinental';
  EMailAdr = 'kontakt(a)rathlev-home.de';
  AppSubDir = 'Sample';    // Unterverz. in Anwendungsdaten für Ini-Dateien

resourcestring
  rsTitle = 'Creating file lists';

type
  TFileInfo = class (TObject)
    DirCount,
    DirLevel    : integer;
    DirName     : string;
    FileInfo    : TSearchRec;
    constructor Create (Count,Level : integer;
                        Dir         : string;
                        Info        : TSearchRec);
    end;

  { THauptForm }

  THauptForm = class(TForm)
    lbPreview: TListBox;
    pmiLanguage: TMenuItem;
    pmiAbout: TMenuItem;
    paTop: TPanel;
    laDirectory: TLabel;
    Label1: TLabel;
    Label3: TLabel;
    FilterComboBox: TFilterComboBox;
    FileListBox: TFileListBox;
    CopyBtn: TBitBtn;
    FilterBtn: TBitBtn;
    pmSettings: TPopupMenu;
    QuitBtn: TBitBtn;
    gbDisplay: TGroupBox;
    cbHidden: TCheckBox;
    cbSystem: TCheckBox;
    PageRuler: TRuler;
    SubDirCB: TCheckBox;
    cbShowDir: TCheckBox;
    cbIndent: TCheckBox;
    gbFilelIst: TGroupBox;
    cbFileSize: TCheckBox;
    cbFileAttr: TCheckBox;
    cbFileDate: TCheckBox;
    cbFilename: TCheckBox;
    seFilename: TSpinEdit;
    seFileSize: TSpinEdit;
    seFileDate: TSpinEdit;
    seFileAttr: TSpinEdit;
    StoreBtn: TBitBtn;
    ShowBtn: TBitBtn;
    stStatus: TStaticText;
    SaveDialog: TSaveDialog;
    ShellTreeView: TShellTreeView;
    NumLevels: TSpinEdit;
    Label4: TLabel;
    btnSettings: TBitBtn;
    rgSize: TRadioGroup;
    paRuler: TPanel;
    procedure btnSettingsClick(Sender: TObject);
    procedure QuitBtnClick(Sender: TObject);
    procedure FilterBtnClick(Sender: TObject);
    procedure CopyBtnClick(Sender: TObject);
    procedure InfoBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cbHiddenClick(Sender: TObject);
    procedure cbSystemClick(Sender: TObject);
    procedure DirectoryListBoxChange(Sender: TObject);
    procedure StoreBtnClick(Sender: TObject);
    procedure ShowBtnClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure ChangeFormatClick(Sender: TObject);
    procedure FilterComboBoxChange(Sender: TObject);
    procedure cbShowDirClick(Sender: TObject);
    procedure SubDirCBClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ShellTreeViewClick(Sender: TObject);
  private
    { Private-Deklarationen }
    AppPath,
    IniName,
    SaveName,
    LastDir,
    OldMask  : string;
    SubDir   : boolean;
    DirCount : integer;
    Languages : TLanguageList;
    FileList : TStringList;
    function ErzeugeZeile (FInfo : TSearchRec) : string;
    procedure AddFile(Dir,FName : string; var FileList   : TStringList);
    procedure Search (const Dir,Mask : string;
                      SubDir,OnlyDir : boolean;
                      Level,MaxLevel : integer;
                      var Count      : integer;
                      var FileList   : TStringList);
    procedure SetLanguageClick(Sender : TObject; const Language : TLangCodeString);
    procedure BuildFileList;
    procedure ShowFileList;
  public
    { Public-Deklarationen }
  end;

var
  HauptForm: THauptForm; 
  
implementation

uses ShlObj, LazGetText, FileFilterdlg, PathUtils, StringUtils,
  ListUtils, NumberUtils, WinFolders, MsgDialogs;

{$R *.lfm}

{ ---------------------------------------------------------------- }
constructor TFileInfo.Create (Count,Level : integer;
                              Dir         : string;
                              Info        : TSearchRec);
begin
  inherited Create;
  DirCount:=Count; DirLevel:=Level;
  DirName:=Dir; FileInfo:=Info;
  end;

{ ---------------------------------------------------------------- }
const
  IniExt = 'ini';

  (* INI-Sektionen *)
  CfgSekt  = 'Config';

  (* INI-Variablen *)
  IniDir     = 'Directory';
  IniFName   = 'Filename';
  IniFilter  = 'Filter';
  IniFiltNdx = 'FilterIndex';
  IniHidden  = 'Hidden';
  IniSystem  = 'System';
  IniSubDir  = 'Subdirectories';
  IniDirs    = 'OnlyDirectories';
  IniLevels  = 'SubDirLevels';
  IniIndent  = 'IndentDirectories';
  IniSize    = 'SizeMode';
  IniNameCol = 'NameCol';
  IniSizeCol = 'SizeCol';
  IniTimeCol = 'TimeStampCol';
  IniAttrCol = 'AttributesCol';

procedure THauptForm.FormCreate(Sender: TObject);
var
  IniFile  : TMemIniFile;
  ok       : boolean;
  n        : integer;
begin
  TranslateComponent(self);
  FileListBox.FileType:=[ftNormal];
  AppPath:=GetDesktopFolder(CSIDL_APPDATA);
  if length(AppPath)>0 then begin
    AppPath:=SetDirName(AppPath)+AppSubDir;  // Pfad zu Anwendungsdaten
    ok:=ForceDirectories(AppPath);
    end
  else ok:=false;
  if not ok then AppPath:=GetDesktopFolder(CSIDL_PERSONAL);
  Caption:=rsTitle;
  IniName:=Erweiter(AppPath,PrgName,IniExt);
  IniFile:=TMemIniFile.Create(IniName);
  with IniFile do begin
    LastDir:=ReadString(CfgSekt,IniDir,'');
    if pos('\\',LastDir)=1 then LastDir:='';   // Netzwerkpfad entfernen
    SaveName:=ReadString(CfgSekt,IniFName,'');
    with FilterComboBox do begin
      Filter:=ReadString(CfgSekt,IniFilter,_('all')+' (*.*)|*.*');
      n:=ReadInteger(CfgSekt,IniFiltNdx,0);
      if (n<0) or (n>=Items.Count) then n:=0;
      ItemIndex:=n;
      FileListBox.Mask:=Mask;
      end;
    cbHidden.Checked:=ReadBool(CfgSekt,IniHidden,false);
    cbSystem.Checked:=ReadBool(CfgSekt,IniSystem,false);
    SubDir:=ReadBool(CfgSekt,IniSubDir,true);
    SubDirCB.Checked:=SubDir;
    NumLevels.Value:=ReadInteger(CfgSekt,IniLevels,-1);
    cbShowDir.Checked:=ReadBool(CfgSekt,IniDirs,false);
    cbIndent.Checked:=ReadBool(CfgSekt,IniIndent,true);
    rgSize.ItemIndex:=ReadInteger(CfgSekt,IniSize,1);
    n:=ReadInteger(CfgSekt,IniNameCol,-20);
    seFilename.Value:=abs(n);
    cbFilename.Checked:=n<0;
    n:=ReadInteger(CfgSekt,IniSizeCol,10);
    seFileSize.Value:=abs(n);
    cbFileSize.Checked:=n<0;
    n:=ReadInteger(CfgSekt,IniTimeCol,16);
    seFileDate.Value:=abs(n);
    cbFileDate.Checked:=n<0;
    n:=ReadInteger(CfgSekt,IniAttrCol,4);
    seFileAttr.Value:=abs(n);
    cbFileAttr.Checked:=n<0;
    Free;
    end;
  if ParamCount>0 then begin
    if DirectoryExists(ParamStr(1)) then LastDir:=ParamStr(1);
    end;
  Languages:=TLanguageList.Create(PrgPath);
  with Languages do begin
    Menu:=pmiLanguage;
    LoadLanguageNames(SelectedLanguage);
    OnLanguageItemClick:=SetLanguageClick;
    end;
  NumLevels.Enabled:=SubDir;
  cbHiddenClick(nil);
  cbSystemClick(nil);
  FileList:=TStringList.Create;
  FileList.Sorted:=true;
  OldMask:='';
  end;

procedure THauptForm.FormDestroy(Sender: TObject);
var
  IniFile  : TMemIniFile;
  n        : integer;
begin
  IniFile:=TMemIniFile.Create(IniName);
  with IniFile do begin
    WriteString(CfgSekt,IniDir,ShellTreeView.Path);
    WriteString(CfgSekt,IniFName,SaveName);
    with FilterComboBox do begin
      WriteString(CfgSekt,IniFilter,Filter);
      WriteInteger(CfgSekt,IniFiltNdx,ItemIndex);
      end;
    WriteBool(CfgSekt,IniHidden,cbHidden.Checked);
    WriteBool(CfgSekt,IniSystem,cbSystem.Checked);
    WriteBool(CfgSekt,IniSubDir,SubDirCB.Checked);
    WriteInteger(CfgSekt,IniLevels,NumLevels.Value);
    WriteBool(CfgSekt,IniDirs,cbShowDir.Checked);
    WriteBool(CfgSekt,IniIndent,cbIndent.Checked);
    WriteInteger(CfgSekt,IniSize,rgSize.ItemIndex);
    n:=seFilename.Value;
    if cbFilename.Checked then n:=-n;
    WriteInteger(CfgSekt,IniNameCol,n);
    n:=seFileSize.Value;
    if cbFileSize.Checked then n:=-n;
    WriteInteger(CfgSekt,IniSizeCol,n);
    n:=seFileDate.Value;
    if cbFileDate.Checked then n:=-n;
    WriteInteger(CfgSekt,IniTimeCol,n);
    n:=seFileAttr.Value;
    if cbFileAttr.Checked then n:=-n;
    WriteInteger(CfgSekt,IniAttrCol,n);
    UpdateFile;
    Free;
    end;
  Languages.Free;
  end;

procedure THauptForm.FormShow(Sender: TObject);
var
  TM: TTextMetric;
begin
  with lbPreview do begin
    Canvas.Font:=Font;
    GetTextMetrics(lbPreview.Canvas.Handle,TM);
    PageRuler.PixelsPerUserUnit:=TM.tmAveCharWidth;
    end;
  with ShellTreeView do begin
    try
      Path:=LastDir;
    except
      Root:='';
      end;
    if assigned(Selected) then Selected.MakeVisible;
    end;
  ShellTreeViewClick(Sender);
  end;

procedure THauptForm.FormActivate(Sender: TObject);
begin
  if not SubDir then ShowBtnClick(Sender);
  end;

procedure THauptForm.SetLanguageClick(Sender : TObject; const Language : TLangCodeString);
var
  sl : TLangCodeString;
  se : string;
  n  : integer;
begin
  if not AnsiSameStr(SelectedLanguage,Language) then begin
    sl:=ChangeLanguage(Language);
    Languages.LoadLanguageNames(sl);
    Caption:=rsTitle;
    stStatus.Caption:=Format(_('%u directories were scanned'+SLineBreak+
      '%u matching files found'),[DirCount,FileList.Count]);
    end;
  end;

procedure THauptForm.QuitBtnClick(Sender: TObject);
begin
  Close;
  end;

procedure THauptForm.btnSettingsClick(Sender: TObject);

  function BottomLeftPos (AControl : TControl) : TPoint;
  begin
    with AControl do if assigned(Parent) then Result:=Parent.ClientToScreen(Point(Left,Top+Height))
    else Result:=Point(Left,Top+Height);
    //Result.Offset(Offset);
    end;

begin
  with BottomLeftPos(btnSettings) do pmSettings.PopUp(x,y);
  end;

{ ---------------------------------------------------------------- }
procedure THauptForm.FilterBtnClick(Sender: TObject);
var
  s : string;
begin
  s:=FilterComboBox.Filter;
  FileFilterDialog.Execute (_('File filter'),s);
  FilterComboBox.Filter:=s;
  end;

{ ---------------------------------------------------------------- }
function THauptForm.ErzeugeZeile (FInfo : TSearchRec) : string;
var
  s,t : string;
begin
  s:='  ';
  with FInfo do begin
    if cbFilename.Checked then s:=s+ExtSp(Name+' ',seFilename.Value-1);
    if cbFileSize.Checked then begin
      with rgSize do if ItemIndex=0 then t:=StrInt(Size,-1) else t:=SizeToStr(Size,ItemIndex=2);
      s:=s+AddSp(t+' ',seFileSize.Value-1);
      end;
    if cbFileDate.Checked then begin
      s:=s+AddSp(DateTimeToStr(TimeStamp)+' ',seFileDate.Value-1);
      end;
    if cbFileAttr.Checked then begin
      t:='';
      if Attr and faReadOnly <>0 then t:=t+'r' else t:=t+'-';
      if Attr and faArchive <>0 then t:=t+'a' else t:=t+'-';
      if Attr and faHidden <>0 then t:=t+'h' else t:=t+'-';
      if Attr and faSysFile <>0 then t:=t+'s' else t:=t+'-';
      s:=s+AddSp(t,seFileAttr.Value);
      end;
    end;
  Result:=s;
  end;

{ ------------------------------------------------------------------- }
procedure THauptForm.AddFile(Dir,FName      : string;
                             var FileList   : TStringList);
var
  FInfo : TSearchRec;
  Info  : TFileInfo;
  s     : string;
begin
  s:=SetDirName(Dir);
  if FindFirst(SetDirName(Dir)+FName,$27,FInfo) = 0 then begin
    Info:=TFileInfo.Create (0,1,Dir,FInfo);
    FileList.AddObject(FName,Info);
    end
  end;

{ ------------------------------------------------------------------- }
procedure THauptForm.Search (const Dir,Mask : string;
                             SubDir,OnlyDir : boolean;
                             Level,MaxLevel : integer;
                             var Count      : integer;
                             var FileList   : TStringList);
var
  FInfo      : TSearchRec;
  Info       : TFileInfo;
  Attr,
  Findresult : integer;
  s,sm,sf    : string;
begin
  Application.ProcessMessages;
  s:=SetDirName(Dir); inc(Level); inc(Count); inc(DirCount);
  if not OnlyDir then begin // auch Dateien anzeigen
    Attr:=0; //faArchive;
    if cbHidden.Checked then Attr:=Attr or faHidden;
    if cbSystem.Checked then Attr:=Attr or faSysFile;
    sm:=Mask;
    repeat
      sf:=ReadNxtStr(sm,';');
      FindResult:=FindFirst (s+sf,Attr,FInfo);
      while FindResult=0 do begin
        Info:=TFileInfo.Create (Count,Level,Dir,FInfo);
        FileList.AddObject(Dir+' '+FInfo.Name,Info);
        FindResult:=FindNext (FInfo);
        end;
      FindClose (FInfo);
      until length(sm)=0
    end
  else begin
    Info:=TFileInfo.Create (Count,Level,Dir,FInfo);
    FileList.AddObject(Dir,Info);
    end;
  if SubDir and ((Level<=MaxLevel) or (MaxLevel=-1)) then begin
    Attr:=faDirectory;
    if cbHidden.Checked then Attr:=Attr or faHidden;
    if cbSystem.Checked then Attr:=Attr or faSysFile;
    FindResult:=FindFirst (Erweiter(s,'*','*'),Attr,FInfo);
    while (FindResult=0) do with FInfo do begin
      if NotSpecialDir(Name) and ((Attr and faDirectory)<>0) then
        Search(Erweiter(Dir,FInfo.Name,''),Mask,SubDir,OnlyDir,Level,MaxLevel,Count,FileList);
      FindResult:=FindNext (FInfo);
      end;
    FindClose(FInfo);
    end;
  end;

{ ------------------------------------------------------------------- }
procedure THauptForm.BuildFileList;
var
  i,n  : integer;
begin
  Screen.Cursor:=crHourglass;
  FreeListObjects(FileList);
  FileList.Clear; DirCount:=0;
  stStatus.Caption:='';
  with FileListBox do if (SelCount=0) then begin
    if SubDir then begin
      n:=0;
      Search (Directory,Mask,SubDir,cbShowDir.Checked,0,NumLevels.Value,n,FileList);
      OldMask:=Mask;
      end
    else begin
      for i:=0 to Items.Count-1 do AddFile(Directory,Items[i],FileList);
      DirCount:=1;
      end;
    end
  else begin
    DirCount:=1;
    for i:=0 to Items.Count-1 do if Selected[i] then
      AddFile(Directory,Items[i],FileList);
    end;
  stStatus.Caption:=Format(_('%u directories were scanned'+SLineBreak+
    '%u matching files found'),[DirCount,FileList.Count]);
  end;

{ ------------------------------------------------------------------- }
procedure THauptForm.ShowFileList;
var
  i,n  : integer;
  s    : string;
begin
  with lbPreview do begin
    Clear;
    Items.BeginUpdate;
    //Items.Add('         10        20        30        40        50        60        70        80');
    n:=0;
    with FileList do for i:=0 to Count-1 do with (Objects[i] as TFileInfo) do begin
      if n<>DirCount then begin
        if cbIndent.Checked then s:=FillSpace(DirLevel) else s:='';
        if not cbShowDir.Checked and (n>0) then Items.Add('');
        Items.Add(s+'==> '+DirName);
        n:=DirCount;
        end;
      if not cbShowDir.Checked then Items.Add(s+ErzeugeZeile(FileInfo));
      end;
    Items.EndUpdate;
    end;
  Screen.Cursor:=crDefault;
  end;

{ ------------------------------------------------------------------- }
procedure THauptForm.ShowBtnClick(Sender: TObject);
begin
  if Visible then begin
    BuildFileList; ShowFileList;
    end;
  end;

procedure THauptForm.FilterComboBoxChange(Sender: TObject);
begin
  if (FilterComboBox.Mask<>OldMask) then begin
    FileListBox.Mask:=FilterComboBox.Mask;
    ShowBtnClick(Sender);
    end;
  end;

procedure THauptForm.ChangeFormatClick(Sender: TObject);
begin
  if Active then ShowFileList;
  end;

procedure THauptForm.DirectoryListBoxChange(Sender: TObject);
begin
  if not SubDir then ShowBtnClick(Sender);
  end;

procedure THauptForm.InfoBtnClick(Sender: TObject);
begin
  InfoDialog(Prog+Vers+sLineBreak+CopRgt+sLineBreak+'E-Mail: '+EMailAdr);
  end;

procedure THauptForm.cbHiddenClick(Sender: TObject);
begin
  with FileListBox do if cbHidden.Checked then FileType:=FileType+[ftHidden]
  else FileType:=FileType-[ftHidden];
  ShowBtnClick(Sender);
  end;

procedure THauptForm.cbSystemClick(Sender: TObject);
begin
  with FileListBox do if cbSystem.Checked then FileType:=FileType+[ftSystem]
  else FileType:=FileType-[ftSystem];
  ShowBtnClick(Sender);
  end;

procedure THauptForm.SubDirCBClick(Sender: TObject);
begin
  SubDir:=SubDirCB.Checked;
  NumLevels.Enabled:=SubDir;
  if not SubDir and cbShowDir.Checked then cbShowDir.Checked:=false
  else ShowBtnClick(Sender);
  end;

procedure THauptForm.cbShowDirClick(Sender: TObject);
begin
  if cbShowDir.Checked then SubDir:=true
  else SubDir:=SubDirCB.Checked;
  ShowBtnClick(Sender);
  end;

procedure THauptForm.CopyBtnClick(Sender: TObject);
var
  TStr : TStringStream;
begin
  TStr:=TStringStream.Create('');
  lbPreview.Items.SaveToStream(TStr);
  Clipboard.Astext:=TStr.DataString;
  TStr.Free;
  end;

procedure THauptForm.StoreBtnClick(Sender: TObject);
begin
  with SaveDialog do begin
    if length(Savename)=0 then InitialDir:=GetDesktopFolder(CSIDL_PERSONAL)
    else InitialDir:=ExtractFilePath(SaveName);
    Filename:=ExtractFilename(Savename);
    if Execute then begin
      lbPreview.Items.SaveToFile(Filename);
      Savename:=Filename;
      end;
    end;
  end;

procedure THauptForm.ShellTreeViewClick(Sender: TObject);
begin
  laDirectory.Caption:=ShellTreeView.Path;
  with ShellTreeView do if (Pos (':\',Path)>0) or (Pos('\\',Path)>0) then
    FileListBox.Directory:=Path;
  ShowBtnClick(Sender)
//  DirectoryListBoxChange(Sender);
  end;

end.
