(* Delphi Dialog
   Dateiauswahldialog mit ShellListView
   =======================================
   Design ähnlich wie Datei-Dialog unter Windows 2000/XP
   aber mit automatischer Vorauswahl einer Datei

   © 2009-2024 J. Rathlev 24222 Schwentinental
      Web:  www.rathlev-home.de
      Mail: kontakt(a)rathlev-home.de

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.


   last modified: April 2024
   *)

unit ShellFileDlg;

interface

uses Winapi.Windows, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Forms,
  Vcl.Controls, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, ShellCtrls, Vcl.Menus,
  Vcl.FileCtrl, Vcl.Dialogs, Vcl.Buttons;

type
  TShellFileDialog = class(TForm)
    PopupMenu: TPopupMenu;
    itmCreate: TMenuItem;
    itmDelete: TMenuItem;
    N1: TMenuItem;
    itmUpdate: TMenuItem;
    cancel1: TMenuItem;
    ShellListView: TShellListView;
    pnlLeft: TPanel;
    spbNetwork: TSpeedButton;
    spbComputer: TSpeedButton;
    spbDesktop: TSpeedButton;
    spbMyFiles: TSpeedButton;
    Label1: TLabel;
    btbCancel: TBitBtn;
    btbOK: TBitBtn;
    cbSelectedFile: TComboBox;
    pnlTop: TPanel;
    ShellComboBox: TShellComboBox;
    spbHome: TSpeedButton;
    spbUp: TSpeedButton;
    spbNew: TSpeedButton;
    Label2: TLabel;
    pnlBottom: TPanel;
    Label3: TLabel;
    cbFilter: TFilterComboBox;
    spbLast: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ShellListViewAddFolder(Sender: TObject;
      AFolder: TShellFolder; var CanAdd: Boolean);
    procedure spbDesktopClick(Sender: TObject);
    procedure spbMyFilesClick(Sender: TObject);
    procedure spbComputerClick(Sender: TObject);
    procedure spbNetworkClick(Sender: TObject);
    procedure spbHomeClick(Sender: TObject);
    procedure spbUpClick(Sender: TObject);
    procedure spbNewClick(Sender: TObject);
    procedure cbFilterChange(Sender: TObject);
    procedure ShellComboBoxChange(Sender: TObject);
    procedure ShellListViewClick(Sender: TObject);
    procedure btbOKClick(Sender: TObject);
    procedure ShellListViewDblClick(Sender: TObject);
    procedure itmDeleteClick(Sender: TObject);
    procedure spbLastClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    FIniName,FIniSection,
    FDefaultDir,FLastPath  : string;
    FLastPaths             : TStringList;
    FOptions               : TOpenOptions;
    GoingBack              : boolean;
{$IFDEF HDPI}   // scale glyphs and images for High DPI
    procedure AfterConstruction; override;
{$EndIf}
    procedure NewColWidths (n1,n2,n3,n4 : integer);
    procedure AddHistory(ADir : string);
    procedure DeleteHistory (ADir : string);
    procedure AddToPathHistory;
  public
    { Public declarations }
    procedure LoadFromIni(IniName, Section : string);
    procedure ResetPosition;
    function Execute (const ATitle,
                      TypeFilter,
                      InitialDir,
                      DefaultDir    : string;
                      Options       : TOpenOptions;
                      var Filename  : string) : boolean;
  end;

var
  ShellFileDialog: TShellFileDialog;

implementation

{$R *.dfm}

uses System.IniFiles, System.StrUtils, Winapi.ShlObj, Winapi.Shellapi,
  Winapi.ActiveX, WinShell, FileUtils, IniFileUtils, System.Masks,
  WinUtils, GnuGetText, MsgDialogs;

const
  FMaxLen = 15;

procedure TShellFileDialog.FormCreate(Sender: TObject);
begin
  TranslateComponent (self,'dialogs');
  FIniName:=''; FIniSection:='';
  FDefaultDir:=''; FOptions:=[];
  pnlLeft.ParentBackground:=false;
  NewColWidths(30,12,15,18);
  FLastPaths:=TStringList.Create;
  FLastPath:=''; GoingBack:=false;
  end;

{$IFDEF HDPI}   // scale glyphs and images for High DPI
procedure TShellFileDialog.AfterConstruction;
begin
  inherited;
  if Application.Tag=0 then
    ScaleButtonGlyphs(self,PixelsPerInchOnDesign,Monitor.PixelsPerInch);
  end;
{$EndIf}

// set column widths in number of characters
procedure TShellFileDialog.NewColWidths (n1,n2,n3,n4 : integer);
begin
  ShellListView.SetColWidths([MulDiv(n1,Screen.PixelsPerInch,PixelsPerInchOnDesign),
                        MulDiv(n2,Screen.PixelsPerInch,PixelsPerInchOnDesign),
                        MulDiv(n3,Screen.PixelsPerInch,PixelsPerInchOnDesign),
                        MulDiv(n4,Screen.PixelsPerInch,PixelsPerInchOnDesign)]);
  end;

const
  iniHistory = 'History';
  iniTop = 'Top';
  iniLeft = 'Left';
  IniWidth = 'Width';
  IniHeight = 'Height';

(* load history list *)
procedure TShellFileDialog.LoadFromIni(IniName,Section : string);
var
  i       : integer;
  IniFile : TMemIniFile;
  s       : string;
begin
  FIniName:=IniName; FIniSection:=Section;
  cbSelectedFile.Items.Clear;
  if (length(FIniName)>0) and (length(FIniSection)>0) then begin
    IniFile:=TMemIniFile.Create(IniName);
    for i:=0 to FMaxLen-1 do begin
      s:=IniFile.ReadString(FIniSection,iniHistory+IntToStr(i),'');
      if s<>'' then cbSelectedFile.AddItem(s,nil);
      end;
    with IniFile do begin
      Top:=ReadInteger(FIniSection,iniTop,Top);
      Left:=ReadInteger(FIniSection,iniLeft,Left);
      ClientWidth:=ReadInteger (FIniSection,IniWidth,ClientWidth);
      ClientHeight:=ReadInteger (FIniSection,IniHeight,ClientHeight);
      Free;
      end;
    end;
  end;

procedure TShellFileDialog.ResetPosition;
begin
  Top:=50; Left:=50;
  end;

(* save history list *)
procedure TShellFileDialog.FormDestroy(Sender: TObject);
var
  i       : integer;
begin
  if (length(FIniName)>0) and (length(FIniSection)>0) then begin
    with TMemIniFile.Create(FIniName) do begin
      try
        EraseSection(FIniSection);
        with cbSelectedFile.Items do for i:=0 to Count-1 do
          WriteString(FIniSection,iniHistory+IntToStr(i),Strings[i]);
        WriteInteger(FIniSection,iniTop,Top);
        WriteInteger(FIniSection,iniLeft,Left);
        WriteInteger(FIniSection,IniWidth,ClientWidth);
        WriteInteger(FIniSection,IniHeight,ClientHeight);
        UpdateFile;
      finally
        Free;
        end;
      end;
    end;
  FLastPaths.Free;
  end;

procedure TShellFileDialog.FormShow(Sender: TObject);
begin
  FitToScreen(Screen,self);
  with ShellListView do if assigned(Selected) then Selected.MakeVisible(false);
  end;

{ ------------------------------------------------------------------- }
(* add directory to history list *)
procedure TShellFileDialog.AddHistory(ADir : string);
begin
  with cbSelectedFile.Items do begin
    if IndexOf(ADir)<0 then Add (ADir);
    end;
  end;

(* delete directory from history list *)
procedure TShellFileDialog.DeleteHistory(ADir : string);
var
  i : integer;
begin
  with cbSelectedFile,Items do begin
    i:=IndexOf(ADir);
    if i>=0 then Delete (i);
    ItemIndex:=0;
    end;
  end;

{ ------------------------------------------------------------------- }
procedure TShellFileDialog.ShellListViewAddFolder(Sender: TObject;
  AFolder: TShellFolder; var CanAdd: Boolean);
var
  s : string;
begin
  s:=AFolder.PathName;
  CanAdd:=AFolder.IsFolder or MatchesMask(ExtractFilename(s),cbFilter.Mask);
  end;

procedure TShellFileDialog.spbDesktopClick(Sender: TObject);
begin
  with ShellComboBox do begin
    Root:='rfDesktop';
    ItemIndex:=0;
    end;
  AddToPathHistory;
  end;

procedure TShellFileDialog.spbMyFilesClick(Sender: TObject);
begin
  with ShellComboBox do begin
    Root:='rfPersonal';
    ItemIndex:=0;
    end;
  AddToPathHistory;
  end;

procedure TShellFileDialog.spbComputerClick(Sender: TObject);
begin
  with ShellComboBox do begin
    Root:='rfMyComputer';
    ItemIndex:=0;
    end;
  AddToPathHistory;
  end;

procedure TShellFileDialog.spbNetworkClick(Sender: TObject);
begin
  with ShellComboBox do begin
    Root:='rfNetwork';
    ItemIndex:=0;
    end;
  AddToPathHistory;
  end;

procedure TShellFileDialog.spbHomeClick(Sender: TObject);
begin
  if not DirectoryExists(FDefaultDir) then begin
    ErrorDialog('',Format(dgettext('dialogs','Directory not found:'+sLineBreak+'%s!'),[FDefaultDir]));
    DeleteHistory(FDefaultDir);
    end
  else with ShellComboBox do begin
    if (Root='rfNetwork') or ((length(FDefaultDir)=3) and (copy(FDefaultDir,2,2)=':\')) then Root:=FDefaultDir
    else begin
      Root:='rfMyComputer';
      try Path:=FDefaultDir; except end;
      end;
    end;
  end;

procedure TShellFileDialog.spbLastClick(Sender: TObject);
var
  s : string;
begin
  if FLastPaths.Count>0 then with ShellComboBox do begin
    GoingBack:=true;
    s:=FLastPaths[0];
    if s[1]='#' then begin
      Root:=copy(s,2,length(s));
      ItemIndex:=0;
      end
    else begin
      if copy(s,1,2)='\\' then Root:='rfNetwork' else Root:='rfMyComputer';
      try Path:=s; except end;
      end;
    with FLastPaths do begin
      delete(0);
      spbLast.Enabled:=Count>0;
      end;
    end;
  GoingBack:=false;
  end;

procedure TShellFileDialog.ShellComboBoxChange(Sender: TObject);
begin
  with ShellComboBox do Hint:=Path;
  AddToPathHistory;
  end;

procedure TShellFileDialog.AddToPathHistory;
begin
  if not GoingBack and (length(FLastPath)>0) then with ShellComboBox do if ItemIndex>=0 then begin
    with Folders[Itemindex] do begin
     if (FLastPaths.Count=0) or not AnsiSameText(FLastPaths[0],FLastPath) then begin
        FLastPaths.Insert(0,FLastPath);
        spbLast.Enabled:=true;
        end;
      if Level=0 then FLastPath:='#'+Root
      else FLastPath:=PathName;
      end;
    end;
  end;

procedure TShellFileDialog.spbUpClick(Sender: TObject);
begin
  with ShellComboBox,Folders[ItemIndex] do begin
    if (Parent=nil) then begin
      Root:='rfMyComputer'; // Path:='';
      end
    else if (Parent.Level>0) then Path:=Parent.PathName;
    Refresh;
    end;
  end;

procedure TShellFileDialog.spbNewClick(Sender: TObject);
var
  s : string;
begin
  s:='';
  if InputQuery (ShellComboBox.Path,dgettext('dialogs','New subdirectory:'),s) then begin
    s:=IncludeTrailingPathDelimiter(ShellComboBox.Path)+s;
    if not ForceDirectories(s) then
      ErrorDialog('',Format(dgettext('dialogs','Could not create directory:'+sLineBreak+'%s!'),[s]))
    else ShellListView.Refresh;
    end;
  end;

procedure TShellFileDialog.itmDeleteClick(Sender: TObject);
var
  s : string;
begin
  s:=ShellComboBox.Path;
  if (DirFiles(s,true)>0) and
    not ConfirmDialog('',dgettext('dialogs','Directory is not empty, delete anyway?')) then Exit;
  spbUpClick(Sender);
  if not DeleteDirectory(s,true) then
    ErrorDialog('',Format(dgettext('dialogs','Error deleting directory:'+sLineBreak+'%s!'),[s]));
  end;

procedure TShellFileDialog.cbFilterChange(Sender: TObject);
begin
  ShellListView.Refresh;
  end;

procedure TShellFileDialog.ShellListViewClick(Sender: TObject);
begin
  with ShellListView do if (SelectedFolder<>nil) and (not SelectedFolder.IsFolder) then
    cbSelectedFile.Text:=SelectedFolder.PathName;
  end;

procedure TShellFileDialog.ShellListViewDblClick(Sender: TObject);
var
  s : string;
begin
  with ShellListView do if (SelectedFolder<>nil) then begin
    if SelectedFolder.IsFolder then begin
      s:=SelectedFolder.PathName;
      with ShellComboBox do begin
        Root:='rfMyComputer'; Path:=s;
        end;
      end
    else begin
      cbSelectedFile.Text:=SelectedFolder.PathName;
      AddHistory(cbSelectedFile.Text);
      ModalResult:=mrOK;
      end;
    end;
  end;

procedure TShellFileDialog.btbOKClick(Sender: TObject);
begin
  AddHistory(cbSelectedFile.Text);
  end;

{------------------------------------------------------------------- }
(* Dialog an Position anzeigen *)
function TShellFileDialog.Execute (const ATitle,
                                   TypeFilter,
                                   InitialDir,
                                   DefaultDir    : string;
                                   Options       : TOpenOptions;
                                   var Filename  : string) : boolean;
var
  ok  : boolean;
  n   : integer;
  r,s : string;

  function GetShellListViewIndex(const Filename : string) : integer;
  begin
    Result:=-1;
    if length(Filename)=0 then Exit;
    with ShellListView do for Result:=0 to FolderCount-1 do with Folders[Result] do
      if not IsFolder and AnsiSameText(DisplayName,Filename) then Exit;
    Result:=-1;
    end;

begin
  Caption:=ATitle; FDefaultDir:=DefaultDir;
  cbFilter.Filter:=TypeFilter;
  s:=InitialDir; FOptions:=Options;
  if length(s)=0 then s:=DefaultDir;
  if (length(s)=0) or not DirectoryExists(s)then begin
    s:=GetDesktopFolder(CSIDL_PERSONAL);
    r:='rfPersonal';
    if length(InitialDir)=0 then begin
      s:=GetCurrentDir;
      r:='rfMyComputer';
      end;
    end
  else begin
    if copy(s,1,2)='\\' then r:='rfNetwork' else r:='rfMyComputer';
    end;
  with ShellComboBox do begin
    Root:=r;
    ObjectTypes:=ObjectTypes+[otHidden];
    Path:=s;
    end;
  FLastPaths.Clear; FLastPath:=s;
  spbLast.Enabled:=false;
  n:=GetShellListViewIndex(ExtractFilename(Filename));
  if n>=0 then with ShellListView do begin
    ItemIndex:=n;
    cbSelectedFile.Text:=SelectedFolder.PathName
    end
  else cbSelectedFile.Text:=Filename;
  ok:=ShowModal=mrOK;
  if ok then Filename:=cbSelectedFile.Text;
  Result:=ok;
  end;

end.
