(* Tool for GnuGetText for Delphi
   Import all not translated items from anaothe po file

   © 2024-2025 Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.

   September 2024
   last modified: September 2025
   *)

unit PoImportMain;

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
    cbEdit: TComboBox;
    Label2: TLabel;
    sbEdit: TSpeedButton;
    cbTrans: TComboBox;
    Label5: TLabel;
    sbTrans: TSpeedButton;
    bbSave: TBitBtn;
    bbInfo: TBitBtn;
    bbExit: TBitBtn;
    OpenDialog: TOpenDialog;
    laStatus: TLabel;
    btnHelp: TBitBtn;
    cbFuzzy: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure bbExitClick(Sender: TObject);
    procedure bbInfoClick(Sender: TObject);
    procedure sbEditClick(Sender: TObject);
    procedure sbTransClick(Sender: TObject);
    procedure bbSaveClick(Sender: TObject);
    procedure cbEditCloseUp(Sender: TObject);
    procedure cbTransCloseUp(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cbDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
      State: TOwnerDrawState);
    procedure btnHelpClick(Sender: TObject);
  private
    { Private-Deklarationen }
    ProgVersName,
    ProgVersDate,
    AppPath,UserPath,
    IniName,ProgPath,
    EdFile,ImportFile     : string;
    EdList,ImportList     : TPoEntryList;
    function LoadFile (const fn : string; PoList : TPoEntryList) : boolean;
    procedure UpdateButton;
    function SelectEdit : boolean;
    function SelectImport : boolean;
  public
    { Public-Deklarationen }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

uses System.IniFiles, GnuGettext, InitProg, WinUtils, ListUtils, PathUtils,
  StringUtils, MsgDialogs, GgtConsts, GgtUtils;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  IniFile  : TMemIniFile;
  i : integer;
begin
  TranslateComponent (self);
  Application.Title:=_('Import translations from po file');
  InitPaths(AppPath,UserPath,ProgPath);
  InitVersion(Application.Title,Vers,CopRgt,3,3,ProgVersName,ProgVersDate);
  Caption:=ProgVersName; EdFile:='';
  if ParamCount>0 then for i:=1 to ParamCount do if not IsOption(ParamStr(i)) then begin
    if EdFile.IsEmpty then EdFile:=ExpandFileName(ParamStr(i));
    end;
  IniName:=Erweiter(AppPath,PrgName,IniExt);
  IniFile:=TMemIniFile.Create(IniName);
  with IniFile do begin
    if length(EdFile)=0 then EdFile:=ReadString(CfGSekt,iniTempl,'');
    ImportFile:=ReadString(CfGSekt,iniTrans,'');
    LoadHistoryList(IniFile,TemplSekt,cbEdit);
    LoadHistoryList(IniFile,TransSekt,cbTrans);
    Free;
    end;
  EdList:=TPoEntryList.Create;
  ImportList:=TPoEntryList.Create;
  end;

procedure TfrmMain.FormShow(Sender: TObject);
var
  ok : boolean;
begin
  ok:=false;
  if FileExists(EdFile) then ok:=LoadFile(EdFile,EdList)
  else if SelectEdit then ok:=LoadFile(EdFile,EdList);
  if ok then begin
    if FileExists(ImportFile) then ok:=LoadFile(ImportFile,ImportList)
    else if SelectImport then ok:=LoadFile(ImportFile,ImportList);
    end;
  laStatus.Caption:='';
  AddToHistoryList(cbEdit,EdFile);
  AddToHistoryList(cbTrans,ImportFile);
  UpdateButton;
  end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
var
  IniFile  : TMemIniFile;
begin
  IniFile:=TMemIniFile.Create(IniName);
  with IniFile do begin
    WriteString(CfGSekt,iniTempl,EdFile);
    WriteString(CfGSekt,iniTrans,ImportFile);
    SaveHistory(IniFile,TemplSekt,cbEdit);
    SaveHistory(IniFile,TransSekt,cbTrans);
    UpdateFile;
    Free;
    end;
  EdList.Free;
  ImportList.Free;
  end;

{ ------------------------------------------------------------------- }
procedure TfrmMain.UpdateButton;
begin
  bbSave.Enabled:=EDList.Loaded and ImportList.Loaded;
  end;

function TfrmMain.SelectEdit : boolean;
begin
  with OpenDialog do begin
    InitialDir:=GetExistingParentPath(ExtractFilePath(EdFile),UserPath);
    Filename:='';
    Title:=_('Select po file to be edited');
    Filter:=Format(_('po files|*.%s|all|*.*'),[PoExt]);
    if Execute then begin
      EdFile:=FileName;
      AddToHistoryList(cbEdit,Filename);
      Result:=true;
      end
    else Result:=false;
    end;
  end;

function TfrmMain.SelectImport : boolean;
var
 sd : string;
begin
  with OpenDialog do begin
    if length(ImportFile)>0 then sd:=ExtractFilePath(ImportFile)
    else sd:=ExtractFilePath(EdFile);
    InitialDir:=GetExistingParentPath(sd,UserPath);
    Filename:='';
    Title:=_('Select po file from where to import');
    Filter:=Format(_('po files|*.%s|all|*.*'),[PoExt]);
    if Execute then begin
      ImportFile:=Filename;
      AddToHistoryList(cbTrans,Filename);
      Result:=true;
      end
    else Result:=false;
    end;
  end;

{ ------------------------------------------------------------------- }
procedure TfrmMain.bbExitClick(Sender: TObject);
begin
  Close;
  end;

procedure TfrmMain.bbInfoClick(Sender: TObject);
begin
  InfoDialog(BottomLeftPos(bbInfo,0,10),ProgVersName+' - '+ProgVersDate+#13+CopRgt
           +sLineBreak+'E-Mail: '+EmailAdr);
  end;

procedure TfrmMain.sbEditClick(Sender: TObject);
begin
  if SelectEdit then LoadFile(EdFile,EdList);
  end;

procedure TfrmMain.sbTransClick(Sender: TObject);
begin
  if SelectImport then LoadFile(ImportFile,ImportList);;
  end;

procedure TfrmMain.cbEditCloseUp(Sender: TObject);
begin
  with cbEdit do begin
    EdFile:=Items[ItemIndex];
    end;
  LoadFile(EdFile,EdList);
  end;

procedure TfrmMain.cbDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
begin
  with Control as TComboBox,Canvas do begin
    if (odSelected in State) and (ItemIndex>=0) then begin
      Brush.Color:=clSkyBlue;
      Font.Color:=clNavy;
      end
    else begin
      Brush.Color:=Color;
      Pen.Color:=Color;
      end;
    Font.Color:=clBlack;
    FillRect(Rect);
    if odFocused in State then  // also check for styles if there's a possibility of using ..
      DrawFocusRect(Rect);
    TextOut(Rect.Left, Rect.Top,' '+Items[Index]);
    end;
  end;

procedure TfrmMain.cbTransCloseUp(Sender: TObject);
begin
  with cbTrans do begin
    ImportFile:=Items[ItemIndex];
    end;
  LoadFile(ImportFile,ImportList);
  end;

function TfrmMain.LoadFile (const fn : string; PoList : TPoEntryList) : boolean;
var
  ne : integer;
begin
  Result:=false;
  if FileExists(fn) then begin
    try
      ne:=PoList.LoadFromFile(fn);
      if ne>0 then ErrorDialog(Format(_('Error in line %u of file "%s"!'),[ne,fn]))
      else Result:=true;
    except
      ErrorDialog(Format(_('Error reading from file "%s"!'),[fn]));
      end;
    end
  else ErrorDialog(Format(_('File not found: "%s"!'),[fn]));
  laStatus.Caption:='';
  UpdateButton;
  end;

procedure TfrmMain.bbSaveClick(Sender: TObject);
var
  pe,pi : TPoEntry;
  sn,s   : string;
  n      : integer;
begin
  with EdList do begin
    pe:=FindFirst; n:=0;
    while (pe<>nil) do begin
      if not pe.MsgId.IsEmpty and pe.MsgStr.IsEmpty then begin
        pi:=ImportList.FindEntry(pe.MsgId);
        if (pi<>nil) and (pe.MsgStr.IsEmpty or (cbFuzzy.Checked and pe.Fuzzy)) then begin
          pe.MsgStr:=pi.MsgStr;
          inc(n);
          end;
        end;
      pe:=FindNext(pe);
      end;
    end;
  laStatus.Caption:=Format(_('%u strings imported'),[n]);
  sn:=EdFile+'.tmp';
  EdList.SaveToFile(sn,true);
  s:=EdFile+'.bak';
  if FileExists(s) then DeleteFile(s);
  RenameFile(EdFile,s);
  RenameFile(sn,EdFile);
  end;

procedure TfrmMain.btnHelpClick(Sender: TObject);
begin
  ShowHelp('tools.html#import');
  end;

end.
