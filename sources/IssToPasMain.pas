(* Tool for GnuGetText for Delphi
   Convert CustomMessages section of iss file to Pascal unit

   © Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.

   Command line:
   -------------
   Calling:  IssToPas <filename> [options]
     <filename>       - Name of iss file from where to extract the CustomMessages
     /prefix:<langid> - Language identifier (two letter, e.g. en, de, ..)
     /out:<UnitName>  - Use this unit name

   J. Rathlev, November 2023
   *)

unit IssToPasMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, HListBox,
  Vcl.ComCtrls, Vcl.ExtCtrls;

const
  Vers = ' - Vers. 3.1';

type
  TfrmMain = class(TForm)
    edIssFile: THistoryCombo;
    Label2: TLabel;
    bbIssFile: TBitBtn;
    Label1: TLabel;
    edLanguage: THistoryCombo;
    bbSave: TBitBtn;
    bbInfo: TBitBtn;
    bbExit: TBitBtn;
    OpenDialog: TOpenDialog;
    StatusBar: TStatusBar;
    laTitle: TLabel;
    edOutName: TLabeledEdit;
    bbCopyName: TBitBtn;
    btnHelp: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure bbExitClick(Sender: TObject);
    procedure bbInfoClick(Sender: TObject);
    procedure bbSaveClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure edLanguageCloseUp(Sender: TObject);
    procedure bbCopyNameClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
  private
    { Private-Deklarationen }
    ProgVersName,
    ProgVersDate,
    AppPath,UserPath,
    IniName,ProgPath,
    IssName,LangId,
    PasName            : string;
    function SelectIss : boolean;
  public
    { Public-Deklarationen }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

uses System.IniFiles, Winapi.ShlObj, System.StrUtils, gnugettext, ggtconsts, ggtutils,
  WinUtils, LangUtils, InitProg, WinApiUtils, PathUtils, NumberUtils, MsgDialogs;

const
  CuMsg = 'CustomMessages';
  IssExt = 'iss';
  PasExt = '.pas';
  UnitHeader = 'unit %s;'+sLineBreak+sLineBreak+
               'interface'+sLineBreak+'resourcestring'+sLineBreak;
  UnitTrailer = sLineBreak+'implementation'+sLineBreak+'end.';

{ ------------------------------------------------------------------- }
const
  (* INI-Sections *)
  IssSekt = 'IssFiles';
  LangSekt = 'Languages';

  (* INI-Variables *)
  iniLast = 'LastFile';
  îniLang = 'LastLanguage';

procedure TfrmMain.FormCreate(Sender: TObject);
var
  sn : string;
  i  : integer;
begin
  TranslateComponent (self);
  Application.Title:=_('Convert CustomMessages from iss to pas');
  InitPaths(AppPath,UserPath,ProgPath);
  InitVersion(Application.Title,Vers,CopRgt,3,3,ProgVersName,ProgVersDate);
  Caption:=ProgVersName;
  IniName:=Erweiter(AppPath,PrgName,IniExt);
  with TIniFile.Create(IniName) do begin
    Top:=ReadInteger(CfGSekt,iniTop,Top);
    Left:=ReadInteger(CfGSekt,iniLeft,Left);
    IssName:=ReadString(CfGSekt,iniLast,'');
    LangId:=ReadString(CfGSekt,îniLang,'en');
    Free;
    end;
  PasName:='';
  if ParamCount>0 then begin
    for i:=1 to ParamCount do begin
      sn:=ParamStr(i);
      if (sn[1]='/') or (sn[1]='-') then begin
        Delete(sn,1,1);
        if ReadOptionValue(sn,'prefix') then LangId:=sn
        else if ReadOptionValue(sn,'out') then PasName:=sn
        end
      else issName:=ExpandFileName(sn);
      end;
    end;
  if length(Pasname)=0 then Pasname:=ChangeFileExt(ExtractFilename(IssName),PasExt);
  with edIssFile do begin
    LoadFromIni(IniName,IssSekt);
    if Items.Count=0 then Style:=csSimple else Style:=csDropDown;
    Text:=IssName;
    end;
  with edLanguage do begin
    LoadFromIni(IniName,LangSekt);
    if Items.Count=0 then Style:=csSimple else Style:=csDropDown;
    Text:=LangId;
    end;
  edOutName.Text:=PasName;
  end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  if not FileExists(issName) then SelectIss;
  end;

function TfrmMain.SelectIss : boolean;
begin
  with OpenDialog do begin
    InitialDir:=GetCurrentDir;
    Filename:='';
    DefaultExt:=IssExt;
    Title:=_('Select iss file');
    Filter:=Format(_('iss files|*.%s|all|*.*'),[IssExt]);
    Result:=Execute;
    if Result then issName:=Filename;
    end;
  if Result then edIssFile.Text:=IssName;
  end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  with TIniFile.Create(IniName) do begin
    WriteInteger(CfGSekt,iniTop,Top);
    WriteInteger(CfGSekt,iniLeft,Left);
    WriteString(CfGSekt,iniLast,IssName);
    WriteString(CfGSekt,îniLang,LangId);
    Free;
    end;
  try HtmlHelp(0,nil,HH_CLOSE_ALL,0); except end;
  end;

procedure TfrmMain.bbSaveClick(Sender: TObject);
var
  sn,sv    : string;
  nl,vl    : TStringList;
  i        : integer;
begin
  nl:=TStringList.Create; vl:=TStringList.Create;
  with TIniFile.Create(issName) do begin
    ReadSection(CuMsg,nl);
    with nl do for i:=0 to Count-1 do begin
      sn:=Strings[i];
      if AnsiStartsText(LangId,sn) then begin
        sv:=ReadString(CuMsg,sn,'');
        System.Delete(sn,1,length(LangId)+1);
        vl.Add('  '+sn+' = '''+sv+''';');
        end;
      end;
    Free;
    end;
  nl.Free;
  with vl do if Count>0 then begin
    if length(PasName)=0 then PasName:=ChangeFileExt(ExtractFileName(issName),'');
    Insert(0,Format(UnitHeader,[PasName]));
    Add(UnitTrailer);
    sn:=ExtractFilePath(issName)+PasName;
    sn:=ChangeFileExt(sn,PasExt);
    SaveToFile(sn);
    StatusBar.SimpleText:=Format(_('%u strings written to "%s"'),[Count,sn]);    end
  else StatusBar.SimpleText:=Format(_('No messages found for ID: "%s"'),[LangId]);
  vl.Free;
  end;

procedure TfrmMain.btnHelpClick(Sender: TObject);
begin
  ShowHelp('tools.html#isstrans');
  end;

procedure TfrmMain.edLanguageCloseUp(Sender: TObject);
var
  s : string;
begin
  with edLanguage do s:=Items[ItemIndex];
  LangId:=s;
  end;

procedure TfrmMain.bbCopyNameClick(Sender: TObject);
begin
  PasName:=ChangeFileExt(ExtractFilename(IssName),PasExt);
  edOutName.Text:=PasName;
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

end.
