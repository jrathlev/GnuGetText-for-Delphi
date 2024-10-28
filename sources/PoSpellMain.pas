(* Tool for GnuGetText for Delphi
   Spell checker for translated strings (msgstr) using HunSpell

   © 2016-2023 Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.

   Jan. 2016
   last modified: April 2024
   *)

unit PoSpellMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons,
  PoParser, SpellChecker;

const
  Vers = ' - Vers. 3.1';

  PoExt = 'po';
  DicExt = 'dic';
  PersDic = 'PoSpell.dic';

type
  TfrmMain = class(TForm)
    Label2: TLabel;
    bbSave: TBitBtn;
    bbInfo: TBitBtn;
    bbExit: TBitBtn;
    Label5: TLabel;
    edWord: TEdit;
    btAdd: TButton;
    btReplace: TButton;
    btNext: TButton;
    meTrans: TMemo;
    Label3: TLabel;
    laEntry: TLabel;
    OpenDialog: TOpenDialog;
    bbReload: TBitBtn;
    Label1: TLabel;
    bbDictionary: TBitBtn;
    bbTranslation: TBitBtn;
    btGetSuggestion: TBitBtn;
    btnHelp: TBitBtn;
    edTranslation: TComboBox;
    edDictionary: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure bbSaveClick(Sender: TObject);
    procedure bbInfoClick(Sender: TObject);
    procedure bbExitClick(Sender: TObject);
    procedure bbDictionaryClick(Sender: TObject);
    procedure bbTranslationClick(Sender: TObject);
    procedure btAddClick(Sender: TObject);
    procedure btReplaceClick(Sender: TObject);
    procedure bbReloadClick(Sender: TObject);
    procedure edTranslationCloseUp(Sender: TObject);
    procedure edDictionaryCloseUp(Sender: TObject);
    procedure btGetSuggestionClick(Sender: TObject);
    procedure btNextClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
  private
    { Private-Deklarationen }
    ProgVersName,
    ProgVersDate,
    AppPath,UserPath,
    IniName,ProgPath,
    DicFile,PoFile,
    LastWord,
    TransStr           : string;
    PoList             : TPoEntryList;
    LastEntry,
    CurEntry           : TPoEntry;
    SpellCheck         : TSpellCheck;
    SuggList           : TStringList;
    WordPos,EntryCnt   : integer;
    Changed            : boolean;
    FsEnu              : TFormatSettings;
    procedure ShowMessage(const AMsg : string; IsErr : boolean = false);
    procedure GetSelectedIndex (n : integer);
    procedure SaveChangedFile;
    procedure SaveFile;
    function LoadFile : boolean;
    function LoadDic : boolean;
    function SelectDic: boolean;
    function SelectPo: boolean;
    procedure NotFound;
    function NotInDictionary (const s : string) : boolean;
    function GetNextEntry : boolean;
    function GetNextWord : boolean;
    function FindFirstWord (ValidPoList : boolean) : boolean;
    procedure ReplaceWord(const AWord : string);
    procedure ShowEntry (po : TPoEntry);
    function ValidEntry (po : TPoEntry) : boolean;
  public
    { Public-Deklarationen }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

uses System.IniFiles, Winapi.ShlObj, System.StrUtils, System.Character,
  GnuGetText, WinUtils, ListUtils, MsgDialogs, LangUtils, InitProg, ListSelectDlg,
  WinApiUtils, WinShell, PathUtils, StringUtils, GgtConsts, GgtUtils;

function RemoveLeadingChars (const s : string; CheckChars : array of char) : string;
var
  c : char;
  i : integer;
  ok  : boolean;
begin
  Result:=s; ok:=true;
  while (length(Result)>0) and ok do begin
    ok:=false;
    c:=Result[1];
    for i:=Low(CheckChars) to High(CheckChars) do ok:=ok or (c=CheckChars[i]);
    if ok then Delete(Result,1,1);
    end;
  end;

function RemoveTrailingChars (const s : string; CheckChars : array of char) : string;
var
  c : char;
  i : integer;
  ok  : boolean;
begin
  Result:=s;
  if length(Result)>0 then repeat
    c:=Result[length(Result)]; ok:=false;
    for i:=Low(CheckChars) to High(CheckChars) do ok:=ok or (c=CheckChars[i]);
    if ok then Delete(Result,length(Result),1);
    until not ok or (length(Result)=0);
  end;

{ ------------------------------------------------------------------- }
const
  (* INI-Sections *)
  DicSekt = 'Dictionaries';

  (* INI-Variables *)
  iniPoName = 'LastPo';
  iniDicName = 'LastDic';

procedure TfrmMain.FormCreate(Sender: TObject);
var
  IniFile  : TMemIniFile;
begin
  TranslateComponent (self);
  Application.Title:=_('Spell checking of po translations');
  InitPaths(AppPath,UserPath,ProgPath);
  InitVersion(Application.Title,Vers,CopRgt,3,3,ProgVersName,ProgVersDate);
  Caption:=ProgVersName;
  if ParamCount>0 then PoFile:=ExpandFileName(ParamStr(1))
  else PoFile:='';
  IniName:=Erweiter(AppPath,PrgName,IniExt);
  IniFile:=TMemIniFile.Create(IniName);
  with IniFile do begin
    Top:=ReadInteger(CfGSekt,iniTop,Top);
    Left:=ReadInteger(CfGSekt,iniLeft,Left);
    if length(PoFile)=0 then PoFile:=ReadString(CfGSekt,iniPoName,'');
    DicFile:=ReadString(CfGSekt,iniDicName,'');
    LoadHistory(IniFile,TransSekt,edTranslation);
    with edTranslation do begin
      if Items.Count=0 then Style:=csSimple else Style:=csDropDown;
      Text:=PoFile;
      end;
    LoadHistory(IniFile,DicSekt,edDictionary);
    with edDictionary do begin
      if Items.Count=0 then Style:=csSimple else Style:=csDropDown;
      Text:=DicFile;
      end;
    Free;
    end;
  WordPos:=0; TransStr:='';
  PoList:=TPoEntryList.Create;
  SpellCheck:=TSpellCheck.Create(self);
  with SpellCheck do begin
    UseUserDictionary:=true;
    UserDictSubDir:='SpellCheck';
    UserDictName:='PoSpell';
    Options:=[sscoIgnoreSingleChars,sscoSuggestWords];
    end;
    SuggList:=TStringList.Create;
//  DicList:=TStringList.Create;
//  with DicList do begin
//    CaseSensitive:=false; Sorted:=true;
//    end;
//  UsrList:=TStringList.Create;
//  with UsrList do begin
//    CaseSensitive:=false; Sorted:=true;
//    end;
//  UsrFile:=AppPath+'PoSpell.dic';
//  if FileExists(UsrFile) then UsrList.LoadFromFile(UsrFile,TEncoding.UTF8);
//  Changed:=false;
  CurEntry:=nil; LastEntry:=nil;
  FsEnu:=FormatSettings;;
  FsEnu.DecimalSeparator:='.';
  end;

procedure TfrmMain.FormShow(Sender: TObject);
var
  ok : boolean;
begin
  with ListSelectDialog do begin
    Sorted:=true; Width:=edWord.Width;
    OnSelect:=GetSelectedIndex;
    end;
  ok:=false;
  if FileExists(DicFile) then ok:=LoadDic;
  if not ok then begin
    ok:=SelectDic;
    if ok then ok:=LoadDic;
    end;
  if ok then begin
    if FileExists(PoFile) then ok:=LoadFile
    else begin
      ok:=SelectPo;
      if ok then LoadFile
      end;
    FindFirstWord(ok);
    end
  else Close;
  end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
var
  IniFile  : TMemIniFile;
begin
  IniFile:=TMemIniFile.Create(IniName);
  with IniFile do begin
    WriteInteger(CfGSekt,iniTop,Top);
    WriteInteger(CfGSekt,iniLeft,Left);
    WriteString(CfGSekt,iniPoName,PoFile);
    WriteString(CfGSekt,iniDicName,DicFile);
    SaveHistory(IniFile,TransSekt,true,edTranslation);
    SaveHistory(IniFile,DicSekt,true,edDictionary);
    UpdateFile;
    Free;
    end;
//  with UsrList do begin
//    if Count>0 then SaveToFile(UsrFile,TEncoding.UTF8);
//    Free;
//    end;
//  DicList.Free;
  SuggList.Free;
  SpellCheck.Free;
  PoList.Free;
  try HtmlHelp(0,nil,HH_CLOSE_ALL,0); except end;
  end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  SaveChangedFile;
  end;

procedure TfrmMain.SaveChangedFile;
begin
  if Changed and ConfirmDialog(TopLeftPos(bbReload),_('Save changed po file?')) then SaveFile
  else begin
    bbSave.Enabled:=false;
    Changed:=false;
    end;
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

procedure TfrmMain.bbReloadClick(Sender: TObject);
begin
  SaveChangedFile;
  FindFirstWord(LoadFile);
  end;

procedure TfrmMain.bbSaveClick(Sender: TObject);
begin
  SaveFile;
  end;

procedure TfrmMain.bbDictionaryClick(Sender: TObject);
begin
  if SelectDic then LoadDic;
  end;

procedure TfrmMain.bbTranslationClick(Sender: TObject);
begin
  if SelectPo then begin
    FindFirstWord(LoadFile);
    end;
  end;

procedure TfrmMain.edDictionaryCloseUp(Sender: TObject);
begin
  with edDictionary do DicFile:=Items[ItemIndex];
  LoadDic;
  bbReloadClick(Sender);
  end;

procedure TfrmMain.edTranslationCloseUp(Sender: TObject);
begin
  with edTranslation do PoFile:=Items[ItemIndex];
  FindFirstWord(LoadFile);
  end;

function TfrmMain.SelectPo: boolean;
begin
  with OpenDialog do begin
    if length(PoFile)>0 then InitialDir:=ExtractFilePath(PoFile)
    else InitialDir:=UserPath;
    Filename:='';
    Title:=_('Select po file for spell checking');
    Filter:=Format(_('po files|*.%s|all|*.*'),[PoExt]);
    if Execute then begin
      PoFile:=Filename;
      Text:=Filename; AddToHistory(edTranslation,Filename);
      Result:=true;
      end
    else Result:=false;
    end;
  end;

function TfrmMain.SelectDic: boolean;
begin
  with OpenDialog do begin
    if length(DicFile)>0 then InitialDir:=ExtractFilePath(DicFile)
    else InitialDir:=AddPath(PrgPath,'Dict');
    Filename:='';
    Title:=_('Select dictionary file');
    Filter:=_('dictionaries|*.'+DicExt+'|all|*.*');
    if Execute then begin
      DicFile:=Filename;
      Text:=Filename; AddToHistory(edDictionary,Filename);
      Result:=true;
      end
    else Result:=false;
    end;
  end;

procedure TfrmMain.SaveFile;
var
  sn,s   : string;
begin
  sn:=PoFile+'.tmp';
  PoList.SaveToFile(sn,true);
  s:=PoFile+'.bak';
  if FileExists(s) then DeleteFile(s);
  RenameFile(PoFile,s);
  RenameFile(sn,PoFile);
  bbSave.Enabled:=false; Changed:=false;
  end;

procedure TfrmMain.ShowMessage(const AMsg : string; IsErr : boolean);
begin
  with laEntry do begin
    with Font do if IsErr then Color:=clRed else Color:=clBlue;
    Caption:=AMsg;
    end;
  end;

function TfrmMain.LoadFile : boolean;
var
  ne : integer;
begin
  SaveChangedFile;
  meTrans.Clear;
  Result:=false;
  if not FileExists(PoFile) then begin
    ShowMessage(Format(_('File not found: %s'),[PoFile]),true);
    Exit;
    end;
  Changed:=false;
  try
    ne:=PoList.LoadFromFile(PoFile);
    if ne>0 then
      ShowMessage(Format(_('Error in line %u of po file!'),[ne]),true)
    else begin
      ShowMessage(_('po file loaded!'));
      EntryCnt:=0;
      Result:=true;
      end;
  except
    ShowMessage(Format(_('Error reading po file: %s'),[PoFile]),true);
    end;
  end;

function TfrmMain.LoadDic : boolean;
begin
  SaveChangedFile;
  meTrans.Clear;
  Result:=false;
  if not FileExists(DicFile) then begin
    ShowMessage(Format(_('File not found: %s'),[DicFile]),true);
    Exit;
    end;
  with SpellCheck do Result:=(LoadDictionary(DicFile) and SelectDictionary);
  if not Result then begin
    ShowMessage(_('Error reading dictionary file!'),true);
    Exit;
    end;
  end;

procedure TfrmMain.ShowEntry(po : TPoEntry);
begin
  with meTrans do begin
    Clear;
    Text:=ReplaceStr(po.MsgStr,#10,sLineBreak);
    end;
  ShowMessage(_('Line number: ')+IntToStr(po.IdLine)
    +' ('+IntToStr(round(EntryCnt*100/PoList.TotalEntries))+'%)');
  TransStr:=Trim(ReplaceStr(po.MsgStr,#10,' '));
  WordPos:=1;
  end;

function TfrmMain.ValidEntry(po : TPoEntry) : boolean;
begin
  with po do Result:=(length(MsgId)>0) and (length(MsgStr)>0)
    and not AnsiSametext(MsgId,MsgStr);
  end;

procedure TfrmMain.NotFound;
begin
  ErrorDialog(BottomLeftPos(edWord),_('No more words found!'));
  end;

function TfrmMain.GetNextEntry : boolean;
begin
  if CurEntry<>nil then begin
    repeat
      LastEntry:=CurEntry;
      CurEntry:=PoList.FindNext(CurEntry);
      inc(EntryCnt);
      Result:=CurEntry<>nil;
      until not Result or ValidEntry(CurEntry);
    end
  else Result:=false;
  if Result then ShowEntry(CurEntry)
  else ShowEntry(LastEntry);
  end;

function TfrmMain.NotInDictionary (const s : string) : boolean;
var
  f : double;
  n : integer;
begin
  Result:=not TryStrToInt(s,n);                        // exclude numbers
  if Result then Result:=not TryStrToFloat(s,f);
  if Result then Result:=not TryStrToFloat(s,f,FsEnu);
//  if Result then Result:=(DicList.IndexOf(s)<0) and (UsrList.IndexOf(s)<0);
  if Result then Result:=not SpellCheck.CheckWord(s);
  end;

function TfrmMain.GetNextWord : boolean;
var
  n1,n2   : integer;
begin
  repeat
    if WordPos>length(TransStr) then Result:=GetNextEntry else Result:=true;
    if Result then begin
      n1:=Pos(' ',TransStr,WordPos);
      n2:=Pos('-',TransStr,WordPos);
      if n1=0 then n1:=Length(TransStr)+1;
      if (n2>0) and (n2<n1) then n1:=n2;
      LastWord:=Trim(copy(TransStr,WordPos,n1-WordPos));
      LastWord:=RemoveLeadingChars(LastWord,['(','[','{','<','.',':','-',' ','"','''']);
      LastWord:=RemoveTrailingChars(LastWord,[',','.','?','!',':',';','-',')',']','}','>',' ','"','''']);
      LastWord:=RemoveCharacters(LastWord,['&']);
      WordPos:=n1+1;
      end;
    until not Result or ((length(LastWord)>1) and (LastWord[1]<>'%') and NotInDictionary(LastWord)); // not found in dictionary
  if Result then edWord.Text:=LastWord;
  end;

function TfrmMain.FindFirstWord (ValidPoList : boolean) : boolean;
var
  n   : integer;
begin
  if ValidPoList then begin
    EntryCnt:=0; n:=1;
  //  NumDialog(BottomRightPos(btStart),_('Start at'),_('Line number: '),1,999999,1,imFixed,n);
    CurEntry:=PoList.FindFirst;
    if (CurEntry<>nil) and (n>1) then repeat
      LastEntry:=CurEntry;
      CurEntry:=PoList.FindNext(CurEntry);
      inc(EntryCnt);
      until (CurEntry=nil) or (CurEntry.IdLine>=n);
    Result:=CurEntry<>nil;
    if Result then begin
      if not ValidEntry(CurEntry) then Result:=GetNextEntry;
      if Result then begin
        ShowEntry(CurEntry);
        Result:=GetNextWord;
        end;
      end
    else if LastEntry<>nil then ShowEntry(LastEntry);
    if not Result then
      ErrorDialog(BottomLeftPos(bbTranslation),_('No misspelled words found!'));
    end
  else begin
    meTrans.Text:='';
    edWord.Text:='';
    end;
  btAdd.Enabled:=ValidPoList and Result;
  btReplace.Enabled:=btAdd.Enabled;
  btNext.Enabled:=btAdd.Enabled;
  btGetSuggestion.Enabled:=btAdd.Enabled;
  end;

procedure TfrmMain.btAddClick(Sender: TObject);
begin
  SpellCheck.AddDictWord(edWord.Text);
  if not GetNextWord then NotFound;
  end;

procedure TfrmMain.ReplaceWord(const AWord : string);
begin
  with CurEntry do begin
    MsgStr:=AnsiReplaceText(MsgStr,LastWord,AWord);
    meTrans.Text:=ReplaceStr(MsgStr,#10,sLineBreak);
    end;
  bbSave.Enabled:=true; Changed:=true;
  if NotInDictionary(AWord) then SpellCheck.AddDictWord(AWord);
//  if not GetNextWord then NotFound;
  end;

procedure TfrmMain.GetSelectedIndex (n : integer);
begin
//  ReplaceWord(SuggList[n]);
//  if not GetNextWord then NotFound;
  edWord.Text:=SuggList[n];
  end;

procedure TfrmMain.btGetSuggestionClick(Sender: TObject);
var
  i  : integer;
begin
  SuggList.Clear;
  with ListSelectDialog do begin
    Clear;
    SpellCheck.GetSuggestions(edWord.Text,SuggList);
    for i:=0 to SuggList.Count-1 do AddItem(SuggList[i],i);
    end;
  ListSelectDialog.ShowList(BottomLeftPos(edWord,0,2));
  end;

procedure TfrmMain.btNextClick(Sender: TObject);
begin
  if not GetNextWord then NotFound;
  end;

procedure TfrmMain.btnHelpClick(Sender: TObject);
begin
  ShowHelp('tools.html#spell');
  end;

procedure TfrmMain.btReplaceClick(Sender: TObject);
begin
  ReplaceWord(edWord.Text);
  end;

end.
