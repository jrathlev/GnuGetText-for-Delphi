(* Tool for GnuGetText for Delphi
   ==============================
   
   Get translated strings from Pascal files (resourcestring) 
   and insert them into po file as translation 

   © Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.

   Sep. 2016
   *)

unit PoFromPasMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, HListBox, PoParser,
  Vcl.ComCtrls, Vcl.ExtCtrls;

const
  Vers = ' - Vers. 1.0';
  PrgNamId = 'Programmer''s name for it: ';

type
  TTextInfo = class (TObject)
  private
    FText,FUnit : string;
  public
    constructor Create (const AUnit,AText : string);
    end;

  TConst= class (TObject)
    Name, Value : string;
    end;

  TfrmMain = class(TForm)
    Label2: TLabel;
    Label5: TLabel;
    sbEdit: TSpeedButton;
    edDir: THistoryCombo;
    edEdit: THistoryCombo;
    bbInfo: TBitBtn;
    bbExit: TBitBtn;
    bbSave: TBitBtn;
    OpenDialog: TOpenDialog;
    btDir: TSpeedButton;
    StatusBar: TStatusBar;
    rgEncoding: TRadioGroup;
    cbOverwrite: TCheckBox;
    btnHelp: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure bbExitClick(Sender: TObject);
    procedure bbInfoClick(Sender: TObject);
    procedure sbEditClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure edDirCloseUp(Sender: TObject);
    procedure edEditCloseUp(Sender: TObject);
    procedure bbSaveClick(Sender: TObject);
    procedure btDirClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
  private
    { Private-Deklarationen }
    ProgVersName,
    ProgVersDate,
    AppPath,UserPath,
    IniName,ProgPath,
    PasDir,EdFile      : string;
    EdList             : TPoEntryList;
    RsList,
    constlist          : TStringList;
    Loaded,Changed     : boolean;
    function SelectEdit : boolean;
    function LoadEditFile : boolean;
    procedure ExtractFromPascal(sourcefilename: string);
  public
    { Public-Deklarationen }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

uses System.IniFiles, Winapi.ShlObj, System.StrUtils, gnugettext, PathUtils,
  WinUtils, MsgDialogs, LangUtils, InitProg, WinApiUtils, WinShell, StringUtils,
  ShellDirDlg, xgettexttools, GgtConsts, GgtUtils;

{ ------------------------------------------------------------------- }
constructor TTextInfo.Create (const AUnit,AText : string);
begin
  inherited Create;
  FText:=AText; FUnit:=AUnit;
  end;

{ ------------------------------------------------------------------- }
const
  (* INI-Variables *)
  iniDir = 'Directory';
  iniTrans = 'Translation';

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  TranslateComponent (self);
  Application.Title:=_('Insert translated strings from pas files');
  InitPaths(AppPath,UserPath,ProgPath);
  InitVersion(Application.Title,Vers,CopRgt,3,3,ProgVersName,ProgVersDate);
  Caption:=ProgVersName;
  IniName:=Erweiter(AppPath,PrgName,IniExt);
  with TIniFile.Create(IniName) do begin
    Top:=ReadInteger(CfGSekt,iniTop,Top);
    Left:=ReadInteger(CfGSekt,iniLeft,Left);
    PasDir:=ReadString(CfGSekt,iniDir,'');
    EdFile:=ReadString(CfGSekt,iniTrans,'');
    Free;
    end;
  with edDir do begin
    LoadFromIni(IniName,DirSekt);
    if Items.Count=0 then Style:=csSimple else Style:=csDropDown;
    Text:=PasDir;
    end;
  with edEdit do begin
    LoadFromIni(IniName,TransSekt);
    if Items.Count=0 then Style:=csSimple else Style:=csDropDown;
    Text:=EdFile;
    end;
  EdList:=TPoEntryList.Create;
  RsList:=TStringList.Create;  // list of resourcestrings
  constlist:=TStringList.Create;
  with constlist do begin
    Sorted:=True; Duplicates:=dupError; CaseSensitive:=True;
    end;
  RsList.Sorted:=true;
  Loaded:=false; Changed:=false;
  end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  if DirectoryExists(PasDir) then edDir.Text:=PasDir else edDir.Text:='';
  if FileExists(EdFile) then LoadEditFile else edEdit.Text:='';
  end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  with TIniFile.Create(IniName) do begin
    WriteInteger(CfGSekt,iniTop,Top);
    WriteInteger(CfGSekt,iniLeft,Left);
    WriteString(CfGSekt,iniDir,PasDir);
    WriteString(CfGSekt,iniTrans,EdFile);
    Free;
    end;
  try HtmlHelp(0,nil,HH_CLOSE_ALL,0); except end;
  end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  EdList.Free;
  FreeListObjects(RsList); FreeListObjects(constlist);
  RsList.Free; constlist.Free;
  end;

{ ------------------------------------------------------------------- }
// parse Pascal source for resourcestrings (see: xgettext.pas)
procedure TfrmMain.ExtractFromPascal(sourcefilename: string);
var
  LastLineRead,
  commentmode, // Empty means that dxreadln is not inside a comment
  lastcomment : string;
  linenr      : Integer;
  resourcestringmode : Integer;  // 0=None, 1=Const, 2=Resourcestring

  procedure ShowError (const Msg : string);
  begin
    ErrorDialog(Msg+sLineBreak+'"'+LastLineRead+'"');
    end;

  procedure dxreadln(var src: TextFile; var line: string);
  var
    i : integer;

    procedure cutuntil (endtag : string);
    var
      p : integer;
    begin
      p:=i+length(endtag)-1;
      while p<=length(line) do begin
        if copy(line,p,length(endtag))=endtag then begin
          delete (line,i,p+length(endtag)-i);
          exit;
        end;
        inc (p);
      end;
      // At this place, the end tag was not found in the line
      line:=copy(line,1,i-1);
      commentmode:=endtag;
    end;

  begin
    line:='';
    while (not eof(src)) and (line='') do begin
      if commentmode<>'' then begin
        while true do begin
          if eof(src) then begin
            line:='';
            exit;
          end;
          readln (src, line);
          line:=trim(line);
          LastLineRead:=line;
          inc (linenr);
          i:=pos(commentmode,line);
          if i<>0 then begin
            delete (line,1,i+length(commentmode)-1);
            commentmode:='';
            break;
          end;
        end;
      end else begin
        readln (src, line);
        line:=trim(line);
        LastLineRead:=line;
        inc (linenr);
        if trim(line)='' then
          lastcomment:='';
      end;
      i:=1;
      while i<=length(line) do begin
        if copy(line,i,1)='''' then begin
          // A string was detected - find the end of it.
          inc (i);
          while true do begin
            if copy(line,i,1)='''' then begin
              inc (i);
              break;
            end;
            // If the string doesn't end until the line is over, finish the procedure
            if i>=length(line) then
              exit;
            inc (i);
          end;
        end else
        if copy(line,i,2)='//' then begin
          // The rest of the line is a comment
          if lastcomment<>'' then
            lastcomment:=lastcomment+sLineBreak;
          lastcomment:=trim(copy(line,i+2,maxint));
          line:=copy(line,1,i-1);
          exit;
        end else
        if copy(line,i,1)='{' then begin
          // Bracket comment
          cutuntil ('}');
        end else
        if copy(line,i,2)='(*' then begin
          // Bracket comment, Danish style
          cutuntil ('*)');
        end else
          inc (i);
      end;
      line:=trim(line);
    end;
  end;

  function RemoveNuls (const s:string):string;
  // Since #0 is used to separate msgid_plural values inside msgid strings
  // in this software, #0 cannot be present in msgid values. In order to
  // prevent this, this function replaces #0 with '#0'.
  var
    p:integer;
  begin
    Result:=s;
    while true do begin
      p:=pos(#0,Result);
      if p=0 then break;
      Result:=MidStr(Result,1,p-1)+'#0'+MidStr(Result,p+1,maxint);
    end;
  end;

  procedure extractstring(var source: string; var res: string);
  const
    whitespace=[#0..#32];
  // Extracts the Pascal coded string at the beginning of source.
  // Returns the result in res.
  // Removes the extracted data from source.
  var
    charset: set of char;
    s: string;
    constname,uconstname:string;
    idx:integer;
  begin
    res := '';
    while source <> '' do begin
      case source[1] of
        '#':
          begin
            if copy(source, 2, 1) = '$' then begin
              s := '$';
              delete(source, 1, 2);
              charset := ['0'..'9', 'a'..'f', 'A'..'F'];
            end else begin
              delete(source, 1, 1);
              s := '';
              charset := ['0'..'9'];
            end;
            while (source <> '') and (ord(source[1])<=255) and (char(ord(source[1])) in charset) do begin
              s := s + source[1];
              delete(source, 1, 1);
            end;
            res := res + widechar(StrToInt(s));
            while (source<>'') and (ord(source[1])<=255) and (char(ord(source[1])) in whitespace) do delete (source,1,1);
            if (length(trim(source))>=2) and (copy(source,1,1)='+') then delete (source,1,1);
          end;
        '''':
          begin
            delete(source, 1, 1);
            while true do begin
              if source = '' then begin
                ShowError('Single quote detected - string starts but does not end');
                exit;
              end;
              if copy(source, 1, 1) = '''' then begin
                if copy(source, 2, 1) = '''' then begin
                  // Double quote detected
                  res := res + '''';
                  delete(source, 1, 2);
                end else begin
                  // End of text part detected
                  delete(source, 1, 1);
                  break;
                end
              end else begin
                res := res + copy(source, 1, 1);
                delete(source, 1, 1);
              end;
            end;
          end;
        'a'..'z','A'..'Z','_':
          begin
            constname:='';
            while (source<>'') and (ord(source[1])<=255) and (char(ord(source[1])) in ['a'..'z','A'..'Z','_','0'..'9']) do begin
              constname:=constname+source[1];
              delete (source,1,1);
            end;
            uconstname:=uppercase(constname);
            if constlist.Find(uconstname,idx) then begin
              res:=res+(constlist.Objects[idx] as TConst).value;
            end else
            if uconstname='CRLF' then begin
              res:=res+#10;
  //            if (resourcestringmode<>1) then ShowError(Format('CRLF substituted with #10 for %s. Consider to use sLineBreak instead.',[constname]));
            end else
            if uconstname='SLINEBREAK' then begin
              // Don't make a warning on this one because it is so common
              res:=res+#10;
            end else
            if uconstname='EOF' then begin
              // Don't make a warning on this one because it is so common
              res:=res+#26;
            end else
            if uconstname='EOL' then begin
              // Don't make a warning on this one because it is so common
              res:=res+#10;
            end else
            if (uconstname='DEPRECATED') or (uconstname='PLATFORM') or (uconstname='LIBRARY') then begin
              // The hinting directive was detected and ignored.
            end else
            begin
              if resourcestringmode=1 then // Don't handle consts that don't work
                break;
              ShowError(Format('Constant %s is not known.',[constname]));
            end;
          end;
      else
        break;
      end;
      while (source<>'') and (ord(source[1])<=255) and (char(ord(source[1])) in whitespace) do delete (source,1,1);
      if (length(trim(source))>=2) and (copy(source,1,1)='+') then delete (source,1,1);
      while (source<>'') and (ord(source[1])<=255) and (char(ord(source[1])) in whitespace) do delete (source,1,1);
    end;
  end;

  function readstring(var line: string; var src: TextFile): string;
  var
    s: string;
    pluscoming:boolean;
    i:integer;
    ansis:ansistring;
    found:boolean;
  begin
    Result := '';
    while true do begin
      if line='' then dxreadln(src, line);
      extractstring(line, s);
      Result := Result + s;
      line := trim(line);
      pluscoming:=(line='');
      if (line='+') or pluscoming then begin
        // This is a multi-line string
        dxreadln(src, line);
        line := trim(line);
        if pluscoming then begin
          if copy(line,1,1)='+' then begin
            delete (line,1,1);
            line:=trim(line);
          end else begin
            if resourcestringmode<>1 then
              ShowError ('This line is not connected with the previous line using a plus (+).');
            break;
          end;
        end;
      end else
        break;
    end;
    // Find out if there is just one character above 255
    found:=False;
    for i:=1 to length(Result) do begin
      if ord(Result[i])>=256 then begin
        found:=True;
        break;
      end;
    end;
    if not found then begin
      // Assume the string is not unicode, but the local character set.
      // Move all characters to an ansistring
      SetLength (ansis,length(Result));
      for i:=1 to length(Result) do
        ansis[i]:=AnsiChar(ord(Result[i]));
      // Convert from local character set to string
      Result:=StringToWidestring(ansis);
    end;
  end;

  function String2PO (s : string; Quoted : boolean = false) : string;
  // Converts a string to the syntax that is used in .po files
  var
    i: integer;
    c: Char;
    escnext:boolean;
  begin
    Result := '';
    escnext:=False;
    for i := 1 to length(s) do begin
      c := s[i];
      case c of
        #32..#33, #35..pred('\'),succ('\')..#65535:
          begin
            if escnext then Result:=Result+'\';
            Result := Result + c;
            escnext:=False;
          end;
        '\':begin
              Result:=Result+'\\';
              escnext:=False;
            end;
        #13:; // Do nothing
        #10:begin
              Result := Result + '\n';
              escnext:=False;
            end;
        #34:begin
              Result := Result + '\"';
              escnext:=False;
            end;
        #0:begin
             Result := Result + '\0';
             escnext:=True;
           end;
        #9:begin
             Result:=Result+'\t';
             escnext:=False;
           end;
      else
        Result := Result + '\x' + IntToHex(ord(c),2);
        escnext:=True;
      end;
    end;
    if Quoted then Result := '"' + Result + '"';
    end;

// I didn't have a Pascal parser available when this code was written.
var
  src : TextFile;
  line,s,constident,
  msgid : string;
  co : TConst;
  p,idx : integer;
  cp : word;
begin
  FreeListObjects(constlist);
  ConstList.Clear;
  FileMode:=fmOpenRead;
  if rgEncoding.ItemIndex=0 then cp:=cpLatin1 else cp:=cpUtf8;
  AssignFile(src,sourcefilename,cp);
  Reset(src);
  try
    lastcomment := '';
    resourcestringmode := 0;
    linenr := 0;
    while not eof(src) do begin
      dxreadln(src, line);
      line := trim(line);

      s := ConvertWhitespaceToSpaces (uppercase(line)) + ' ';

      // This should catch resourcestring start
      if (copy(s, 1, 15) = 'RESOURCESTRING ') then begin
        resourcestringmode := 2;
        delete (line,1,15);
      end;
      if (copy(s, 1, 6) = 'CONST ') then begin
        resourcestringmode := 1;
        delete (line,1,6);
      end;
      // This should catch all ends of resourcestring areas
      if (copy(s, 1, 9) = 'FUNCTION ') or (copy(s, 1, 10) = 'PROCEDURE ') or
        (copy(s, 1, 6) = 'BEGIN ') or (copy(s, 1, 4) = 'VAR ') or
        (copy(s, 1, 5) = 'TYPE ') or
        (copy(s, 1, 12) = 'CONSTRUCTOR ') or (copy(s, 1, 11) = 'DESTRUCTOR ') then
        resourcestringmode := 0;

      if resourcestringmode<>0 then begin
        while true do begin
          line:=trim(line);
          p := pos('''', line);
          if p = 0 then
            break;

          s:=trim(copy(line,1,p-1));
          if copy(s,length(s),1)='=' then begin
            // Identifier probably precedes the string
            s:=trim(copy(s,1,length(s)-1));
            if is_identifier(s) then
              constident:=s
            else
              constident:='';
          end;

          delete(line, 1, p - 1);
          // Extract the string
          msgid:=RemoveNuls(readstring(line, src));
          if resourcestringmode=2 then begin
//            if constident<>'' then begin
//              if lastcomment<>'' then
//                lastcomment:=lastcomment+sLinebreak;
//              lastcomment:=lastcomment+'Programmer''s name for it: '+constident;
//              end;
            if length(msgid)>0 then
              RsList.AddObject(constident,TTextInfo.Create(ExtractFilename(sourcefilename),String2PO(msgid)));
//            AddTranslation(defaultDomain, msgid, lastcomment, MakePathLinuxRelative(sourcefilename)+':'+IntToStr(linenr));
            lastcomment := '';
          end;
          if constident<>'' then begin
            if constlist.Find(uppercase(constident),idx) then begin
              co:=constlist.Objects[idx] as TConst;
            end else begin
              co:=TConst.Create;
              co.Name:=constident;
              constlist.AddObject(uppercase(co.name),co);
            end;
            co.Value:=msgid;
//
//            // If source-code comments for gnugettext enable it,
//            // extract the constant even though it is not a resourcestring.
//              if lastcomment <> '' then lastcomment := lastcomment + sLinebreak;
////              lastcomment := lastcomment + 'Programmer''s name for it: ' + constident;
////              AddTranslation (definedDomain, msgid, lastcomment, MakePathLinuxRelative(sourcefilename)+':'+IntToStr(linenr));
//              if length(msgid)>0 then
//                RsList.AddObject(constident,TTextInfo.Create(ExtractFilename(sourcefilename),msgid));
//              lastcomment := '';
            end;

          // Check what comes next in the line
          if copy(line, 1, 1) <> ';' then begin
            // First parameter is line number, second is the contents of the line
            if resourcestringmode=2 then
              ShowError('resourcestring does not end in semicolon.');
            line:='';
            break;
          end else begin
            // If it ended with a semicolon, analyze the rest of the line as if it was a new line
            delete(line, 1, 1);
          end;
        end;
      end { if resourcestringmode };
    end;
  finally
    CloseFile(src);
    end;
  end;

{ ------------------------------------------------------------------- }
procedure TfrmMain.bbExitClick(Sender: TObject);
begin
  Close;
  end;

procedure TfrmMain.bbInfoClick(Sender: TObject);
begin
  InfoDialog(ProgVersName+' - '+ProgVersDate+#13+CopRgt+#13'E-Mail: '+EmailAdr);
  end;

procedure TfrmMain.edEditCloseUp(Sender: TObject);
begin
  with edEdit do begin
    EdFile:=HistoryList[ItemIndex];
    LoadEditFile;
    end;
  end;

procedure TfrmMain.edDirCloseUp(Sender: TObject);
begin
  with edDir do begin
    PasDir:=HistoryList[ItemIndex];
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
    if Execute then with edEdit do begin
      EdFile:=Filename;
      Text:=Filename; AddItem(Filename);
      Result:=true;
      end
    else Result:=false;
    end;
  end;

procedure TfrmMain.btDirClick(Sender: TObject);
var
  s : string;
begin
  s:=PasDir;
  if ShellDirDialog.Execute(_('Select directory with pas sources'),false,true,true,'',s) then begin
    with edDir do begin
      AddItem(s); ItemIndex:=0;
      end;
    PasDir:=s;
    end;
  end;

procedure TfrmMain.btnHelpClick(Sender: TObject);
begin
  ShowHelp('tools.html#frompas');
  end;

procedure TfrmMain.sbEditClick(Sender: TObject);
begin
  if SelectEdit then LoadEditFile;
  end;

function TfrmMain.LoadEditFile : boolean;
begin
  Result:=false;
  if FileExists(EdFile) then EdList.LoadFromFile(EdFile)
  else InfoDialog(Format(_('File not found: %s'),[EdFile]));
  end;

procedure TfrmMain.bbSaveClick(Sender: TObject);
var
  pe  : TPoEntry;
  sn,s,sdir  : string;
  DirInfo    : TSearchRec;
  Findresult : integer;
  n,k        : integer;

  function GetProgrammerName(ucl : TstringList) : string;
  var
    i : integer;
    s : string;
  begin
    Result:='';
    with ucl do for i:=0 to Count-1 do begin
      s:=Strings[i];
      if AnsiContainsText(s,PrgNamId) then begin
        ReadNxtStr(s,':'); // overread
        Result:=Trim(s);
        Break;
        end;
      end;
    end;

  function RecodeMsgStr(s : string) : string;
  var
    i : integer;
  begin
    i:=1;
    while i<=length(s) do begin
      if copy(s,i,2)='\n' then begin
        delete (s,i,1);
        s[i]:=#10;
      end else
      if copy(s,i,2)='\r' then begin
        delete (s,i,1);
        s[i]:=#13;
      end else
      if copy(s,i,2)='\t' then begin
        delete (s,i,1);
        s[i]:=#9;
      end else
      if copy(s,i,2)='\f' then begin
        delete (s,i,1);
        s[i]:=#26;
      end else
      if s[i]='\' then begin
        case s[i+1] of
          'n': s[i+1] := #10;
          'r': s[i+1] := #13;
          't': s[i+1] := #9;
        end;
        delete (s,i,1);
      end else
      if s[i]='"' then begin
        delete (s,i,maxint);
      end;
      inc (i);
      end;
    Result:=s;
    end;

begin
  sdir:=IncludeTrailingPathDelimiter(PasDir);
  FindResult:=FindFirst (sdir+'*.'+PasExt,faAnyFile,DirInfo);
  while (FindResult=0) do begin
    n:=RsList.Count;
    StatusBar.SimpleText:=DirInfo.Name+': '+IntToStr(RsList.Count-n)+' '+_('strings');
    Application.ProcessMessages;
    ExtractFromPascal(sdir+DirInfo.Name);
    FindResult:=FindNext(DirInfo)
    end;
  FindClose(DirInfo);
  k:=0;
  with EdList do begin
    pe:=FindFirst;
    while pe<>nil do begin
      s:=GetProgrammerName(pe.AutoCommentList);
      if length(s)>0 then n:=RsList.IndexOf(s) else n:=-1;
      if n>=0 then with pe do if (length(MsgStr)=0) or cbOverwrite.Checked then  begin
        pe.MsgStr:=RecodeMsgStr((RsList.Objects[n] as TTextInfo).FText);
        inc(k);
        end;
      pe:=FindNext(pe);
      end;
    end;
  StatusBar.SimpleText:=Format(_('%u strings replaced'),[k]);
  Application.ProcessMessages;
  sn:=EdFile+'.tmp';
  EdList.SaveToFile(sn);
  s:=EdFile+'.bak';
  if FileExists(s) then DeleteFile(s);
  RenameFile(EdFile,s);
  RenameFile(sn,EdFile);
  end;

end.
