(* Tool for GnuGetText for Delphi
   Copy translated strings from po file to Pascal unit

   © Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.

   Note: The routines ExtractStrings, ExtractFromPascal and ExtractFromDFM
   originate from xgettexttools:
      (C) Copyright by Lars B. Dybdahl and Jens Berke
      E-mail: Lars@dybdahl.dk, phone +45 70201241
      You may distribute and modify this file as you wish for free

   Feb. 2021
   last modified: April 2024
   *)

unit PoToDfmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons,
  Vcl.ComCtrls, PoParser;

const
  Vers = ' - Vers. 1.0';

  GgtName = 'GnuGetText';
  GgtDummy = 'GgtDummy';

type
  TMainForm = class(TForm)
    Label2: TLabel;
    bbExit: TBitBtn;
    bbInfo: TBitBtn;
    OpenDialog: TOpenDialog;
    Label1: TLabel;
    Label3: TLabel;
    lvStrings: TListView;
    bbReplace: TBitBtn;
    edLang: TEdit;
    Label4: TLabel;
    edPasFile: TEdit;
    StatusBar: TStatusBar;
    bbUnit: TBitBtn;
    bbPoFile: TBitBtn;
    btnHelp: TBitBtn;
    edPoFile: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure bbPoFileClick(Sender: TObject);
    procedure bbUnitClick(Sender: TObject);
    procedure bbExitClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure bbReplaceClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure edLangChange(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure bbInfoClick(Sender: TObject);
  private
    { Private-Deklarationen }
    ProgVersName,
    ProgVersDate,
    AppPath,UserPath,
    IniName,ProgPath,
    PoFile,
    PasFile,DfmFile    : string;
    PoList             : TPoEntryList;
    constlist          : TStringList;   // List of consts. Strings are names, Objects are TConst
    resourcestringmode : Integer;  // 0=None, 1=Const, 2=Resourcestring
    function LoadPoFile : boolean;
    procedure FindTranslations;
    procedure ClearConstList;
    procedure extractstring(var source: string; var res: string);
    procedure AddEntry(aValue:string; IsPas : boolean = false);
    procedure ExtractStrings (UnitName: string);
    procedure ExtractFromPascal (sourcefilename: string);
    procedure ExtractFromDFM (sourcefilename: string);
  public
    { Public-Deklarationen }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses System.IniFiles, System.StrUtils, System.Math, GnuGetText, InitProg, PathUtils,
  WinUtils, ListUtils, xgettexttools, MsgDialogs, ShellFileDlg, GgtConsts, GgtUtils;

const
  (* INI-Sections *)
  PoSekt = 'PoFiles';

  (* INI-Variables *)
  iniPoFile = 'PoFile';
  iniUnit = 'LastUnit';

type
  TConst= class
    name,value : string;
    end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  IniFile  : TMemIniFile;
begin
  TranslateComponent (self);
  Application.Title:=_('Copy translated strings from po file to Pascal unit');
  InitPaths(AppPath,UserPath,ProgPath);
  InitVersion(Application.Title,Vers,CopRgt,3,3,ProgVersName,ProgVersDate);
  Caption:=ProgVersName;
  IniName:=Erweiter(AppPath,PrgName,IniExt);
  IniFile:=TMemIniFile.Create(IniName);
  with IniFile do begin
    Top:=ReadInteger(CfGSekt,iniTop,Top);
    Left:=ReadInteger(CfGSekt,iniLeft,Left);
    PoFile:=ReadString(CfGSekt,iniPoFile,'');
    PasFile:=ReadString(CfGSekt,iniUnit,'');
    LoadHistory(IniFile,PoSekt,edPoFile);
    Free;
    end;
  AddToHistory(edPoFile,PoFile);
  edPasFile.Text:=PasFile;
  PoList:=TPoEntryList.Create;
  constlist:=TStringList.Create;
  with constlist do begin
    Sorted:=True; Duplicates:=dupError; CaseSensitive:=True;
    end;
  end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  if FileExists(PoFile) then LoadPoFile else edPoFile.Text:='';
  if FileExists(PasFile) then begin
    ExtractStrings(PasFile);
    FindTranslations;
    end
  else edPasFile.Text:='';
  end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
  IniFile  : TMemIniFile;
begin
  IniFile:=TMemIniFile.Create(IniName);
  with IniFile do begin
    WriteInteger(CfGSekt,iniTop,Top);
    WriteInteger(CfGSekt,iniLeft,Left);
    WriteString(CfGSekt,iniPoFile,PoFile);
    WriteString(CfGSekt,iniUnit,PasFile);
    SaveHistory(IniFile,PoSekt,edPoFile);
    UpdateFile;
    Free;
    end;
  try HtmlHelp(0,nil,HH_CLOSE_ALL,0); except end;
  end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  PoList.Free;
  ClearConstList; FreeAndNil (constlist);
  end;

procedure TMainForm.bbUnitClick(Sender: TObject);
var
  sd : string;
begin
  if length(PasFile)>0 then sd:=ExtractFilePath(PasFile)
  else sd:=UserPath;
  if ShellFileDialog.Execute(_('Select unit'),_('pas files|*.'+PasExt),sd,UserPath,
      OpenDialog.Options,PasFile) then begin
//  with OpenDialog do begin
//    if length(PasFile)>0 then InitialDir:=ExtractFilePath(PasFile)
//    else InitialDir:=UserPath;
//    Filename:=ExtractFilename(PasFile);
//    Title:=_('Select unit');
//    Filter:=_('pas files|*.'+PasExt);
//    if Execute then with edPasFile do begin
//      PasFile:=FileName; Text:=Filename;
      edPasFile.Text:=PasFile;
      ExtractStrings(PasFile);
      FindTranslations;
    end;
  end;

procedure TMainForm.btnHelpClick(Sender: TObject);
begin
  ShowHelp('tools.html#transunit');
  end;

procedure TMainForm.ExtractStrings (UnitName: string);
begin
  DfmFile:=NewExt(PasFile,DfmExt);
  lvStrings.Clear;
  ExtractFromDFM(DfmFile);
  ExtractFromPascal(PasFile);
  end;

procedure TMainForm.bbPoFileClick(Sender: TObject);
begin
  with OpenDialog do begin
    if length(PoFile)>0 then InitialDir:=ExtractFilePath(PoFile)
    else InitialDir:=UserPath;
    Filename:='';
    Title:=_('Select po file');
    Filter:=Format(_('po files|*.%s|all|*.*'),[PoExt]);
    if Execute then begin
      PoFile:=FileName;
      AddToHistory(edPoFile,PoFile);
      LoadPoFile;
      end
    end;
  end;

function TMainForm.LoadPoFile : boolean;
var
  s  : string;
  ne : integer;
begin
  Result:=false;
  if FileExists(PoFile) then begin
    ne:=PoList.LoadFromFile(PoFile);
    if ne>0 then
      ErrorDialog(Format(_('Error in line %u of po file!'),[ne]))
    else begin
      s:=ExtractLastDir(ExtractFilePath(PoFile));
      with edLang do if length(s)>0 then Text:=s else Text:='temp';
      end;
    end
  else InfoDialog(Format(_('File not found: %s'),[PoFile]));
  end;

procedure TMainForm.bbExitClick(Sender: TObject);
begin
  Close;
  end;

procedure TMainForm.bbInfoClick(Sender: TObject);
begin
  InfoDialog(ProgVersName+' - '+ProgVersDate+#13+CopRgt
           +#13'E-Mail: '+EmailAdr);
  end;

procedure TMainForm.FindTranslations;
var
  i : integer;
  pe : TPoEntry;
begin
  with lvStrings do for i:=0 to Items.Count-1 do with Items[i] do begin
    pe:=PoList.FindEntry(Caption);
    if assigned(pe) then begin
      SubItems.Add(pe.MsgStr);
      Checked:=true;
      end;
    end;
  end;

procedure TMainForm.bbReplaceClick(Sender: TObject);
var
  i : integer;
  s,sd,ss : string;
  n  : integer;

  function CvChar (c : char) : string;
  begin
    Result:='#'+IntToStr(ord(c));
    end;

  function ProcessPasString (const s : string) : string;
  begin
    Result:=ReplaceStr(s,#$A,'''+sLineBreak+''');
    end;

  function ProcessDfmString (const s : string) : string;
  var
    chmd : boolean;
    i    : integer;
  begin
    chmd:=true; Result:='';
    for i:=1 to length(s) do begin
      if word(s[i])<128 then begin
        if chmd then Result:=Result+s[i] else Result:=Result+''''+s[i];
        chmd:=true;
        end
      else begin
        if chmd then Result:=Result+''''+CvChar(s[i]) else Result:=Result+CvChar(s[i]);
        chmd:=false;
        end;
      end;
    end;

begin
  n:=0;
  with TStringList.Create do begin
    LoadFromFile(PasFile);
    i:=0;
    while i<Count do begin
      s:=TrimRight(Strings[i]);
      if AnsiEndsText('+sLineBreak+',s) and (i<Count-1) then begin
        Strings[i]:=s+TrimLeft(Strings[i+1]);
        Delete(i+1);
        end
      else inc(i);
      end;
    sd:=Text;
    Free;
    end;
  sd:=ReplaceText(sd,GgtName,GgtDummy);
  with lvStrings do for i:=0 to Items.Count-1 do with Items[i] do
      if Checked and (integer(Data)=1) then begin
    sd:=ReplaceStr(sd,''''+ProcessPasString(Caption)+'''',''''+ProcessPasString(SubItems[1])+'''');
    inc(n);
    end;
  ss:=AddPath(ExtractFilePath(PasFile),edLang.Text);
  ForceDirectories(ss);
  s:=AddPath(ss,ExtractFileName(PasFile));
//  RenameFile(DfmFile,DfmFile+'.bak');
  with TStringList.Create do begin
    Text:=sd;
    SaveToFile(s);
    Free;
    end;
  StatusBar.SimpleText:=Format(_('File: "%s" processed'),[s]);
  if FileExists(DfmFile) then begin
    with TStringList.Create do begin
      LoadFromFile(DfmFile);
      sd:=Text;
      Free;
      end;
    with lvStrings do for i:=0 to Items.Count-1 do with Items[i] do
        if Checked and (integer(Data)=0) then begin
      sd:=ReplaceStr(sd,''''+Caption+'''',''''+ProcessDfmString(SubItems[1])+'''');
      inc(n);
      end;
    s:=AddPath(ss,ExtractFileName(DfmFile));
  //  RenameFile(DfmFile,DfmFile+'.bak');
    with TStringList.Create do begin
      Text:=sd;
      SaveToFile(s);
      Free;
      end;
    end
  else Errordialog(Format(_('File: "%s" not found'),[DfmFile]));
  StatusBar.SimpleText:=Format(_('%u strings replaced by translation'),[n]);
  end;

procedure TMainForm.ClearConstList;
begin
  with constlist do while Count<>0 do begin
    Objects[0].Free; Delete (0);
    end;
  end;

procedure TMainForm.edLangChange(Sender: TObject);
begin
  bbReplace.Hint:=('Replace selected strings and save unit in subdirectory')+' "'+edLang.Text+'"';
  end;

procedure TMainForm.AddEntry(aValue : string; IsPas : boolean);
const
  ft : array[boolean] of string =('D','P');
begin
  if length(aValue)>0 then with lvStrings.Items.Add do begin
    Caption:=aValue;
    Data:=pointer(IsPas);
    SubItems.Add(ft[IsPas]);
    end;
  end;

procedure TMainForm.extractstring(var source: string; var res: string);
const whitespace=[#0..#32];
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
//                Warning (wtSyntaxError,_('Single quote detected - string starts but does not end'));
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

procedure TMainForm.ExtractFromPascal(sourcefilename: string);
// I didn't have a Pascal parser available when this code was written.
var
  src: TextFile;
  line, uline:string;
  s:string;
  msgid: string;
  p, p2, idx:Integer;
  domain: string;
  co:TConst;
  constident:string;
  idlength,idoffset:integer;
  idplural:boolean;

  LastLineRead:string;
  linenr:Integer;
  commentmode:string; // Empty means that dxreadln is not inside a comment
  lastcomment:string;

  procedure dxreadln(var src: TextFile; var line: string);
  var
    i:integer;

    procedure cutuntil (endtag:string);
    var p:integer;
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
      if line='' then
        dxreadln(src, line);
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


begin
  ClearConstList;
  FileMode:=fmOpenRead;
  AssignFile(src, sourcefilename);
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
            if constident<>'' then begin
              if lastcomment<>'' then
                lastcomment:=lastcomment+sLinebreak;
              lastcomment:=lastcomment+'Programmer''s name for it: '+constident;
            end;
            AddEntry(msgid,true);
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
          end;

          // Check what comes next in the line
          if copy(line, 1, 1) <> ';' then begin
            // First parameter is line number, second is the contents of the line
            line:='';
            break;
          end else begin
            // If it ended with a semicolon, analyze the rest of the line as if it was a new line
            delete(line, 1, 1);
          end;
        end;
      end else begin
        // Check for occurence of gettext()
        while true do begin
          uline:=uppercase(line);
          p:=poscode('_',uline);
          p2:=poscode('GETTEXT', uline);
          if p=0 then begin
            p:=p2;
          end else
            if p2<>0 then
              p:=min(p,p2);
          if p=0 then
            break;
          if (poscode('FUNCTION',uline)<>0) or
             (poscode('PROCEDURE',uline)<>0) then
            break;

          domain := 'default';
          idoffset:=0;
          if copy(uline,p,1)='_' then begin
            idlength:=1;
            idplural:=False;
          end else begin
            idlength:=7;
            if uppercase(copy(line, p - 1, 1)) = 'D' then begin
              domain := '';
              idlength:=8;
              idoffset:=-1;
            end;
            if uppercase(copy(line, p - 2, 2)) = 'DC' then begin
              domain := '';
              idlength:=9;
              idoffset:=-2;
            end;
            idplural:=False;
            if uppercase(copy(line, p - 2, 2)) = 'DN' then begin
              domain := '';
              idlength:=9;
              idoffset:=-2;
              idplural:=True;
            end else
            if uppercase(copy(line, p - 1, 1)) = 'N' then begin
              idlength:=8;
              idoffset:=-1;
              idplural:=True;
            end;
          end;
          if ((p+idoffset=1) or (not ((ord(uline[p+idoffset-1])<=255) and (char(ord(uline[p+idoffset-1])) in ['a'..'z','A'..'Z','_','0'..'9'])))) and
              (length(line)>=p+idlength+idoffset) and (not ((ord(uline[p+idoffset+idlength])<=255) and (char(ord(uline[p+idoffset+idlength])) in ['a'..'z','A'..'Z','_','0'..'9']))) then begin
            line := trim(copy(line, p + idlength+idoffset, maxint));
            if copy(line, 1, 1) = '(' then begin
              line := trim(copy(line, 2, maxint));
              if domain = '' then begin
                // get first parameter
                extractstring(line, domain);
                line := trim(line);
                if copy(line, 1, 1) = ',' then begin
                  delete(line, 1, 1);
                  line:=trim(line);
                end;
              end;

              // Get parameter that contains the msgid
              msgid := RemoveNuls(readstring(line, src));
              if idplural then begin
                line := trim(line);
                if copy(line, 1, 1) = ',' then begin
                  delete(line, 1, 1);
                  line:=trim(line);
                end;
                if line='' then
                  dxreadln(src, line);
                msgid := msgid+#0+RemoveNuls(readstring(line,src));
              end;
              AddEntry(msgid,true);
              lastcomment := '';
            end { if a parenthesis is found };
          end else begin
            line := trim(copy(line, p + idlength+idoffset, maxint));
          end { if it looks like a function call identifier };
        end { loop that finds several occurences in the same line };
      end { if resourcestringmode };
    end;
  finally
    CloseFile(src);
  end;
end;

procedure TMainForm.ExtractFromDFM(sourcefilename: string);
var
  src: TStream;
  mem: TMemoryStream;
  line, lastline:string;
  s: string;
  i:integer;
  indent: integer;
  p, linenr: integer;
  scope: TStringList;
  propertyname: string;
  multilinevalue: boolean;
  mvalue: string;
  p1, p2, p3: integer;
  pClassname: integer;
  c:char;
  classnamepart: string;
  linechar:AnsiString;
  currentclassname: string;
  classnames: TStringList;
  instancenames: TStringList;
  collectionlevel:integer; // will be increased which each occurence of a collection, in order to recognize nested collections
  collectionpropertyname:string; // will be the propertyname of the highest-level collection property

begin
  src:=TFileStream.Create(sourcefilename,fmOpenRead);
  try
    // Check for empty file
    if src.Read(c,1)=0 then
      exit;
    // Check for binary dfm file
    src.Seek(0, soFromBeginning);
    if c=#$FF then begin
      // Convert the file into text form in a memory stream
      mem:=TMemoryStream.Create;
      ObjectResourceToText(src,mem);
      FreeAndNil (src);
      src:=mem;
    end;
    src.Seek(0,soFrombeginning);

    scope := TStringList.Create;
    classnames := TStringlist.Create;
    instancenames := TStringlist.Create;
    try
      classnames.Add(''); // we need that one because "indent" might start with 0
      instancenames.Add('');
      linenr := 0;
      line := '';
      propertyname := '';
      collectionpropertyname := '';
      multilinevalue := false;
      collectionlevel := 0;
      while true do begin
        // Get next line and check it out
        lastline := line;
        if not StreamReadln (src, line) then break;
        inc(linenr);
        indent := measureindent(line);
        line := trim(line);
        if line='' then continue;  // *** ABORT IF LINE IS EMPTY ***

        // Check if a collection starts or ends in this line.
        // If we have nested collections, the nesting-level
        // will be remembered
        if RightStr(line, 3) = '= <' then
          inc(collectionlevel);
        if RightStr(lowercase(line), 4) = 'end>' then begin
          dec(collectionlevel);
          if collectionlevel = 0 then
            collectionpropertyname := '';
        end;

        // Always adjust the count of "classnames" to the current "indent"
        // and make sure, the a bigger indent gets the same classname as the
        // smaller indent before. This will be overwritten as soon as we reach
        // an line containing "object", "inherited" or "inline", like this:
        //
        // object Form1: TForm      indent = 0, classname[0] = 'TForm'
        //  Caption = 'Form1'       indent = 1, classname[1] = 'TForm'
        //  object Edit1: TEdit     indent = 1, classname[1] = 'TEdit'
        //   Left = 1;              indent = 2, classname[2] = 'TEdit'
        while indent < classnames.Count-1 do begin
          classnames.Delete(classnames.Count-1);
          instancenames.Delete(instancenames.Count-1);
        end;
        while indent > classnames.Count-1 do begin
          classnames.Add(classnames[classnames.Count-1]);
          instancenames.Add(instancenames[instancenames.Count-1]);
        end;

        // check for occurence of a classname and remember it at the current indention.
        // Take into account that some properties might contain identifiers as part
        // of their name, e.g. "InlineSkaterCount" or "InheritedFromGrandPa"
        if (Pos(':', line) > 0) and ((Pos('object ', lowercase(line)) > 0) or (Pos('inherited ', lowercase(line)) > 0) or (Pos('inline ', lowercase(line)) > 0)) then begin
          pClassname := Pos(':', line);
          if pClassname > 0 then begin
            currentclassname := '';
            classnamepart := Trim(Copy(line, pClassname+1, Length(line)-pClassname+1));
            for i := 1 to Length(classnamepart) do begin
              // be aware of constructs like "TScrollbox [0]" or other unlikely things, simply just get only the chars that are valid for classnames
              linechar := UTF8Encode(classnamepart[i]);
              if (Length(linechar) > 1) or (not (linechar[1] in ['a'..'z','A'..'Z','_','0'..'9'])) then
                break
              else
                currentclassname := currentclassname + classnamepart[i];
            end;
            classnames[indent] := currentclassname;
            // remember the name of instance of that class as well in the same way
            p := Pos(' ', line);
            instancenames[indent] := Copy(line, p +1, pClassname -p -1);
          end;
        end;

        // Check for changes in scope
        if (indent < scope.Count) and multilinevalue then begin
          multilinevalue := false;
          AddEntry(mvalue);
          scope.Delete(scope.count - 1);
        end;
        while indent < scope.Count do begin
          scope.Delete(scope.count - 1);
        end;

        if indent > scope.Count then begin
          p := pos(' ', lastline);
          if p = 0 then s := lastline else s := copy(lastline, p + 1, maxint);
          p := pos(':', s);
          multilinevalue := true;
          mvalue := '';
          if p = 0 then s := '' else s := copy(s, 1, p - 1);
        end;
        while indent > scope.Count do begin
          scope.Add(s);
          s := '';
        end;

        // Analyze the line
        p := pos(' =', line);
        p1 := pos('''', line);
        p2 := pos('#', line);
        if p1 = 0 then p1 := maxint;
        if p2 = 0 then p2 := maxint;
        p3 := min(p1, p2);

        // Extract property name if the line contains such one
        if (p <> 0) and (p < p3) then begin
          propertyname := trim(copy(line, 1, p - 1));
          // is we're in a collection (and it's the highest level if there are nested collections), remember the property name of that collection
          if (collectionlevel = 1) and (collectionpropertyname = '') then
            collectionpropertyname := propertyname;
          multilinevalue := false;
        end;

        // Extract string, if the line contains such one
        if p3 <> maxint then begin
          delete(line, 1, p3 - 1);
          extractstring(line, s);
          if multilinevalue then begin
            mvalue := mvalue + s;
            if trim(line) <> '+' then begin
              AddEntry(mvalue);
              mvalue:='';
            end;
          end else begin
            AddEntry(s);
          end;
        end;
      end;
    finally
      FreeAndNil(scope);
      FreeAndNil(classnames);
    end;
  finally
    FreeAndNil (src);
  end;
end;


end.
