(****************************************************************)
(*                                                              *)
(*  (C) Copyright by Lars B. Dybdahl                            *)
(*  E-mail: Lars@dybdahl.dk, phone +45 70201241                 *)
(*  You received this file under the Mozilla Public License 1.1 *)
(*                                                              *)
(*  See http://dybdahl.dk/dxgettext/ for more information       *)
(*                                                              *)
(****************************************************************)

// converted to unicode strings (Delphi XE2) and other enhancements
// J. Rathlev kontakt(a)rathlev-home.de - 2012-06-29 / 2014-02-11 / 2023-08-16

// TPoParser.ReadNxtEntry can read from any coded textfile (e.g. UTF-8 or ISO-8859-1)
// just by specifying the needed codepage on assigning the filename from the
// calling program
// uses always UTF-8 encoding for writing the template

// Added: TPoHeader - new record containing the po file header infos
//        Optional saving of file entries in original order
//        Find algorithm using SoundEx to find similar translations
// last modified: October 2024

unit PoParser;

interface

uses
  System.Classes;

const
  UcBom = #$FEFF;
  cpAnsi = 850;
  cpLatin1 = 1252;
  cpUtf8 = 65001;

  HistMarker = '#*';
  defSimMeasure = 5;     // length for SoundEx
  defId = 'PACKAGE VERSION';

type
  TPoHeaderIds = (hiProjectId,hiCreationDate,hiRevisionDate,hiLastTranslator,hiLanguageTeam,
    hiLanguage,hiMIMEVersion,hiContentType,hiContentEncoding,hiPluralForms,hiXGenerator,hiXSourceCharset);

const
  HeaderIds : array[TPoHeaderIds] of string = ('Project-Id-Version:','POT-Creation-Date:',
    'PO-Revision-Date:','Last-Translator:','Language-Team:','Language:',
    'MIME-Version:','Content-Type:','Content-Transfer-Encoding:',
    'Plural-Forms:','X-Generator:','X-Poedit-SourceCharset:');

type
  TPoHeader = record
    private
      FHeaderItems : array[TPoHeaderIds] of string;
      function GetHeaderEntry (Index : TPoHeaderIds) : string;
      procedure SetHeaderEntry (Index : TPoHeaderIds; const Val : string);
    public
      procedure GetFromString (hs : string);
      function ToString : string;
      property Items[Index : TPoHeaderIds] : string read GetHeaderEntry write SetHeaderEntry; default;
    end;

  TPoEntry = class
    public
      UserCommentList : TStringList;   // Entire lines, unicode!
      AutoCommentList : TStringList;   // Entire lines, unicode!
      HistCommentList : TStringList;   // Entire lines, unicode!
      IdEntry,IdLine  : integer;       // original number of entry, linenumber JR
      MsgId  : string;                 // singular and plural are separated by #0, if plural form is present
      MsgStr : string;                 // plural forms are separated by #0, if present
      Merged,                          // Entry was merged
      Fuzzy  : boolean;                // If true, msgstr is not the translation, but just a proposal for a translation
      constructor Create;
      destructor Destroy; override;
      procedure Assign (po : TPoEntry);
      procedure Clear;
      procedure WriteToStream (str : TStream; WriteComments : boolean = true);  // Adds an empty line afterwards.
    end;

  TPoEntryList = class        // read and write utf-8
    private
      list : TStringList;    // Strings are searchkeys, objects are TList of TPoEntries
      SoundList,             // list SopundEx expressions
      CiList : TStringList;  // case insensitive list
      FSimMeasure : integer;
      FHeader : TPoHeader;
      FLoaded,
      HdChanged : boolean;  // header was changed
      function GetSearchKey (const MsgId : string) : string;
      function GetCount : integer;   // JR
      function GetHeader (Index : TPoHeaderIds) : string;
      procedure SetHeader (Index : TPoHeaderIds; const Val : string);
      function GetItem (Index : integer) : TPoEntry;
      procedure BuildHeader (hs : string);
    public
      constructor Create;
      destructor Destroy; override;
      function LoadFromFile (const filename : string) : integer;
      procedure SaveToFile (const filename : string; OrgOrder : boolean = false);
      procedure Clear;
      function FindEntry (const MsgId : string) : TPoEntry;
      function IsEntry (const MsgId : string) : boolean;
      function FindNoCaseEntry (const MsgId : string) : TPoEntry;
      function FindSoundEntry (const MsgId : string) : TPoEntry;
//      function DeleteEntry (MsgId : string) : boolean;  // True if found and deleted, false if not found
      procedure AddEntry (entry : TPoEntry); // Will fail if MsgId exists. Entry is copied.
      procedure UpdateHeader (pe : TPoEntry);

      // Iterate through all items. When nil is returned, no more elements are there.
      function FindFirst : TPoEntry;
      function FindNext (po : TPoEntry) : TPoEntry;
      property Loaded : boolean read FLoaded;
      property TotalEntries : integer read GetCount;  // JR
      property SimMeasure : integer read FSimMeasure write FSimMeasure;
      property Header[Index : TPoHeaderIds] : string read GetHeader write SetHeader;
      property Items[Index: Integer] : TPoEntry read GetItem; default;
    end;

  // Easy to use parser for .po files. Just put all the lines into AddLine(),
  // and each time a translation has been found, it turns true once.
  // Always end your parsing by putting an empty line into Addline.
  TPoParser = class
    private
      HistCount,         // JR
      LineNumber : Integer;
      IsMsgId:boolean;
      entry : TPoEntry;  // This is the return value if last AddLine returned True
      entryhasdata : boolean;
    public
      constructor Create;
      destructor Destroy; override;

      // Put all your lines into AddLine(). It will return nil most
      // of the times, but it will return an entry each time the whitespace
      // after an entry has been reached. The entry is only valid until the next
      // call to AddLine().
      function AddLine (line : string) : TPoEntry;

      // Read a couple of lines from file and return next TPoEntry. Returns nil if no more entries.
      function ReadNextEntry (var tf : TextFile) : TPoEntry;
      property CurrentLineNumber:integer read LineNumber;
    end;

function GetUTCOffset : string;
function CurrentTimestamp : string;

function String2PO (s : string; Quoted : boolean = true) : string;
function FindBestBreak (const s : string;LineWidth:integer):integer;

procedure StreamWrite (s : TStream; const line : string);
procedure StreamWriteln (s   : TStream; const line : string='');
procedure StreamWriteMinimumPoHeader (s : TStream; const appname : string);
procedure StreamWriteDefaultPoTemplateHeader (s : TStream; const appname,name : string);


implementation

uses
  System.Math, System.SysUtils, System.StrUtils, System.DateUtils, Winapi.Windows, gnugettext;

{ ---------------------------------------------------------------- }
function GetUTCOffset : string;
begin
  with TTimeZone.Local.UtcOffset do begin
    Result:=Format('%.4d',[100*Hours+Minutes]);
    if not AnsiStartsStr('-',Result) then Result:='+'+Result;
    end;
  end;

function CurrentTimestamp : string;
begin
  Result:=FormatDateTime('yyyy-mm-dd hh:nn',now)+GetUTCOffset;
  end;

{ ---------------------------------------------------------------- }
procedure StreamWriteMinimumPoHeader (s : TStream; const appname : string);
begin
  StreamWriteln(s, '#, fuzzy');
  StreamWriteln(s, 'msgid ""');
  StreamWriteln(s, 'msgstr ""');
  StreamWriteln(s, '"'+HeaderIds[hiCreationDate]+' '+CurrentTimestamp+'\n"');
  StreamWriteln(s, '"'+HeaderIds[hiRevisionDate]+' '+CurrentTimestamp+'\n"');
  StreamWriteln(s, '"'+HeaderIds[hiLastTranslator]+' '+'Somebody <your.email@address.com>\n"');
  StreamWriteln(s, '"'+HeaderIds[hiMIMEVersion]+' 1.0\n"');
  StreamWriteln(s, '"'+HeaderIds[hiContentType]+' text/plain; charset=UTF-8\n"');
  StreamWriteln(s, '"'+HeaderIds[hiContentEncoding]+' 8bit\n"');
  StreamWriteln(s, '"'+HeaderIds[hiXGenerator]+' '+appname + '\n"');
  Streamwriteln(s, '');
end;

procedure StreamWriteDefaultPoTemplateHeader (s : TStream; const appname,name : string);
var
  sn : string;
begin
  StreamWriteln(s, '# SOME DESCRIPTIVE TITLE.');
  StreamWriteln(s, '# Copyright (C) YEAR THE PACKAGE''S COPYRIGHT HOLDER');
  StreamWriteln(s, '# This file is distributed under the same license as the PACKAGE package.');
  StreamWriteln(s, '# FIRST AUTHOR <EMAIL@ADDRESS>, YEAR.');
  StreamWriteln(s, '# ');
  StreamWriteln(s, '#, fuzzy');
  StreamWriteln(s, 'msgid ""');
  StreamWriteln(s, 'msgstr ""');
  if length(name)=0 then sn:=defId else sn:=name;
  StreamWriteln(s, '"'+HeaderIds[hiProjectId]+' '+sn+'\n"');
  StreamWriteln(s, '"'+HeaderIds[hiCreationDate]+' '+CurrentTimestamp+'\n"');
  StreamWriteln(s, '"'+HeaderIds[hiRevisionDate]+' '+CurrentTimestamp+'\n"');
  StreamWriteln(s, '"'+HeaderIds[hiLastTranslator]+' '+'Somebody <your.email@address.com>\n"');
  StreamWriteln(s, '"'+HeaderIds[hiMIMEVersion]+' 1.0\n"');
  StreamWriteln(s, '"'+HeaderIds[hiContentType]+' text/plain; charset=UTF-8\n"');
  StreamWriteln(s, '"'+HeaderIds[hiContentEncoding]+' 8bit\n"');
  StreamWriteln(s, '"'+HeaderIds[hiXGenerator]+' '+appname + '\n"');
  Streamwriteln(s, '');
end;

procedure StreamWriteln (s : TStream; const line : string='');
begin
  StreamWrite (s, line);
  StreamWrite (s, sLineBreak);
  end;

procedure StreamWrite (s : TStream; const line : string);
var
  len : integer;
  su  : utf8string;
begin
  su:=utf8encode(line);       // use always UTF-8 encoding for template
  len:=length(su);
  if (len>0) and (s.Write(su[1],len)<>len) then
      raise Exception.Create (_('Error when writing to stream.'));
  end;

function String2PO (s : string; Quoted : boolean = true) : string;
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

{ ---------------------------------------------------------------- }
function TPoHeader.GetHeaderEntry (Index : TPoHeaderIds) : string;
begin
  Result:=FHeaderItems[Index];
  end;

procedure TPoHeader.SetHeaderEntry (Index : TPoHeaderIds; const Val : string);
begin
  if val<>FHeaderItems[Index] then FHeaderItems[Index]:=val;
  end;

procedure TPoHeader.GetFromString (hs : string);

  function ReadNxtStr (var s : String) : string;
  var
    i : integer;
  begin
    if length(s)>0 then begin
      i:=pos (#10,s);
      if i=0 then i:=succ(length(s));
      Result:=copy(s,1,pred(i));
      System.delete(s,1,i);
      end
    else Result:='';
    end;

var
  s : string;
  id : TPoHeaderIds;
begin
  for id:=Low(TPoHeaderIds) to High(TPoHeaderIds) do FHeaderItems[id]:='';
  repeat
    s:=ReadNxtStr(hs);
    for id:=Low(TPoHeaderIds) to High(TPoHeaderIds) do begin
      if AnsiStartsText(HeaderIds[id],s) then
        FHeaderItems[id]:=Trim(AnsiRightStr(s,length(s)-length(HeaderIds[id])));
      end;
    until hs.IsEmpty;
  end;

function TPoHeader.ToString : string;
var
  id : TPoHeaderIds;
begin
  Result:='';
  for id:=Low(TPoHeaderIds) to High(TPoHeaderIds) do if not FHeaderItems[id].IsEmpty then
    Result:=Result+HeaderIds[id]+' '+FHeaderItems[id]+#10;
  end;

{ ---------------------------------------------------------------- }
{ TPoParser }

constructor TPoParser.Create;
begin
  inherited;
  LineNumber:=0; HistCount:=0;
  entry:=TPoEntry.Create;
  end;

destructor TPoParser.Destroy;
begin
  FreeAndNil (entry);
  inherited;
  end;

function TPoParser.AddLine(line: string) : TPoEntry;
var
  i:integer;
  value : string;
begin
  Result:=nil;
  try
    Inc (LineNumber);
    line:=trim(line);
    if line<>'' then begin
      if not entryhasdata then begin
        entry.Clear;
        entryhasdata:=False;
      end;
      if copy(line,1,2)='#,' then begin
        if lowercase(trim(line))='#, fuzzy' then
          entry.Fuzzy:=True
      end else
      if copy(line,1,2)='# ' then
        entry.UserCommentList.Add(line)
      else
      if copy(line,1,2)='#~' then begin
        with entry do if length(MsgId)=0 then begin
          inc(HistCount);
          MsgId:=HistMarker+IntToStr(HistCount);
          end;
        entry.HistCommentList.Add(line);
        end
      else
      if copy(line,1,1)='#' then
        entry.AutoCommentList.Add(line)
      else begin
        if uppercase(copy(line,1,12))='MSGID_PLURAL' then begin
          IsMsgId:=True;
          delete (line,1,12);
          line:=trim(line);
          entry.MsgId:=entry.MsgId+#0;
        end;
        if uppercase(copy(line,1,5))='MSGID' then begin
          IsMsgId:=True;
          entry.IdLine:=LineNumber;
          delete (line,1,5);
          line:=trim(line);
        end;
        if uppercase(copy(line,1,6))='MSGSTR' then begin
          IsMsgId:=False;
          delete (line,1,6);
          if copy(line,1,1)='[' then begin
            if copy(line,2,1)<>'0' then
              entry.MsgStr:=entry.MsgStr+#0;
            delete (line,1,3);
          end;
          line:=trim(line);
        end;
        if (copy(line,1,1)<>'"') or (copy(line,length(line),1)<>'"') then
          raise Exception.Create (Format(_('Illegal line: %s'),[line]));
        value:=copy(line,2,length(line)-2);
        i:=1;
        while i<length(value) do begin
          if value[i]='\' then begin
            delete (value,i,1);
            case value[i] of
              '0':value[i]:=#0;
              'n':value[i]:=#10;
              't':value[i]:=#9;
              'x':begin
                    value[i]:=Char(StrToInt('$'+copy(value,i+1,2)));
                    delete (value,i+1,2);
                  end;
            else
              // Do nothing - the character was just escaped.
            end;
          end;
          inc (i);
        end;
        if IsMsgId then entry.MsgId:=entry.MsgId+value
                   else entry.MsgStr:=entry.MsgStr+value;
      end;
    end;
    if (line='') and entryhasdata then begin
      Result:=entry;
    end else begin
      Result:=nil;
    end;
    entryhasdata:=line<>'';
  except
    on e:Exception do
      raise Exception.Create (format(_('Exception %s in line %d:'+SLineBreak+'%s'),[e.ClassName,LineNumber,e.Message]));
  end;
end;

function TPoParser.ReadNextEntry(var tf: TextFile): TPoEntry;
var
  line: String;
begin
  while not eof(tf) do begin
    Readln (tf, line);
    if copy(line,1,1)=UcBom then delete(line,1,1);
    Result:=AddLine(line);
    if Result<>nil then exit;
    end;
  Result:=AddLine ('');
  end;


{ TPoEntry }

constructor TPoEntry.Create;
begin
  inherited;
  UserCommentList:=TStringList.Create;
  AutoCommentList:=TStringList.Create;
  HistCommentList:=TStringList.Create;
  IdEntry:=-1; Merged:=false;
  end;

destructor TPoEntry.Destroy;
begin
  FreeAndNil (UserCommentList);
  FreeAndNil (AutoCommentList);
  FreeAndNil (HistCommentList);
  inherited;
  end;

procedure TPoEntry.Assign(po: TPoEntry);
begin
  UserCommentList.Assign(po.UserCommentList);
  AutoCommentList.Assign(po.AutoCommentList);
  HistCommentList.Assign((po.HistCommentList));
  IdEntry:=po.IdEntry;
  IdLine:=po.IdLine;
  MsgId:=po.MsgId;
  MsgStr:=po.MsgStr;
  Fuzzy:=po.Fuzzy;
  Merged:=po.Merged;
  end;

procedure TPoEntry.Clear;
begin
  UserCommentList.Clear;
  AutoCommentList.Clear;
  HistCommentList.Clear;
  MsgId:='';
  MsgStr:='';
  IdEntry:=-1;
  IdLine:=0;
  Fuzzy:=False;
  Merged:=false;
  end;

function FindBestBreak (const s : string; LineWidth : integer) : integer;
// Returns number of characters to include in the line
var
  spacepos:integer;
  i,p:integer;
  MaxLength:integer;
begin
  p:=pos(#10,s);
  spacepos:=0;
  MaxLength:=min(length(s),LineWidth);
  if (p>2) and (p<MaxLength) then begin
    Result:=p;
    exit;
    end;
  i:=MaxLength;
  while i>=1 do begin
    case s[i] of
      #10:begin
            Result:=i;
            exit;
          end;
      ' ':
        if spacepos=0 then
          spacepos:=i;
      end;
    dec (i);
    end;
  if spacepos>LineWidth div 2 then begin
    Result:=spacepos;
  end else
    Result:=MaxLength;
  if (Result>=2) and (ord(s[Result])<32) and (ord(s[Result])<>10) then begin
    for i:=Result-1 downto 1 do begin
      if (ord(s[i])>=32) or (ord(s[i])=10) then begin
        Result:=i;
        exit;
        end;
      end;
    end;
  end;

procedure TPoEntry.WriteToStream(str: TStream; WriteComments : boolean);

  procedure WritePart (const token : string; msg : string; StartEmpty : boolean = false);
  var
    p:integer;
    part : string;
  begin
    StreamWrite (str, token+' ');
    if StartEmpty then StreamWriteln (str,'""');
    repeat
      p:=FindBestBreak (msg,70);
      part:=copy(msg,1,p);
      delete (msg,1,length(part));
      StreamWriteln (str, String2PO(part));
      until length(msg)=0;
    end;

var
  s : string;
  p:integer;
  idx:integer;
  isplural:boolean;
begin
  if WriteComments then begin
    // Write comments
    s:=trim(UserCommentList.Text);
    if s<>'' then StreamWrite (str, s+sLineBreak);
    s:=trim(AutoCommentList.Text);
    if s<>'' then StreamWrite (str, s+sLineBreak);
    end;

  // Fuzzy?
  if Fuzzy then StreamWriteln(str, '#, fuzzy');

  if copy(MsgId,1,2)=HistMarker then begin   // history comments - JR
    s:=trim(HistCommentList.Text);
    if s<>'' then StreamWrite (str, s+sLineBreak);
    end
  else begin
    // Write msgid and msgstr
    p:=pos(#0,MsgId);
    isplural:=p<>0;
    if not isplural then
      WritePart ('msgid',MsgId)
    else begin
      WritePart ('msgid',copy(MsgId,1,p-1));
      WritePart ('msgid_plural',copy(MsgId,p+1,maxint));
    end;
    p:=pos(#0,MsgStr);
    if (p=0) and (not isplural) then WritePart ('msgstr',MsgStr,length(MsgId)=0)
    else begin
      idx:=0;
      while true do begin
        if p<>0 then begin
          WritePart ('msgstr['+IntToStr(idx)+']',copy(MsgStr,1,p-1));
          delete (MsgStr,1,p);
        end else begin
          WritePart ('msgstr['+IntToStr(idx)+']',MsgStr);
          break;
        end;
        inc (idx);
        p:=pos(#0,MsgStr);
        end;
      end;
    end;
  // Write empty line
  StreamWrite (str, sLineBreak);
  end;

{ TPoEntryList }

constructor TPoEntryList.Create;
begin
  inherited Create;
  list:=TStringList.Create;
  with list do begin
    Duplicates:=dupError;
    CaseSensitive:=True;
    Sorted:=true;
    end;
  CiList:=TStringList.Create;
  with CiList do begin       // case insensitive duplicate
    Duplicates:=dupIgnore;
    Sorted:=true;
    end;
  SoundList:=TStringList.Create;
  with SoundList do begin       // soundex duplicate
    Duplicates:=dupIgnore;
    Sorted:=true;
    end;
  HdChanged:=false; FLoaded:=false; FSimMeasure:=defSimMeasure;
  end;

procedure TPoEntryList.Clear;
var
  i,j:integer;
  l : TList;
begin
  for i:=0 to list.count-1 do begin
    l:=list.Objects[i] as TList;
    for j:=0 to l.count-1 do TObject(l.Items[j]).Free;
    l.Free;
    end;
  list.Clear; CiList.Clear;
  end;

destructor TPoEntryList.Destroy;
begin
  Clear;
  FreeAndNil (list); FreeAndNil (CiList); FreeAndNil (SoundList);
  inherited;
  end;

function TPoEntryList.GetHeader (Index : TPoHeaderIds) : string;
begin
  Result:=FHeader[Index];
  end;

procedure TPoEntryList.SetHeader (Index : TPoHeaderIds; const Val : string);
begin
  if val<>FHeader[Index] then begin
    FHeader[Index]:=val;
    HdChanged:=true;
    end;
  end;

procedure TPoEntryList.UpdateHeader (pe : TPoEntry);
begin
  pe.MsgStr:=FHeader.ToString;
  HdChanged:=false;
  end;

procedure TPoEntryList.BuildHeader (hs : string);
begin
  FHeader.GetFromString(hs);
  end;

procedure TPoEntryList.AddEntry (entry: TPoEntry);
var
  p:Integer;
  l : TList;
  idx:integer;
  po : TPoEntry;
  searchkey : string;
begin
  with entry do if IdEntry<0 then IdEntry:=TotalEntries+1;
  searchkey:=GetSearchKey(entry.MsgId);
  if searchkey.IsEmpty then BuildHeader(entry.MsgStr);
  if list.Find(searchkey,idx) then begin
    l:=list.Objects[idx] as TList;
    for p:=0 to l.count-1 do begin
      po:=TObject(l.Items[p]) as TPoEntry;
      if po.MsgId=entry.MsgId then
        raise Exception.Create (Format(_('This list of translations cannot handle MsgId duplicates. Please remove the duplicate of "%s".'),[po.MsgId]));
      end;
    end
  else begin
    l:=TList.Create;
    list.AddObject(searchkey,l);
    end;
  po:=TPoEntry.Create;
  po.Assign (entry);
  l.Add(po);
  end;

//function TPoEntryList.DeleteEntry (MsgId: string): boolean;
//var
//  p:Integer;
//  l : TList;
//  idx:integer;
//  po : TPoEntry;
//begin
//  Result:=False;
//  if list.Find(GetSearchKey(MsgId),idx) then begin
//    l:=list.Objects[idx] as TList;
//    for p:=0 to l.count-1 do begin
//      po:=TObject(l.Items[p]) as TPoEntry;
//      if po.MsgId=MsgId then begin
//        po.Free;
//        l.Delete (p);
//        Result:=True;
//        if l.Count=0 then begin
//          l.Free;
//          list.Delete (idx);
//          end;
//        exit;
//        end;
//      end;
//    end;
//  end;

function TPoEntryList.FindEntry (const MsgId : string) : TPoEntry;
var
  p :Integer;
  l : TList;
  idx : integer;
begin
  if list.Find(GetSearchKey(MsgId),idx) then begin
    l:=list.Objects[idx] as TList;
    for p:=0 to l.count-1 do begin
      Result:=TObject(l.Items[p]) as TPoEntry;
      if Result.MsgId=MsgId then exit;
      end;
    end;
  Result:=nil;
  end;

function TPoEntryList.IsEntry (const MsgId : string) : boolean;
begin
  Result:=FindEntry(MsgId)<>nil;
  end;

function TPoEntryList.FindNoCaseEntry (const MsgId : string) : TPoEntry;
var
  idx,p : integer;
  l : TList;
begin
  if CiList.Find(GetSearchKey(MsgId),idx) then begin
    idx:=integer(CiList.Objects[idx]);
    l:=list.Objects[idx] as TList;
    for p:=0 to l.count-1 do begin
      Result:=TObject(l.Items[p]) as TPoEntry;
      if AnsiSameText(Result.MsgId,MsgId) then exit;
      end;
    end;
  Result:=nil;
  end;

function TPoEntryList.FindSoundEntry (const MsgId : string) : TPoEntry;
var
  idx,p : integer;
  l : TList;
begin
  if SoundList.Find(SoundEx(MsgId,FSimMeasure),idx) then begin
    idx:=integer(SoundList.Objects[idx]);
    l:=list.Objects[idx] as TList;
    for p:=0 to l.count-1 do begin
      Result:=TObject(l.Items[p]) as TPoEntry;
      if length(Result.MsgStr)>0 then exit;
      end;
    end;
  Result:=nil;
  end;

function TPoEntryList.FindFirst: TPoEntry;
var
  l : TList;
begin
  if list.Count=0 then begin
    Result:=nil;
    exit;
    end;
  l:=list.Objects[0] as TList;
  if l.Count=0 then
    raise Exception.Create (_('Internal error in TPoEntryList data structure. Sublist for searchkey was empty.'));
  Result:=TObject(l.Items[0]) as TPoEntry;
  end;

function TPoEntryList.FindNext(po: TPoEntry): TPoEntry;
var
  p:Integer;
  l : TList;
  idx:integer;
begin
  Result:=Nil;
  if list.Find(GetSearchKey(po.MsgId),idx) then begin
    l:=list.Objects[idx] as TList;
    p:=l.IndexOf(po);
    if p=-1 then
      raise Exception.Create (_('Error: Specified TPoEntry was not found in list.'));
    if p=l.Count-1 then begin
      if idx=list.Count-1 then Result:=nil
      else begin
        l:=list.Objects[idx+1] as TList;
        if l.Count=0 then
          raise Exception.Create (_('Internal error in TPoEntryList data structure. Sublist for searchkey was empty.'));
        Result:=TObject(l.Items[0]) as TPoEntry;
        end;
      end
    else Result:=TObject(l.Items[p+1]) as TPoEntry;
    end;
  end;

function TPoEntryList.GetSearchKey(const MsgId : string) : string;
var
  p:integer;
begin
  p:=pos(#10,MsgId);
  if p<>0 then Result:=copy(MsgId,1,p-1)
  else Result:=MsgId;
  end;

function TPoEntryList.GetCount : integer;
var
  i : integer;
begin
  Result:=0;
  with list do for i:=0 to Count-1 do
    if not Items[i].MsgId.IsEmpty then Result:=Result+(Objects[i] as TList).Count;
  end;

function TPoEntryList.GetItem (Index : integer) : TPoEntry;
var
  l : TList;
begin
  with list do if (Index>=0) and (Index<Count) then begin
    l:=list.Objects[Index] as TList;
    if l.Count=0 then
      raise Exception.Create (_('Internal error in TPoEntryList data structure. Sublist for searchkey was empty.'));
    Result:=TObject(l.Items[0]) as TPoEntry;
    end
  else Result:=nil;
  end;

function TPoEntryList.LoadFromFile(const filename: string) : integer;
var
  tf  : TextFile;
  pop : TPoParser;
  pe  : TPoEntry;
  ne,i : integer;
begin
  Clear; Result:=0; ne:=0;
  FileMode:=fmOpenRead;
  AssignFile (tf,filename,cpUtf8);   // read utf-8
  try
    try
      Reset (tf);
    except
      ne:=2;
      end;
    pop:=TPoParser.Create;
    try
      while true do begin
        try
          pe:=pop.ReadNextEntry(tf);
        except
          Result:=pop.CurrentLineNumber;
          pe:=nil;
          end;
        if pe=nil then break;
        pe.IdEntry:=ne;
        AddEntry (pe);
        inc(ne);
        end;
    finally
      FreeAndNil (pop);
      end;
  finally
    CloseFile (tf);
    end;
  FLoaded:=Result=0;
  if FLoaded then begin
//    for i:=0 to list.Count-1 do CiList.AddObject(list[i],pointer(i));
    for i:=0 to list.Count-1 do SoundList.AddObject(SoundEx(list[i],FSimMeasure),pointer(i));
    end;
  end;

procedure TPoEntryList.SaveToFile(const filename : string; OrgOrder : boolean);
var
  outfile : TFileStream;
  pe : TPoEntry;
  i,n : integer;
  pel : TList;

  function CompareEntries(Item1, Item2: Pointer): Integer;
  begin
    Result:=CompareValue(TPoEntry(Item1).IdEntry,TPoEntry(Item2).IdEntry);
    end;

begin
  outfile:=TFileStream.Create (filename, fmCreate);
  try
    // Write header
//    StreamWriteDefaultPoTemplateHeader(outfile,appname);
    pe:=FindEntry('');
    if pe<>nil then begin
      if HdChanged then UpdateHeader(pe);
      pe.WriteToStream(outfile);
      end;
    // Write entries
    pel:=TList.Create;
    pe:=FindFirst;
    while pe<>nil do begin
      if (pe.MsgId<>'') and (copy(pe.MsgId,1,2)<>'##') then pel.Add(pe);
        // pe.WriteToStream(outfile);
      pe:=FindNext (pe);
      end;
    if OrgOrder then pel.Sort(@CompareEntries);  // original order of entries
    with pel do for i:=0 to Count-1 do TPoEntry(Items[i]).WriteToStream(outfile);
    pel.Free;
    // Write history comments
    pe:=FindFirst;
    while pe<>nil do begin
      if (pe.MsgId<>'') and (copy(pe.MsgId,1,2)='##') then pe.WriteToStream(outfile);
      pe:=FindNext (pe);
      end;
  finally
    FreeAndNil (outfile);
    end;
  end;

end.

