unit xgettext;
(****************************************************************)
(*                                                              *)
(*  (C) Copyright by Lars B. Dybdahl, Jens Berke and            *)
(*        Jacques Garcia Vazquez                                *)
(*  E-mail: Lars@dybdahl.dk, phone +45 70201241                 *)
(*  You received this file under the Mozilla Public License 1.1 *)
(*                                                              *)
(*  See http://dybdahl.dk/dxgettext/ for more information       *)
(*                                                              *)
(****************************************************************)

// changes for Windows 7: J. Rathlev, Nov. 2009
// changes for Windows 10, J. Rathlev, May 2016
//   removed C,CPP parts from source code
// changes to include c/cpp again, J. Rathlev, July 2023
//   requires XGetText.exe Windows binary from
//   https://mlocati.github.io/articles/gettext-iconv-windows.html
//   removed: ExtractFromEXE, linux sections, casesentive lists
//   added: Load list with left most characters of possible unrenamed objects (IgnoreObjects.txt)
// last modified: October 2025

interface

uses
  System.Classes, poparser;

type
  {TXExcludeFormClassProperties: represents 1..n properties of a certain class
   that shall be excluded from string extraction in form files. }
  TXExcludeFormClassProperties = class(TCollectionItem)
  private
    FProperties: TStringList;
    FNameOfClass: String;
    procedure SetNameOfClass(const Value: String);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    function ExcludeFormClassProperty(const aPropertyname : String): boolean;
    procedure AddFormClassProperty(const aPropertyname : String);
    property NameOfClass : String read FNameOfClass write SetNameOfClass; // "Classname" already used by TObject => needed other name
  end;

  {TXExcludeFormClassPropertyList: represents a collection of
   TXExcludeFormClassProperties}
  TXExcludeFormClassPropertyList = class(TCollection)
  private
    function GetItems(Index: integer): TXExcludeFormClassProperties;
    procedure SetItem(Index: integer;
      const Value: TXExcludeFormClassProperties);
    function Add: TXExcludeFormClassProperties;
    function AddFormClass(const aClassname: String): TXExcludeFormClassProperties;
  public
    function FindItem(const aClassname : String): TXExcludeFormClassProperties;
    function ExcludeFormClassProperty(const aClassname,aPropertyname : String): Boolean;
    function AddFormClassProperty(const aClassPropertyname : String): TXExcludeFormClassProperties;
    property Items[Index: integer]: TXExcludeFormClassProperties read GetItems write SetItem; default;
  end;

  {TXExcludes: holds all information about what shall be excluded from string
   extraction, specified in a "ggexclude.cfg" file }
  TXExcludes = class(TObject)
  private
    FFormClasses: TStringList;
    FFormInstances: TStringList;
    FDirectories: TStringList;
    FFiles: TStringList;
    FBaseDirectory: String;
    FExcludeFormClassPropertyList: TXExcludeFormClassPropertyList;
    FLastErrorMsg: string;
    function GetFullInternalPath(const s : String) : String;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function AddDirectory(const aDirectory: String): boolean;
    function AddFormFile(const aFilename: String): boolean;
    function AddFormClass(const aClassname: String): boolean;
    function AddFormClassProperty(const aPropertyname: String): boolean;
    function AddFormInstance(const aInstanceName : String): boolean;
    function ExcludeDirectory(const aDirectory: String): Boolean;
    function ExcludeFormFile(const aFilename: String): Boolean;
    function ExcludeFormClass(const aClassname: String): Boolean;
    function ExcludeFormClassProperty(const aClassname,aPropertyname : String): Boolean; overload;
    function ExcludeFormClassProperty(const aClassname : String): Boolean; overload;
    function ExcludeFormInstance(const aFilename,aInstanceName : String): boolean;
    function FormClassHasWildcard(const aClassname : String): Boolean;
    property BaseDirectory: String read FBaseDirectory write FBaseDirectory;
    property LastErrorMsg: string read FLastErrorMsg;
    end;

  TWarningType = (wtGenericWarning, wtUnexpectedException, wtConstantReplaced,
     wtSyntaxError, wtParameterError, wtExtendedDirectiveError, wtNonAscii,
     wtNotImplemented, wtExcludeFile);

  TOnOverwrite = Procedure (sender: TObject; const aFileName: string; var Overwrite: boolean) of object;
  TOnProgress = procedure (const CurrentTask,CurrentFileName : string; LineNumber:Integer) of object;
  TOnWarning = procedure (WarningType:TWarningType;const Msg,Line,Filename : string;LineNumber:Integer) of object;

  TXGTDomain = class
    public
      msgid : TStringList;  // Sorted for binary lookups, objects are TItem
      order : TStringList;  // same as msgid, but sorted after occurence. Points to the same objects as msgid, so don't free them!
      constructor Create;
      destructor Destroy; override;
    end;

  TXGetText = class
    private
      ignorelist : TPoEntryList;
      domainlist : TStringList; // Strings are domain name, values are TXGTDomain
      constlist : TStringList;   // List of consts. Strings are names, Objects are TConst
      IgnoreMarkers : TStringList; // List with left most characters of possible unrenamed objects
      CFiles:TStringList;   // This will contain filenames of C/C++ source files to be scanned elsewhere
      definedDomain: String;
      FCanceled : boolean;
      procedure doHandleExtendedDirective (line: string);
      procedure ClearConstList;
      function GetDomain(domain: string): TXGTDomain;
      procedure AddTranslation(const Domain,msgid,Comments,Location : string);
      function ReadHeader (const spo : string; HdList : TStringList) : boolean;
      procedure WriteHeader (const spo : string; HdList : TStringList);
      function WriteAll(const domain: string) : boolean;
      function MakePathRelative(const path: string): string;
      procedure DoProgress (const CurrentTask, CurrentFileName: string; LineNumber: Integer);
    private
      resourcestringmode : Integer;  // 0=None, 1=Const, 2=Resourcestring
      CurrentFilename,
      fIgnoreListFile,
      LastLineRead : string;
      linenr : Integer;
      commentmode : string; // Empty means that dxreadln is not inside a comment
      lastcomment : string;
      BaseDirectoryList : TStringList; // Always ends in a pathdelimiter
      BaseDirectory : String;
      Excludes : TXExcludes;
      ExcludeDirs : TStringList;   // JR
      function MakePoFileName (const domain : string) : string;
      procedure Warning (WarningType : TWarningType; const msg : string); overload;
      procedure Warning (WarningType : TWarningType; const Msg,Line,Filename : string; LineNumber : Integer); overload;
      procedure dxreadln (var src : TextFile; var line : string); // same as system.readln, but takes care of comments
      procedure extractstring(var source : string;var res: string);
      function readstring(var line: string; var src: TextFile): string; // Reads a pascal ansistring constant
      procedure ExtractFromPascal(const sourcefilename: string);
      procedure ExtractFromDFM(const sourcefilename: string);
      procedure ExtractFromRC(const sourcefilename: string);
      procedure ExtractFromCppFile(const XFilename,CFilename : string);
      procedure ExtractFromFile(const sourcefilename: string);
      procedure ExtractFromFileMasks(const mask: string);
      procedure ParseExcludeFile;
      procedure SetIgnoreListFile (const FileName : string);
    public
      // When set, only default domain is written to a file, and this file has it's filename from this variable
      SingleOutputFilename : String;
      ExePath : string;
      CodePage : integer;
      OnProgress:TOnProgress;
      OnWarning:TOnWarning;
      Recurse:boolean;
      UpdateIgnore:boolean;  // Will create ignore.po if not exists, and put obvious untranslatable items into it
      UseIgnoreFile:boolean; // Will make sure that no item from ignore.po is stored in other files
      AllowNonAscii:boolean;
      OrderbyMsgid:boolean;
      NoWildcards:boolean;
      defaultDomain : string;
      filemasks:TStringList;
      DestinationPath : string;
      Generator: string;
      OnOverwrite: TOnOverwrite;
      constructor Create (const AGenerator : string = 'Delphi Get Text');
      destructor Destroy; override;
      procedure AddBaseDirectory (path : string);
      procedure SetExcludeDirs(const AList: string);
      procedure AddDelphiFilemasks;
      procedure AddLazarusFilemasks;
      procedure AddCppFilemasks;
      procedure HandleIgnores;
      function Execute : boolean;
      procedure Cancel;
      property IgnoreListFile : string read fIgnoreListFile write SetIgnoreListFile;
    end;

implementation

uses
  Winapi.Windows, ExecuteApp, GgtConsts,
  System.SysUtils, System.Math, gnugettext, xgettexttools, System.Masks,
  ignoredetector, System.StrUtils, ExtSysUtils, PathUtils;

type
  TConst = class
    name : string;
    value : string;
    end;

  EGetText = class (Exception)
    end;

const
  cDefineDirective  = '{gnugettext:'; // Start of an option inside the source code
  cScanOption       = 'scan-all';     // Means that all strings in the source code should be extracted
  cDomainDefinition = 'text-domain';  // Changes default text domain for strings
  cScanResetOption  = 'reset';        // Changes back to default behaviour

  { consts for exclusion of files, directories, properties and classes from extraction: }
  cExcludeFormInstance = 'exclude-form-instance';
  cExcludeFormClassProperty = 'exclude-form-class-property';
  cExcludeFormClass = 'exclude-form-class';
  cExcludeFile = 'exclude-file';
  cExcludeDir = 'exclude-dir';

  PoExt = '.po';
  PoIgnore = 'ignore.po';

function RemoveNuls (const s : string) : string;
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

{ TXGTDomain }

constructor TXGTDomain.Create;
begin
  inherited Create;
  msgid:=TStringList.Create;
  order:=TStringList.Create;
  msgid.Sorted:=True;
  msgid.Duplicates:=dupAccept;
//  msgid.CaseSensitive:=True;
  end;

destructor TXGTDomain.Destroy;
begin
  while msgid.count<>0 do begin
    msgid.Objects[0].Free;
    msgid.Delete (0);
    end;
  FreeAndNil (msgid);
  FreeAndNil (order);
  inherited;
  end;

constructor TXGetText.Create (const AGenerator : string);
begin
  inherited Create;
  ignorelist:=TPoEntryList.Create;
  BaseDirectoryList:=TStringList.create;
  filemasks:=TStringList.Create;
  with filemasks do begin
    Sorted:=True;
    Duplicates:=dupIgnore;
    end;
  domainlist:=TStringList.Create;
  with domainlist do begin
    Sorted:=True;
    Duplicates:=dupError;
//    CaseSensitive:=True;
    end;
  constlist:=TStringList.Create;
  with constlist do begin
    Sorted:=True;
    Duplicates:=dupError;
//    CaseSensitive:=True;
    end;
  IgnoreListFile:='';
  IgnoreMarkers:=nil;
  CFiles:=TStringList.Create;
  with CFiles do begin
    Duplicates:=dupError;
//    CaseSensitive:=True;
    Sorted:=True;
    end;
  OrderbyMsgid:=false;
  CodePage:=0;
  Excludes:=TXExcludes.Create;
  ExcludeDirs:=TStringList.Create; // JR
  defaultDomain:='default';
  Generator:=AGenerator; //Format(_('dxgettext %s'),[GgtVersion])   // JR
  FCanceled:=false;
  end;

destructor TXGetText.Destroy;
begin
  ClearConstList;
  FreeAndNil (constlist);
  while domainlist.Count <> 0 do begin
    domainlist.Objects[0].Free;
    domainlist.Delete(0);
    end;
  FreeAndNil(domainlist);
  FreeAndNil(BaseDirectoryList);
  FreeAndNil(filemasks);
  if assigned(IgnoreMarkers) then FreeAndNil(IgnoreMarkers); // JR
  FreeAndNil(CFiles);
  FreeAndNil(ignorelist);
  FreeAndNil(Excludes);
  FreeAndNil(ExcludeDirs); // JR
  inherited;
  end;

procedure TXGetText.SetIgnoreListFile (const FileName : string);
begin
  if not Filename.IsEmpty and FileExists(FileName) then begin  // JR
    fIgnoreListFile:=Filename;
    if assigned(IgnoreMarkers) then IgnoreMarkers.Free;
    IgnoreMarkers:= TStringList.Create;
    IgnoreMarkers.LoadFromFile(fIgnoreListFile);
    end;
  end;

procedure TXGetText.extractstring(var source: string; var res: string);
const
  whitespace : TSysCharSet = [#0..#32];
// Extracts the Pascal coded string at the beginning of source.
// Returns the result in res.
// Removes the extracted data from source.
var
  charset : TSysCharSet;
  s: string;
  constname,uconstname : string;
  idx:integer;
begin
  res:='';
  while source <> '' do begin
    case source[1] of
      '#':
        begin
          if copy(source, 2, 1) = '$' then begin
            delete(source, 1, 2); s:='$';
            charset:=['0'..'9', 'a'..'f', 'A'..'F'];
            end
          else begin
            delete(source, 1, 1); s:='';
            charset:=['0'..'9'];
            end;
          while (source <> '') and (ord(source[1])<=255) and CharInSet(source[1],charset) do begin
            s:=s + source[1];
            delete(source, 1, 1);
            end;
          res:=res+char(StrToInt(s));
          while (source<>'') and (ord(source[1])<=255) and CharInSet(source[1],whitespace) do delete (source,1,1);
          if (length(trim(source))>=2) and (copy(source,1,1)='+') then delete (source,1,1);
          end;
      '''':
        begin
          delete(source, 1, 1);
          while true do begin
            if source = '' then begin
//              Warning (wtSyntaxError,_('Single quote detected - string starts but does not end'));
              exit;
              end;
            if copy(source, 1, 1) = '''' then begin
              if copy(source, 2, 1) = '''' then begin
                // Double quote detected
                res:=res + '''';
                delete(source, 1, 2);
                end
              else begin
                // End of text part detected
                delete(source, 1, 1);
                break;
                end
              end
            else begin
              res:=res + copy(source, 1, 1);
              delete(source, 1, 1);
              end;
            end;
          end;
      'a'..'z','A'..'Z','_':
        begin
          constname:='';
          while (source<>'') and (ord(source[1])<=255) and CharInSet(source[1],['a'..'z','A'..'Z','_','0'..'9']) do begin
            constname:=constname+source[1];
            delete (source,1,1);
            end;
          uconstname:=uppercase(constname);
          if constlist.Find(uconstname,idx) then begin
            res:=res+(constlist.Objects[idx] as TConst).value;
            end
          else if uconstname='CRLF' then begin
            res:=res+#10;
            if (resourcestringmode<>1) then
//              Warning (wtConstantReplaced,Format(_('CRLF substituted with #10 for %s - consider to use sLineBreak instead'),[constname]));
            end
          else if uconstname='SLINEBREAK' then begin
            // Don't make a warning on this one because it is so common
            res:=res+#10;
            end
          else if uconstname='EOF' then begin
            // Don't make a warning on this one because it is so common
            res:=res+#26;
            end
          else if uconstname='EOL' then begin
            // Don't make a warning on this one because it is so common
            res:=res+#10;
            end
          else if (uconstname='DEPRECATED') or (uconstname='PLATFORM') or (uconstname='LIBRARY') then begin
            // The hinting directive was detected and ignored.
            end
          else begin
            if resourcestringmode=1 then // Don't handle consts that don't work
              break;
//            Warning (wtGenericWarning,Format(_('Constant %s is not known'),[constname]));
            end;
          end;
      else break;
      end;
    while (source<>'') and (ord(source[1])<=255) and CharInSet(source[1],whitespace) do delete (source,1,1);
    if (length(trim(source))>=2) and (copy(source,1,1)='+') then delete (source,1,1);
    while (source<>'') and (ord(source[1])<=255) and CharInSet(source[1],whitespace) do delete (source,1,1);
    end;
  end;

function TXGetText.readstring(var line: string; var src: TextFile): string;
var
  s: string;
  pluscoming:boolean;
  i:integer;
  ansis:ansistring;
  found:boolean;
begin
  Result:='';
  while true do begin
    if line='' then dxreadln(src, line);
    extractstring(line, s);
    Result:=Result + s;
    line:=trim(line);
    pluscoming:=(line='');
    if (line='+') or pluscoming then begin
      // This is a multi-line string
      dxreadln(src, line);
      line:=trim(line);
      if pluscoming then begin
        if copy(line,1,1)='+' then begin
          delete (line,1,1);
          line:=trim(line);
          end
        else begin
          if resourcestringmode<>1 then
            Warning (wtSyntaxError,_('This line is not connected with the previous line using a plus (+)'));
          break;
          end;
        end;
      end
    else break;
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

function TXGetText.MakePathRelative (const path : string) : string;
var
  baselen:integer;
begin
  baselen:=length(BaseDirectory);
  if AnsiSameText(copy(path,1,baselen),BaseDirectory) then Result:=copy(path,baselen+1,maxint)
  else Result:=copy(path,3,maxint);
  end;

procedure TXGetText.ExtractFromPascal(const sourcefilename : string);
// I didn't have a Pascal parser available when this code was written.
var
  src: TextFile;
  line, uline : string;
  s : string;
  msgid: string;
  p, p2, idx:Integer;
  domain: string;
  co:TConst;
  constident : string;
  idlength,idoffset:integer;
  idplural:boolean;
begin
  if MatchesMask(extractfilename(sourcefilename),'gnugettext*.pas') then exit;
//  if AnsiSameText(extractfilename(sourcefilename),'gnugettext.pas') then exit;
//  if AnsiSameText(extractfilename(sourcefilename),'gnugettextd5.pas') then exit;
  ClearConstList;
  FileMode:=fmOpenRead;
  if CodePage>0 then AssignFile(src,sourcefilename,CodePage)
  else AssignFile(src,sourcefilename);
  Reset(src);
  try
    definedDomain:='';
    lastcomment:='';
    resourcestringmode:=0;
    linenr:=0;
    while not eof(src) do begin
      dxreadln(src, line);
      line:=trim(line);
      if (linenr>10) and (linenr mod 100 =0) then begin
        DoProgress('','',linenr);
        sleep(1);
        end;

      s:=ConvertWhitespaceToSpaces (uppercase(line)) + ' ';

      // This should catch resourcestring start
      if (copy(s, 1, 15) = 'RESOURCESTRING ') then begin
        resourcestringmode:=2;
        delete (line,1,15);
        end;
      if (copy(s, 1, 6) = 'CONST ') then begin
        resourcestringmode:=1;
        delete (line,1,6);
        end;
      // This should catch all ends of resourcestring areas
      if (copy(s, 1, 9) = 'FUNCTION ') or (copy(s, 1, 10) = 'PROCEDURE ') or
        (copy(s, 1, 6) = 'BEGIN ') or (copy(s, 1, 4) = 'VAR ') or
        (copy(s, 1, 5) = 'TYPE ') or
        (copy(s, 1, 12) = 'CONSTRUCTOR ') or (copy(s, 1, 11) = 'DESTRUCTOR ') then
        resourcestringmode:=0;

      if resourcestringmode<>0 then begin
        while true do begin
          if FCanceled then Break;
          line:=trim(line);
          p:=pos('''', line);
          if p = 0 then break;

          s:=trim(copy(line,1,p-1));
          if copy(s,length(s),1)='=' then begin
            // Identifier probably precedes the string
            s:=trim(copy(s,1,length(s)-1));
            if is_identifier(s) then constident:=s
            else constident:='';
            end;

          delete(line, 1, p - 1);
          // Extract the string
          msgid:=RemoveNuls(readstring(line, src));
          if resourcestringmode=2 then begin
            if constident<>'' then begin
              if lastcomment<>'' then lastcomment:=lastcomment+sLinebreak;
              lastcomment:=lastcomment+'Programmer''s name for it: '+constident;
              end;
            AddTranslation(defaultDomain, msgid, lastcomment, MakePathRelative(sourcefilename)+':'+IntToStr(linenr));
            lastcomment:='';
            end;
          if constident<>'' then begin
            if constlist.Find(uppercase(constident),idx) then begin
              co:=constlist.Objects[idx] as TConst;
              end
            else begin
              co:=TConst.Create;
              co.Name:=constident;
              constlist.AddObject(uppercase(co.name),co);
              end;
            co.Value:=msgid;

            // If source-code comments for gnugettext enable it,
            // extract the constant even though it is not a resourcestring.
            if Length (definedDomain) > 0 then begin
              if lastcomment <> '' then lastcomment:=lastcomment + sLinebreak;
              lastcomment:=lastcomment + 'Programmer''s name for it: ' + constident;
              AddTranslation (definedDomain, msgid, lastcomment, MakePathRelative(sourcefilename)+':'+IntToStr(linenr));
              lastcomment:='';
              end;
            end;

          // Check what comes next in the line
          if copy(line, 1, 1) <> ';' then begin
            // First parameter is line number, second is the contents of the line
            if resourcestringmode=2 then
              Warning (wtSyntaxError,_('resourcestring does not end in semicolon'));
            line:='';
            break;
            end
          else begin
            // If it ended with a semicolon, analyze the rest of the line as if it was a new line
            delete(line, 1, 1);
            end;
          end;
        end
      else begin
        // Check for occurence of gettext()
        while true do begin
          if FCanceled then Break;
          uline:=uppercase(line);
          p:=poscode('_',uline);
          p2:=poscode('GETTEXT', uline);
          if p=0 then p:=p2
          else if p2<>0 then p:=min(p,p2);
          if p=0 then break;
          if (poscode('FUNCTION',uline)<>0) or (poscode('PROCEDURE',uline)<>0) then break;

          domain:=defaultDomain;
          idoffset:=0;
          if copy(uline,p,1)='_' then begin
            idlength:=1;
            idplural:=False;
            end
          else begin
            idlength:=7;
            if uppercase(copy(line, p - 1, 1)) = 'D' then begin
              domain:='';
              idlength:=8;
              idoffset:=-1;
              end;
            if uppercase(copy(line, p - 2, 2)) = 'DC' then begin
              domain:='';
              idlength:=9;
              idoffset:=-2;
              end;
            idplural:=False;
            if uppercase(copy(line, p - 2, 2)) = 'DN' then begin
              domain:='';
              idlength:=9;
              idoffset:=-2;
              idplural:=True;
              end
            else if uppercase(copy(line, p - 1, 1)) = 'N' then begin
              idlength:=8;
              idoffset:=-1;
              idplural:=True;
              end;
            end;
          if ((p+idoffset=1) or (not ((ord(uline[p+idoffset-1])<=255)
              and CharInSet(uline[p+idoffset-1],['a'..'z','A'..'Z','_','0'..'9']))))
              and (length(line)>=p+idlength+idoffset) and (not ((ord(uline[p+idoffset+idlength])<=255)
              and CharInSet(uline[p+idoffset+idlength],['a'..'z','A'..'Z','_','0'..'9']))) then begin
            line:=trim(copy(line, p + idlength+idoffset, maxint));
            if copy(line, 1, 1) = '(' then begin
              line:=trim(copy(line, 2, maxint));
              if domain = '' then begin
                // get first parameter
                extractstring(line, domain);
                line:=trim(line);
                if copy(line, 1, 1) = ',' then begin
                  delete(line, 1, 1);
                  line:=trim(line);
                  end
                else begin
                  // First parameter is line number, second is line contents
                  Warning (wtSyntaxError,_('Missing comma after first parameter'));
                  end;
                end;

              // Get parameter that contains the msgid
              msgid:=RemoveNuls(readstring(line, src));
              if idplural then begin
                line:=trim(line);
                if copy(line, 1, 1) = ',' then begin
                  delete(line, 1, 1);
                  line:=trim(line);
                  end
                else Warning (wtSyntaxError,_('Missing comma after first parameter'));
                if line='' then dxreadln(src, line);
                msgid:=msgid+#0+RemoveNuls(readstring(line,src));
                end;
              AddTranslation(domain, msgid, lastcomment, MakePathRelative(sourcefilename) + ':' + IntToStr(linenr));
              lastcomment:='';
              end { if a parenthesis is found };
            end
          else begin
            line:=trim(copy(line, p + idlength+idoffset, maxint));
            end { if it looks like a function call identifier };
          end { loop that finds several occurences in the same line };
        end { if resourcestringmode };
      end;
  finally
    CloseFile(src);
    end;

  if length (definedDomain) > 0 then
    Warning (wtExtendedDirectiveError, _('$gnugettext: end directive is missing !'));
  end;

procedure TXGetText.ExtractFromCppFile(const XFilename,CFilename : string);
var
  cmdline : string;
  el      : TStringList;
  se      : string;
  res,n   : integer;
begin
  el:=TStringList.Create;
  CurrentFilename:=CFilename;
  DoProgress(Format(_('Reading %s'),[CFilename]),CFilename,0);
  cmdline:='--keyword=_ -n '+AnsiQuotedStr(MakeRelativePath(DestinationPath,CFilename),'"')
    +' -j -o '+AnsiQuotedStr(MakePoFileName(DefaultDomain),'"');
  if (CodePage>0) and (CodePage<>cpAscii) then begin
    if CodePage=cpLatin1 then cmdline:=cmdline+' --from-code=ISO-8859-1'
    else if CodePage=cpUtf8 then cmdline:=cmdline+' --from-code=UTF-8';
    end;
  res:=StartProgram(XFilename,cmdline,DestinationPath,el);
  if res>0 then begin
    with el do if Count>0 then begin
      se:=el[0]; n:=pos('exe:',se);
      if n>0 then se:=Trim(copy(se,n+4,length(se)));
      end
    else se:=ExtractFileName(XFilename)+': '+_('Error on execution');
    Warning(wtSyntaxError,se);
    end
  else if res<0 then Warning(wtUnexpectedException,ExtractFileName(XFilename)+': '+SystemErrorMessage(abs(res)));
  el.Free;
  end;

procedure TXGetText.ExtractFromDFM(const sourcefilename: string);
var
  src: TStream;
  mem: TMemoryStream;
  line, lastline : string;
  s: string;
  i:integer;
  indent: integer;
  comment: string;
  p : integer;
  scope : TStringList;
  propertyname: string;
  multilinevalue: boolean;
  mvalue: string;
  p1,p2,p3: integer;
  pClassname: integer;
  c : AnsiChar;
  classnamepart: string;
  linechar:AnsiString;
  currentclassname: string;
  classnames: TStringList;
  instancenames: TStringList;
  excludeclass:boolean;
  excludeinstance:boolean;
  collectionlevel:integer; // will be increased which each occurence of a collection, in order to recognize nested collections
  collectionpropertyname : string; // will be the propertyname of the highest-level collection property

  procedure AddEntry(const aValue : string);
  var
    propname : string;
  begin
    if collectionlevel > 0 then propname:=collectionpropertyname
    else propname:=propertyname;
    if not excludeclass and not excludeinstance and not Excludes.ExcludeFormClassProperty(classnames[indent], propname) then begin
      comment:=scope2comment(scope, propertyname);
      AddTranslation(defaultDomain, RemoveNuls(aValue), comment, MakePathRelative(sourcefilename) + ':' + IntToStr(linenr));
    end;
  end;

begin
  src:=TFileStream.Create(sourcefilename,fmOpenRead);
  try
    // Check for empty file
    if src.Read(c,1)=0 then exit;
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

    scope:=TStringList.Create;
    classnames:=TStringlist.Create;
    instancenames:=TStringlist.Create;
    try
      classnames.Add(''); // we need that one because "indent" might start with 0
      instancenames.Add('');
      linenr:=0;
      line:='';
      propertyname:='';
      collectionpropertyname:='';
      multilinevalue:=false;
      collectionlevel:=0;
      while true do begin
        if FCanceled then Break;
        // Get next line and check it out
        lastline:=line;
        if not StreamReadln (src, line) then break;
        inc(linenr);
        if (linenr>10) and (linenr mod 100 =0) then DoProgress('','',linenr);
        indent:=measureindent(line);
        line:=trim(line);
        if line='' then continue;  // *** ABORT IF LINE IS EMPTY ***

        // Check if a collection starts or ends in this line.
        // If we have nested collections, the nesting-level
        // will be remembered                                                    
        if RightStr(line, 3) = '= <' then inc(collectionlevel);
        if RightStr(lowercase(line), 4) = 'end>' then begin
          dec(collectionlevel);
          if collectionlevel = 0 then collectionpropertyname:='';
          end;

        // Always adjust the count of "classnames" to the current "indent"
        // and make sure, that a bigger indent gets the same classname as the
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
          pClassname:=Pos(':', line);
          if pClassname > 0 then begin
            currentclassname:='';
            classnamepart:=Trim(Copy(line, pClassname+1, Length(line)-pClassname+1));
            for i:=1 to Length(classnamepart) do begin
              // be aware of constructs like "TScrollbox [0]" or other unlikely things, simply just get only the chars that are valid for classnames
              linechar:=UTF8Encode(classnamepart[i]);
              if (Length(linechar) > 1) or (not (linechar[1] in ['a'..'z','A'..'Z','_','0'..'9'])) then
                break
              else
                currentclassname:=currentclassname + classnamepart[i];
            end;
            classnames[indent]:=currentclassname;
            // remember the name of instance of that class as well in the same way
            p:=Pos(' ', line);
            instancenames[indent]:=Copy(line, p +1, pClassname -p -1);
            end;
          end;

        // check if the whole class shall be excluded
        excludeclass:=Excludes.ExcludeFormClass(classnames[indent]);
        excludeinstance:=false;
        if not excludeclass then begin
          for i:=indent downto 0 do // check parent classes if they contain a wildcard
              if Excludes.FormClassHasWildcard(classnames[i]) then begin
            excludeclass:=true;
            break;
            end;
          if not excludeclass then begin
            excludeinstance:=Excludes.ExcludeFormInstance(sourcefilename, instancenames[indent]);
            if not excludeinstance then begin
              for i:=indent-1 downto 0 do if Excludes.ExcludeFormInstance(sourcefilename, instancenames[i]) then begin
                excludeinstance:=true;
                break;
                end;
              end;
            end;
          end;

        // Check for changes in scope
        if (indent < scope.Count) and multilinevalue then begin
          multilinevalue:=false;
          AddEntry(mvalue);
          scope.Delete(scope.count - 1);
          end;
        while indent < scope.Count do begin
          scope.Delete(scope.count - 1);
          end;

        if indent > scope.Count then begin
          p:=pos(' ', lastline);
          if p = 0 then s:=lastline else s:=copy(lastline, p + 1, maxint);
          p:=pos(':', s);
          multilinevalue:=true;
          mvalue:='';
          if p = 0 then s:='' else s:=copy(s, 1, p - 1);
          end;
        while indent > scope.Count do begin
          scope.Add(s);
          s:='';
          end;

        // Analyze the line
        p:=pos(' =', line);
        p1:=pos('''', line);
        p2:=pos('#', line);
        if p1 = 0 then p1:=maxint;
        if p2 = 0 then p2:=maxint;
        p3:=min(p1, p2);

        // Extract property name if the line contains such one
        if (p <> 0) and (p < p3) then begin
          propertyname:=trim(copy(line, 1, p - 1));
          // is we're in a collection (and it's the highest level if there are nested collections), remember the property name of that collection
          if (collectionlevel = 1) and (collectionpropertyname = '') then
            collectionpropertyname:=propertyname;
          multilinevalue:=false;
          end;

        // Extract string, if the line contains such one
        if p3 <> maxint then begin
          delete(line, 1, p3 - 1);
          extractstring(line, s);
          if multilinevalue then begin
            mvalue:=mvalue + s;
            if trim(line) <> '+' then begin
              AddEntry(mvalue);
              mvalue:='';
              end;
            end
          else AddEntry(s);
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

procedure TXGetText.AddTranslation(const Domain,msgid,Comments,Location : string);
// Adds something to translate to the list
var
  it : TPoEntry;
  i,e : integer;
  sm : string;
  sl : TStringList;
  dom : TXGTDomain;
  lookupvalue : ansistring;
begin
  // Check, that all parts of msgid are nonempty, if there are multiple parts
  sm:=Trim(msgid);
  if length(sm)>0 then begin
    for i:=1 to length(sm)+1 do begin
      if copy(#0+sm+#0,i,2)=#0#0 then
        raise Exception.Create(_('Illegal msgid_plural value: It containes empty strings'));
      end;
    end;

  // Check for non-ascii characters
  if not AllowNonAscii then begin
    for i:=1 to length(sm) do begin
      if ord(sm[i])>=128 then begin
        Warning (wtNonAscii,format(_('msgid contains non-ascii characters: "%s"'),[sm]));
        // Don't add an invalid msgid
        exit;
        end;
      end;
    end;

  // Remove any Carriage Returns
  while true do begin
    i:=pos(#13,sm);
    if i=0 then break;
    delete (sm,i,1);
    end;

  // Don't add empty strings
  if length(sm)=0 then exit;

  // Don't add numbers
  val(sm,i,e);
  if (e=0) and (sm=IntToStr(i)) then exit;

  dom:=GetDomain(domain);
  sl:=TStringList.Create;
  try
    sl.Text:=utf8encode(sm);
    if sl.Count=0 then
      lookupvalue:='Weird, but happens if the string contains weird ascii chars'
    else
      lookupvalue:=sl.Strings[0];
  finally
    FreeAndNil(sl);
    end;
  it:=nil;
  if dom.msgid.Find(lookupvalue,i) then begin
    // Scroll back to the first in the list that has the same
    // first line in msgid
    while (i > 0) and (dom.msgid.Strings[i - 1] = lookupvalue) do dec(i);
    // Now loop through all those in the list it may be
    while true do begin
      it:=dom.msgid.Objects[i] as TPoEntry;
      // Check if we found the correct one
      if it.msgid = msgid then break;
      // Check if we have scrolled past the last one
      if (i = dom.msgid.Count - 1) or (dom.msgid.Strings[i+1] <> lookupvalue) then begin
        it:=nil;
        break;
        end;
      inc(i);
      end;
    end;
  if it = nil then begin
    it:=TPoEntry.Create;
    dom.msgid.AddObject(lookupvalue, it);
    it.msgid:=msgid;
    dom.order.AddObject(lookupvalue, it);
    end;
  if comments<>'' then begin
    sl:=TStringList.Create;
    try
      sl.Text:=utf8encode(comments);
      for i:=0 to sl.Count-1 do begin
        it.AutoCommentList.Add('#. '+sl.Strings[i]);
      end;
    finally
      FreeAndNil (sl);
    end;
  end;

  it.AutoCommentList.Add('#: '+RemoveFilenameSpaces(utf8encode(Location)));
  end;

function TXGetText.WriteAll(const domain: string) : boolean;
// Outputs a .po file
var
  destination: TFileStream;
  i: integer;
  item: TPoEntry;
  dom:TXGTDomain;
  filename: string;
//  orderlist:TStrings;
  overwrite: boolean;
begin
  Result:=false;
  dom:=GetDomain(domain);
  if SingleOutputFilename<>'' then begin
    if AnsiSameText(domain,defaultDomain) then filename:=SingleOutputFilename
    else exit;
    end
  else filename:=MakePoFileName(domain);

  // Check for overwriting. Call custom handler if present, and abort if overwriting is not permitted.
  if FileExists (fileName) then begin
    overwrite:=True;
    if assigned (OnOverwrite) then OnOverwrite (self, fileName, overwrite);
    if not overwrite then begin
      DoProgress(format (_('Overwrite %s aborted'), [fileName]), filename, 0);
      Exit;
      end;
    end;

  Result:=true;
  DoProgress(Format(_('Writing %s'),[filename]),filename,0);
  destination:=TFileSTream.Create (filename, fmCreate);
  try
    // Write a dummy header that the user can modify
    StreamWriteDefaultPoTemplateHeader(destination,Generator);

    // Write out all msgids - use always dom.order because dom.msgid has no ignore processing
    if OrderbyMsgid then dom.order.Sort;
//    else orderlist:=dom.order;
    with dom.order do for i:=0 to Count - 1 do begin
      item:=Objects[i] as TPoEntry;
      item.WriteToStream(destination);
      end;
  finally
    FreeAndNil (destination);
    end;
  end;

function TXGetText.ReadHeader (const spo : string; HdList : TStringList) : boolean;
var
  s : string;
  tf  : TextFile;
begin
  HdList.Clear;
  AssignFile (tf,spo,cpUtf8);
  Reset(tf); Result:=false;
  readln(tf,s);
  if copy(s,1,1)=UcBom then delete(s,1,1);
  while not Eof(tf) and not Result do begin
    Result:=length(Trim(s))=0;    //  first zero length line
    if not Result then HdList.Add(s);
    readln(tf,s);
    end;
  CloseFile(tf);
  end;

procedure TXGetText.WriteHeader (const spo : string; HdList : TStringList);
var
  tmp : TStringList;
  i   : integer;
  ok  : boolean;
begin
  tmp:=TStringList.Create;
  with tmp do begin
    LoadFromFile(spo);
    repeat
      ok:=length(Trim(Strings[0]))=0;  //  first zero length line
      if not ok then delete(0);
      until ok or (Count=0);
    for i:=0 to HdList.Count-1 do Insert(i,HdList.Strings[i]);  // insert old header
    SaveToFile(spo);
    end;
  end;

procedure TXGetText.DoProgress (const CurrentTask, CurrentFileName: string; LineNumber: Integer);
begin
  if Assigned(OnProgress) then OnProgress (CurrentTask,CurrentFileName,LineNumber)
  end;

procedure TXGetText.ExtractFromFile(const sourcefilename: string);
var
  ext : string;
  sf : string;
begin
  CurrentFilename:=sourcefilename;
  linenr:=0; LastLineRead:='';
  if ExpandFileName(sourcefilename)<>sourcefilename then sf:=BaseDirectory+SourceFilename
  else sf:=SourceFilename;
  if Excludes.ExcludeDirectory(ExtractFilePath(sf)) or Excludes.ExcludeFormFile(sf) then
    Exit;
  if FileExists(sf) then begin
    try
      ext:=uppercase(ExtractFileExt(sf));
      if (ext='.C') or (ext='.CPP') then CFiles.Add(sf)
      else begin
        DoProgress(Format(_('Reading %s'),[sf]),sf,0);
        if (ext='.DFM') or (ext='.LFM') or (ext='.XFM') then
          ExtractFromDFM(sf)
        else
        if ext='.RC' then
          ExtractFromRC(sf)
        else
        if (ext='.PAS') or (ext='.LPR') or (ext='.DPR') or (ext='.INC') then
          ExtractFromPascal(sf)
        else begin
          Warning (wtParameterError,Format(_('WARNING: Unknown file extension % - reading file as being pascal source'),[ext]));
          ExtractFromPascal(sf)
        end;
      end;
    except
      on e:EControlC do begin
        raise;
        end;
      on e:Exception do begin
        Warning (wtUnexpectedException,'Exception '+e.ClassName+sLineBreak+e.Message);
        end;
      end;
    end
  else Warning(wtUnexpectedException,Format(_('File not found: %s'),[sourcefilename]));
  CurrentFilename:='';
  end;

procedure TXGetText.ExtractFromFileMasks(const mask : string);
var
  sr: TSearchRec;
  more: boolean;
  curdir,sm : string;
  dirlist:TStringList;
  sl:TStringList;
  i,idx,n:integer;
begin
  sm:=ExpandFileName(BaseDirectory+mask);
  dirlist:=TStringList.Create;
  try
    dirlist.Add(ExtractFilePath(sm));
    sm:=ExtractFileName(sm);

    if recurse then begin   // only one level, not full recursively
      idx:=0;
      while idx<dirlist.count do begin
        curdir:=dirlist.Strings[idx];
        // Find all subdirectories
        more:=FindFirst(curdir+'*', faAnyFile, sr) = 0;
        while more do begin
          n:=ExcludeDirs.IndexOf(sr.Name);          // JR
          if (sr.Attr and faDirectory<>0) and (sr.Name<>'.') and (sr.Name<>'..') and (n<0) then
            dirlist.Add(curdir+sr.Name+PathDelim);
          if FCanceled then more:=false
          else more:=FindNext(sr) = 0;
          end;
        FindClose (sr);
        inc(idx);
        end;
      end;
    dirlist.Sort;
    for idx:=0 to dirlist.Count-1 do begin
      curdir:=dirlist.Strings[idx];
      sl:=TStringList.Create;
      try
        // Extract from all files in current directory
        more:=FindFirst(curdir+sm, faAnyFile-faDirectory, sr) = 0;
        while more do begin
          // The following if is only necessary, because several Windows versions
          // have a bug in FindFirst, that makes "test.cpp,v" match on the
          // file mask "*.cpp"
          if MatchesMask(sr.Name,sm) then sl.Add (curdir + sr.Name);
          more:=FindNext(sr) = 0;
          end;
        FindClose(sr);
        sl.Sort;
        for i:=0 to sl.count-1 do begin
          ExtractFromFile(sl.Strings[i]);
          if FCanceled then Break;
          end;
      finally
        FreeAndNil (sl);
        end;
      if FCanceled then Break;
      end;
  finally
    FreeAndNil (dirlist);
    end;
  end;

function TXGetText.GetDomain(domain: string): TXGTDomain;
var
  i: integer;
begin
  if domainlist.Find(domain, i) then begin
    Result:=domainlist.Objects[i] as TXGTDomain;
  end else begin
    Result:=TXGTDomain.Create;
    domainlist.AddObject(domain, Result);
    end;
  end;

procedure TXGetText.dxreadln(var src: TextFile; var line: string);
var
  i:integer;

  procedure cutuntil (const endtag : string);
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
      end
    else begin
      readln (src, line);
      line:=trim(line);
      LastLineRead:=line;
      inc (linenr);
      if trim(line)='' then lastcomment:='';
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
        end
      else if copy(line,i,2)='//' then begin
        // The rest of the line is a comment
        if lastcomment<>'' then
          lastcomment:=lastcomment+sLineBreak;
        lastcomment:=trim(copy(line,i+2,maxint));
        line:=copy(line,1,i-1);
        exit;
        end
      else if copy(line,i,1)='{' then begin
        if pos (cDefineDirective, lowercase(copy(line,1,length(cDefineDirective)))) = 1 then
          doHandleExtendedDirective (line);
        // Bracket comment
        cutuntil ('}');
        end
      else if copy(line,i,2)='(*' then begin
        // Bracket comment, Danish style
        cutuntil ('*)');
        end
      else inc (i);
      end;
    line:=trim(line);
    end;
  end;

function TXGetText.MakePoFileName (const domain : string) : string;
begin
  Result:=destinationpath+domain+PoExt;
  end;

procedure TXGetText.ClearConstList;
begin
  while constlist.Count<>0 do begin
    constlist.Objects[0].Free;
    constlist.Delete (0);
    end;
  end;

procedure TXGetText.Warning(WarningType : TWarningType; const msg : string);
begin
  if Assigned(OnWarning) then OnWarning (WarningType,msg,LastLineRead,CurrentFilename,linenr);
  end;

procedure TXGetText.Warning(WarningType: TWarningType; const Msg,Line,Filename : string; LineNumber: Integer);
begin
  if Assigned(OnWarning) then OnWarning (WarningType,msg,Line,Filename,linenumber);
  end;

procedure TXGetText.ExtractFromRC(const sourcefilename : string);
var
  tf:TextFile;
  line : string;
  p, i:integer;
  ident : string;
begin
  // Currently, this scanner is based on the RC file that was included
  // with DBISAM version 3. It may not work with other RC files, but
  // if you find an RC file that it does not work with, please send that
  // RC file to Lars@dybdahl.dk
  FileMode:=fmOpenRead;
  if CodePage>0 then AssignFile(tf,sourcefilename,CodePage)
  else AssignFile(tf,sourcefilename);
  Reset (tf);
  try
    linenr:=0;
    while not eof(tf) do begin
      // Get next line
      readln (tf,line);
      inc (linenr);
      if (linenr>=10) and (linenr mod 10 =0) then DoProgress('','',linenr);
      line:=trim(line);
      LastLineRead:=line;

      if copy(line,1,1)<>'#' then begin
        p:=pos('"',line);
        if p<>0 then begin
          // Find identifier in the beginning of the line
          ident:=trim(copy(line,1,p-1));
          if copy(ident,length(ident),1)=',' then delete (ident,length(ident),1);
          if ident<>'' then ident:='Programmer''s name: '+ident;
          // Find the msgid
          delete (line,1,p);
          i:=1;
          while i<=length(line) do begin
            if copy(line,i,2)='\n' then begin
              delete (line,i,1);
              line[i]:=#10;
              end
            else if copy(line,i,2)='\r' then begin
              delete (line,i,1);
              line[i]:=#13;
              end
            else if copy(line,i,2)='\t' then begin
              delete (line,i,1);
              line[i]:=#9;
              end
            else if copy(line,i,2)='\f' then begin
              delete (line,i,1);
              line[i]:=#26;
              end
            else if line[i]='\' then begin
              case line[i+1] of
                'n': line[i+1]:=#10;
                'r': line[i+1]:=#13;
                't': line[i+1]:=#9;
                end;
              delete (line,i,1);
              end
            else if line[i]='"' then delete (line,i,maxint);
            inc (i);
            end;
          AddTranslation(defaultDomain,RemoveNuls(line),ident,MakePathRelative(sourcefilename) + ':' + IntToStr(linenr));
          end;
        end;
      end;
  finally
    CloseFile (tf);
    end;
  end;

procedure TXGetText.AddBaseDirectory(path: String);
begin
  if path<>'' then BaseDirectoryList.Add(IncludeTrailingPathDelimiter(path))
  else BaseDirectoryList.Add('');
  end;

procedure TXGetText.SetExcludeDirs(const AList: string);
begin
  ExcludeDirs.CommaText:=AList;
  end;

function TXGetText.Execute : boolean;
var
  i,j : integer;
  sx,sp : string;
  hd  : TStringList;
  ok  : boolean;
begin
  FCanceled:=false;
  // If no base directories, make one
  if BaseDirectoryList.Count=0 then
    AddBaseDirectory(IncludeTrailingPathDelimiter(ExpandFileName('.')));

  // Find destination path
  // does not work on Windows 7 ==> see changes in uWork
  if DestinationPath='' then
    DestinationPath:=IncludeTrailingPathDelimiter(ExpandFileName('.'));

  // Read current ignore.po file
  if FileExists(DestinationPath+PoIgnore) then
    ignorelist.LoadFromFile(DestinationPath+PoIgnore);

  // Iterate base directories
  for j:=0 to BaseDirectoryList.Count-1 do begin
    BaseDirectory:=BaseDirectoryList.Strings[j];
    ParseExcludeFile;
    for i:=0 to filemasks.count-1 do begin
      if NoWildcards then ExtractFromFile(filemasks.Strings[i])
      else ExtractFromFileMasks(filemasks.Strings[i]);
      if FCanceled then Break;
      end;
    end;

  if FCanceled then ok:=false
  else begin
    // Handle ignores
    HandleIgnores;

    // Write files
    if UpdateIgnore then ignorelist.SaveToFile(DestinationPath+PoIgnore,true);
    ok:=true;
    // Write all domain.po files
    for i:=0 to domainlist.Count-1 do ok:=ok and WriteAll(domainlist.Strings[i]);
    end;

  if ok and (CFiles.Count>0) then begin  // process c/cpp files
    DoProgress(_('Extract strings from C/CPP files:'),'',0);
    sp:=MakePoFileName(DefaultDomain);
    hd:=TStringList.Create;
    ok:=ReadHeader(sp,hd);
    LastLineRead:=''; linenr:=0;
    sx:=ExePath+XGetTextExe;
    if FileExists(sx) then begin
      for j:=0 to CFiles.Count-1 do ExtractFromCppFile (sx,CFiles[j]);
      end
    else begin
      Warning (wtNotImplemented,_('XGetText.exe not found!'));
      ok:=false;
      end;
    if ok then WriteHeader(sp,hd);
    hd.Free;
    DoProgress(Format(_('Updating %s'),[sx]),sx,0);
    end;
  Result:=ok;
  end;

procedure TXGetText.Cancel;
begin
  FCanceled:=true;
  end;

procedure TXGetText.AddDelphiFilemasks;
begin
  with filemasks do begin
    Add ('*.pas');
    Add ('*.inc');
    Add ('*.rc');
    Add ('*.dpr');
    Add ('*.xfm');
    Add ('*.dfm');
    end;
  end;

procedure TXGetText.AddLazarusFilemasks;
begin
  with filemasks do begin
    Add ('*.pas');
    Add ('*.inc');
    Add ('*.rc');
    Add ('*.lpr');
    Add ('*.lfm');
    end;
  end;

procedure TXGetText.AddCppFilemasks;
begin
  with filemasks do begin
    Add ('*.cpp');
    Add ('*.c');
    Add ('*.rc');
    Add ('*.dfm');
    end;
  end;

procedure TXGetText.doHandleExtendedDirective(line : string);
Var
  i : integer;
  tmp : String;
begin
  delete (line, 1, length(cDefineDirective));
  line:=trim (line);
  if IsDirective(cScanOption, line) then begin
    delete (line, 1, length (cScanOption));
    line:=trim (line);
    if pos (cDomainDefinition, lowerCase (copy (line, 1, length(cDomainDefinition)))) = 1 then begin
      delete (line, 1, Length (cDomainDefinition));
      line:=trim (line);
      if (length (line) > 0) and (line[1] = '=') then begin
        delete (line, 1, 1);
        line:=trim (line);
        if (length (line) > 0) and (line[1] = '''') then begin
          delete (line, 1, 1);
          i:=1;
          tmp:='';
          while (i <= length (line)) and (line[i] <> '}') do begin
            if (line[i] = '''') then begin
              if (i = length (line)) or (line[i+1] <> '''') then begin
                definedDomain:=tmp;
                break;
                end
              else inc (i);
              end;
            tmp:=tmp + line[i];
            inc (i);
            end;
          end;
        end;
      if length (definedDomain) = 0 then
        Warning (wtExtendedDirectiveError,_('gnugettext: error in the domain name definition'));
      end
    else definedDomain:=defaultDomain;
    end
  else if IsDirective(cScanResetOption, line) then begin
    if length (definedDomain) = 0 then Warning(wtExtendedDirectiveError,_('gnugettext: reset found without scan-all'))
    else definedDomain:=''
    end
  else Warning (wtExtendedDirectiveError,_('gnugettext: Unknown option'))
  end;

procedure TXGetText.HandleIgnores;
var
  j : integer;
  dom : TXGTDomain;
  item,newitem,
  ignoreitem : TPoEntry;
begin
  // Only default domain is affected by ignore.po
  dom:=GetDomain(defaultDomain);

  // Add new ignores to new ignore list and update autocomments
  if UpdateIgnore then begin
    for j:=0 to dom.order.Count-1 do begin
      item:=dom.order.Objects[j] as TPoEntry;
      ignoreitem:=ignorelist.FindEntry(item.MsgId);
      if ignoreitem=nil then begin
        newitem:=TPoEntry.Create;
        newitem.Assign(item);
        if not IsProbablyTranslatable(newitem,IgnoreMarkers) then ignorelist.AddEntry(newitem)
        else FreeAndNil (newitem);
        end
      else ignoreitem.AutoCommentList.Text:=item.AutoCommentList.Text;
      end;
    end;

  // Remove ignores from default list
  if UseIgnoreFile then begin
    for j:=dom.order.Count-1 downto 0 do begin
      item:=dom.order.Objects[j] as TPoEntry;
      if ignorelist.FindEntry(item.MsgId)<>nil then
        // Only delete from order list
        dom.order.Delete (j);
      end;
    end;
  end;

procedure TXGetText.ParseExcludeFile;
const
 cExcludeFilename = 'ggexclude.cfg';
var
  F : TextFile;
  excludefile,
  section,
  line : string;
  lnr : integer;
  added : boolean;
begin
  lnr:=0;
  Excludes.Clear;
  Excludes.Basedirectory:=BaseDirectory;
  excludefile :=BaseDirectory;
  if RightStr(excludefile, 1) <> PathDelim then
    excludefile:=excludefile + PathDelim;
  excludefile:=ExpandFilename(excludefile + cExcludeFilename);
  if not FileExists(excludefile) then Exit;
  section:='';
  FileMode:=fmOpenRead;
  AssignFile(F,excludefile);
  Reset(F);
  try
    DoProgress(Format(_('Reading exclude file (%s)'),[excludefile]),excludefile,0);
    while not EOF(F) do begin
      Readln(F, line);
      line:=Trim(line);
      inc(lnr);
      if line <> '' then begin // skip empty lines
        if line[1] = '#' then // skip remarks
          Continue;
        if line[1] = '[' then begin // entering new section
          if RightStr(line, 1) = ']' then begin
            section:=LowerCase(Copy(line, 2, Length(line) - 2));
            if (section <> cExcludeDir)
              and (section <> cExcludeFile)
              and (section <> cExcludeFormClass)
              and (section <> cExcludeFormClassProperty)
              and (section <> cExcludeFormInstance) then
                Warning(wtExcludeFile, Format(_('Line %d: Unknown section'), [lnr]), section, excludefile, lnr);
              continue;
              end
            else Warning(wtExcludeFile, Format(_('Line %d: Looks like a section but has no closing square brackets'), [lnr]), line, excludefile, lnr);
          end;
        added:=true;
        if section = cExcludeDir then
          added:=Excludes.AddDirectory(line)
        else if section = cExcludeFile then
          added:=Excludes.AddFormFile(line)
        else if section = cExcludeFormClass then
          added:=Excludes.AddFormClass(line)
        else if section = cExcludeFormClassProperty then
          added:=Excludes.AddFormClassProperty(line)
        else if section = cExcludeFormInstance then
          added:=Excludes.AddFormInstance(line);
        if not added then
          Warning(wtExcludeFile, Format(_('Line %d: %s'), [lnr, Excludes.LastErrorMsg]), line, excludefile, lnr);
        end;
      end;
  finally
    CloseFile(F);
    end;
  end;

{ TXExcludes }

constructor TXExcludes.Create;
begin
  inherited Create;
  FExcludeFormClassPropertyList:=TXExcludeFormClassPropertyList.Create(TXExcludeFormClassProperties);
  FLastErrorMsg:='';

  FDirectories:=TStringList.Create;
  with FDirectories do begin
    Duplicates:=dupIgnore;
    Sorted:=True;
//  CaseSensitive:=True;
    end;

  FFiles:=TStringList.Create;
  with FFiles do begin
    Duplicates:=dupIgnore;
    Sorted:=True;
//  CaseSensitive:=True;
    end;

  FFormClasses:=TStringList.Create;
  with FFormClasses do begin
    Sorted:=True;
    Duplicates:=dupIgnore;
//    CaseSensitive:=False;
    end;

  FFormInstances:=TStringList.Create;
  end;

destructor TXExcludes.Destroy;
begin
  FreeAndNil(FExcludeFormClassPropertyList);
  FreeAndNil(FFormInstances);
  FreeAndNil(FFormClasses);
  FreeAndNil(FFiles);
  FreeAndNil(FDirectories);
  inherited;
  end;

function TXExcludes.AddDirectory(const aDirectory: String): boolean;
var
  sd : string;
begin
  Result:=True;
  sd:=Trim(aDirectory);
  if length(sd)>0 then begin
    if length(BaseDirectory)>0 then begin
      sd:=ExcludeTrailingPathDelimiter(GetFullInternalPath(sd));
      if DirectoryExists(sd) then FDirectories.Add(sd)
      else begin
        Result:=False;
        FLastErrorMsg:=Format(_('Directory %s doesn''t exist'),[sd]);
        end;
      end;
    end;
  end;

function TXExcludes.AddFormClass(const aClassname: String): boolean;
begin
  Result:=True;
  if length(aClassname)>0 then begin
    if Pos('.',aClassname)>0 then begin
      Result:=False;
      FLastErrorMsg:=Format(_('Wrong section: %s is a property name and not a class'),[aClassname]);
      end
    else FFormClasses.Add(Trim(aClassname));
    end;
  end;

function TXExcludes.AddFormClassProperty(const aPropertyname: String) : boolean;
var
  p : integer;
begin
  Result:=True;
  p:=Pos('.',aPropertyname);
  if p=0 then begin
    Result:=False;
    FLastErrorMsg:=Format(_('Wrong section: %s seems to be a class and not a property name'), [aPropertyname]);
    end
  else FExcludeFormClassPropertyList.AddFormClassProperty(aPropertyname);
  end;

function TXExcludes.AddFormFile(const aFilename : String): boolean;
var
  wildcardfilecount: integer;
  sf : string;
begin
  Result:=True;
  sf:=Trim(aFilename);
  if length(sf)>0 then begin
    if length(BaseDirectory)>0 then begin
      wildcardfilecount:=0;
      // if a wildcard is used, add all possible Delphi- or Kylix-files to the list
      if RightStr(sf,2) = '.*' then begin
        sf:=Copy(sf, 1, Length(sf) -2);
        if AddFormFile(sf+ '.dpr') then
          inc(wildcardfilecount);
        if AddFormFile(sf+ '.pas') then
          inc(wildcardfilecount);
        if AddFormFile(sf+ '.dfm') then
          inc(wildcardfilecount);
        if AddFormFile(sf+ '.xfm') then
          inc(wildcardfilecount);
        if AddFormFile(sf+ '.inc') then
          inc(wildcardfilecount);
        if AddFormFile(sf+ '.rc') then
          inc(wildcardfilecount);
        if wildcardfilecount = 0 then begin
          Result:=False;
          FLastErrorMsg:=Format(_('No file found for "%s.*"'), [sf]);
          end;
        end
      else begin
        sf:=GetFullInternalPath(sf);
        if FileExists(sf) then FFiles.Add(sf)
        else begin
          Result:=False;
          FLastErrorMsg:=Format(_('File %s doesn''t exist'), [sf]);
          end;
        end;
      end;
    end;
  end;

function TXExcludes.AddFormInstance(const aInstanceName : String): boolean;
var
  filenamepart,si,
  instancenamepart : String;
  i: integer;
  p: integer;
begin
  Result:=True;
  si:=Trim(aInstanceName);
  if length(si)>0 then begin
    // Search from the end of the line
    // Take into account that filenames might be absolute, containing
    // ':' on Windows; and that a file-ext might be there.
    p:=0;
    for i:=Length(si) downto 1 do begin
      if si[i] = ':' then begin
        p:=i;
        break;
        end;
      end;

    if p = 0 then begin
      Result:=False;
      FLastErrorMsg:=Format(_('Wrong syntax: No ":" found in %s'), [si]);
      exit;
      end;

    if p = Length(si) then begin
      Result:=False;
      FLastErrorMsg:=Format(_('Wrong syntax: ":" is at the end of the line of %s'), [si]);
      exit;
      end;

    filenamepart:=GetFullInternalPath(LeftStr(si,p-1));
    if not FileExists(filenamepart) then begin
      Result:=False;
      FLastErrorMsg:=Format(_('File "%s" doesn''t exist'), [filenamepart]);
      exit;
      end;
    filenamepart:=AnsiLowerCase(filenamepart);
    instancenamepart:=RightStr(si,Length(si)-p);
    FFormInstances.Append(Format('%s:%s', [filenamepart,instancenamepart]));
    end;
  end;

procedure TXExcludes.Clear;
begin
  FFiles.Clear;
  FDirectories.Clear;
  FFormClasses.Clear;
  FFormInstances.Clear;
  FExcludeFormClassPropertyList.Clear;
  FLastErrorMsg:='';
  FBaseDirectory:='';
  end;

function TXExcludes.ExcludeDirectory(const aDirectory : String): Boolean;
var
 i : Integer;
 sd : string;
begin
  Result:=False;
  sd:=Trim(aDirectory);
  if  (length(sd)=0) or (FDirectories.Count = 0) then Exit;
  sd:=ExcludeTrailingPathDelimiter(sd);
  for i:=0 to FDirectories.Count-1 do begin
    // this checks for subfolders in FDirectories[i] as well:
    if Pos(FDirectories[i],sd)=1 then begin
      Result:=True;
      Exit;
      end;
    end;
  end;

function TXExcludes.ExcludeFormClass(const aClassname : String): Boolean;
var
  i : integer;
  s,sc : String;
begin
  Result:=False;
  sc:=Trim(aClassname);
  if (length(sc)=0) or (FFormClasses.Count = 0) then Exit;
  for i:=0 to FFormClasses.Count-1 do begin
    s:=FFormClasses[i];
    if RightStr(s,1)='*' then s:=LeftStr(s, Length(s)-1);
    if AnsiStartsText(s,sc) then begin
      Result:=true;
      exit;
      end;
    end;
  end;

function TXExcludes.ExcludeFormClassProperty(const aClassname,aPropertyname : String): Boolean;
begin
  Result:=FExcludeFormClassPropertyList.ExcludeFormClassProperty(aClassname,aPropertyname)
  end;

function TXExcludes.ExcludeFormClassProperty(const aClassname : String): Boolean;
begin
  Result:=Assigned(FExcludeFormClassPropertyList.FindItem(aClassname));
  end;

function TXExcludes.ExcludeFormFile(const aFilename : String): Boolean;
var
  sf : string;
begin
  Result:=False;
  sf:=Trim(aFilename);
  if (length(sf)=0) or (FFiles.Count = 0) then Exit;
  Result:=FFiles.IndexOf(sf) > -1;
  end;

function TXExcludes.ExcludeFormInstance(const aFilename,aInstanceName : String): boolean;
var
  i,p: integer;
  filenamepart,sf,si,
  instancenamepart: String;
begin
  Result:=False;
  sf:=Trim(aFilename); si:=Trim(aInstanceName);
  if (length(si)=0) or (length(sf)=0) or (FFormInstances.Count=0) then Exit;
  p:=0;
  for i:=0 to FFormInstances.Count-1 do begin
    if AnsiEndsText(si,FFormInstances[i]) then p:=Length(si)-1
    else continue;
    if p>0 then begin
      filenamepart:=AnsiLeftStr(FFormInstances[i],Length(FFormInstances[i])-p-2);
      instancenamepart:=AnsiRightStr(FFormInstances[i],p+1);
      if AnsiSameText(filenamepart,sf) and AnsiSameText(instancenamepart,si) then begin
        Result:=true;
        exit;
        end;
      end;
    end;
  end;

function TXExcludes.FormClassHasWildcard(const aClassname : String): Boolean;
var
  i : integer;
  sc : string;
begin
  Result:=False;
  sc:=Trim(aClassname);
  for i:=0 to FFormClasses.Count-1 do begin
    if AnsiRightStr(FFormClasses[i],1) = '*' then begin
      if LeftStr(FFormClasses[i], Length(FFormClasses[i])-1)=sc then begin
        Result:=true;
        exit;
        end;
      end;
    end;
  end;

function TXExcludes.GetFullInternalPath(const s : String) : String;
begin
  Result:=Trim(ExpandFilename(Basedirectory+s));
  end;

{ TXExcludeFormClassProperties }

constructor TXExcludeFormClassProperties.Create(Collection: TCollection);
begin
  inherited Create (Collection);
  FProperties:=TStringList.Create;
  with FProperties do begin
    Duplicates:=dupIgnore;
    Sorted:=True;
    CaseSensitive:=False;
    end;
  end;

destructor TXExcludeFormClassProperties.Destroy;
begin
  FreeAndNil(FProperties);
  inherited;
  end;

procedure TXExcludeFormClassProperties.AddFormClassProperty(const aPropertyname : String);
begin
  FProperties.Add(Trim(aPropertyname));
  end;

function TXExcludeFormClassProperties.ExcludeFormClassProperty(const aPropertyname: String) : boolean;
begin
  Result:=FProperties.IndexOf(Trim(aPropertyname)) > -1;
  end;

procedure TXExcludeFormClassProperties.SetNameOfClass(const Value: String);
begin
  FNameOfClass:=Trim(Value);
  end;

{ TXExcludeFormClassPropertyList }

function TXExcludeFormClassPropertyList.Add: TXExcludeFormClassProperties;
begin
  Result:=TXExcludeFormClassProperties(inherited Add);
  end;

function TXExcludeFormClassPropertyList.AddFormClass(const aClassname : String): TXExcludeFormClassProperties;
begin
  Result:=FindItem(aClassname);
  if not assigned(Result) then begin
    Result:=Add;
    Result.NameOfClass:=aClassname;
    end;
  end;

function TXExcludeFormClassPropertyList.AddFormClassProperty(const aClassPropertyname : String): TXExcludeFormClassProperties;
var
  p: integer;
  sc,theclassname,
  thepropertyname: String;
  item: TXExcludeFormClassProperties;
begin
  Result:=nil;
  sc:=Trim(aClassPropertyname);
  if length(sc)=0 then Exit;
  p:=Pos('.',sc);
  theclassname:=Trim(LeftStr(sc,p-1));
  thepropertyname:=Trim(RightStr(sc, Length(sc) - p));
  item:=AddFormClass(theclassname);
  assert(assigned(item),'This should''t happen: item of a class was neither created nor found');
  item.AddFormClassProperty(thepropertyname);
  end;

function TXExcludeFormClassPropertyList.ExcludeFormClassProperty(const aClassname,aPropertyname : String): Boolean;
var
  item: TXExcludeFormClassProperties;
begin
  Result:=False;
  if Count>0 then begin
    item:=FindItem(aClassname);
    if assigned(item) then Result:=item.ExcludeFormClassProperty(aPropertyname);
    end;
  end;

function TXExcludeFormClassPropertyList.FindItem(const aClassname : String) : TXExcludeFormClassProperties;
var
  i:integer;
  sc : string;
begin
  Result:=nil;
  sc:=Trim(aClassname);
  if Count>0 then begin
    for i:=0 to Count-1 do if AnsiSameText(Items[i].NameOfClass,sc) then begin
      Result:=Items[i];
      exit;
      end;
    end;
  end;

function TXExcludeFormClassPropertyList.GetItems(Index: integer): TXExcludeFormClassProperties;
begin
  Result:=TXExcludeFormClassProperties(inherited Items[Index]);
  end;

procedure TXExcludeFormClassPropertyList.SetItem(Index: integer; const Value: TXExcludeFormClassProperties);
begin
  inherited SetItem(Index, Value);
  end;

end.
