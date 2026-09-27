(* Lazarus Unit
  Subroutines and component for multilinual support with LazGetText
  =================================================================

  © Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de)

  The contents of this file may be used under the terms of the
  Mozilla Public License ("MPL") or
  GNU Lesser General Public License Version 2 or later (the "LGPL")

  Software distributed under this License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
  the specific language governing rights and limitations under the License.

  Derived from Delphi unit "LangUtils" version 2.3 (April 2026)

  Version 1.0 - September 2026

  last modified: September 2026
*)

(* @abstract(Subroutines and components for multi languge support with GnuGetText)
   @author(© Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de))
   @created(September 2026)
   @lastmod(September 2026)
*)

unit LazLangUtils;

{$mode Delphi}

interface

uses
  Classes, Windows, SysUtils, IniFiles, Menus, Graphics;

type
  TLocaleID = LCID;
  TLangCodeString = string;

  TLanguageMenuEvent = procedure(Sender : TObject; const LangCode : TLangCodeString) of object;

  TLanguageList = class (TStringList)
  private
    FMenu : TMenuItem;
    MenuSizeBefore : integer;
    FLangCode : TLangCodeString;
    FOnLangItemClick : TLanguageMenuEvent;
    FOnLangMeasureItem :TMenuMeasureItemEvent;
    FCurrentLanguage,
    FPath,FLangName : string;
    procedure AddMenuItems;
    procedure RemoveMenuItems;
    function GetLangCode (Index : integer) : TLangCodeString;
    procedure SetMenu(Menu : TMenuItem);
    function LoadDefaultNames : boolean;
  protected
    procedure DoLangItemClick(Sender : TObject); virtual;
    procedure DoLangMeasureItem (Sender: TObject; ACanvas: TCanvas; var Width, Height: Integer);
    function GetLangIndex (const Value: TLangCodeString) : integer;
    procedure SetLangCode (const Value: TLangCodeString);
  public
    constructor Create (const APath : string; const Filename : string = '');
    destructor Destroy; override;
    function LoadLanguageNames (LangCode : TLangCodeString) : boolean;
    property CurrentLanguage : string read FCurrentLanguage;
    property SelectedLanguageCode : TLangCodeString read FLangCode write SetLangCode;
    property LanguageCode[Index : integer] : TLangCodeString read GetLangCode;
    property Menu : TMenuItem read FMenu write SetMenu;
    property OnLanguageItemClick : TLanguageMenuEvent read FOnLangItemClick write FOnLangItemClick;
    property OnLanguageMeasureItem: TMenuMeasureItemEvent read FOnLangMeasureItem write FOnLangMeasureItem;
    end;

(* Instructions for Use
   ====================
  InitTranslation must be added to the project file before “Application.Initialize” is called
  and defines the application data directory to be used.
  "AppDir" is either a relative path to “AppData” or any absolute path.
  "ConfigName" specifies the optional filename for the language settings.
  "Domains" is a list of library translations required by the program
  Example call: InitTranslation(‘MySoftware’,'LangSet.cfg',['lclstrconsts','units']);
*)

procedure InitTranslation (const Domains : array of string); overload;
procedure InitTranslation (const CfgDir : string; const Domains : array of string); overload;
procedure InitTranslation (const CfgDir,ConfigName : string; const Domains : array of string); overload;

procedure SaveLanguage (const NewLangCode : TLangCodeString);
function ChangeLanguage (const NewLangCode : TLangCodeString) : TLangCodeString;

// Windows API functions missing from the standard libraries
//function GetLocaleInfoEx(lpLocaleName: PWideChar; LCType: LCTYPE;
//  lpLCData: PWideChar; cchData: Integer): Integer;

function GetUserDefaultUILanguage: LANGID; stdcall;

var
  SelectedLanguage   : TLangCodeString;
  UserLangID         : TLocaleID;
  LangFromCfg        : boolean;
  PrgPath,PrgName,
  AppSubDir,
  CfgName            : string;

const
  defLangName = 'Language.cfg';  // file with language names

  // Language setting in <PrgName>.cfg
  CfgExt = 'cfg';
  LangSekt  = 'Language';
  LangID = 'LangID';

  // Command line
  siLangOption = 'lang';      // langusge selection (e.g. /lang:de)
  siAltIni = 'ini';           // location for the alternative INI file
  siPortable = 'portable';    // run as portables program

implementation

uses Forms, StrUtils, UnitConsts, StringUtils, WinFolders, lazgettext;

type
  TGetLocaleInfoEx = function(lpLocaleName: PWideChar; LCType: LCTYPE;
    lpLCData: PWideChar; cchData: Integer): Integer; stdcall;

const
  IsVista : boolean = true;

  LOCALE_SNAME  = $0000005c;  { locale name (ie: en-us) }

var
  DllHandle : THandle;
  FGetLocaleInfoEx : TGetLocaleInfoEx;                   // available since Vista

// Windows API functions missing from the standard libraries

function GetUserDefaultUILanguage; external kernel32 name 'GetUserDefaultUILanguage'; // 5.0

//function GetLocaleInfoEx(lpLocaleName: PWideChar; LCType: LCTYPE;
//  lpLCData: PWideChar; cchData: Integer): Integer;
function GetLocalId (const LangName : widestring) : TLocaleID;
var
  nc : cardinal;
begin
  if assigned(FGetLocaleInfoEx) then begin
    nc:=0;
    if FGetLocaleInfoEx(pwidechar(LangName),LOCALE_RETURN_NUMBER or LOCALE_ILANGUAGE,@nc,4)>0 then
      Result:=nc
    else Result:=0;
    end
  else Result:=0;
  end;

type
  TLangEntry = record
    ShortName : TLangCodeString;
    Id        : TLocaleID;
    end;

const
// Table to get LangId from language short name
// Used for older systems not supporting GetLocaleInfoEx (XP and older)
  LangCount = 82;
  LangTable : array [0..LangCount-1] of TLangEntry = (
    (ShortName : 'af'; Id : $0436),
    (ShortName : 'ar'; Id : $0401),
    (ShortName : 'be'; Id : $0423),
    (ShortName : 'bg'; Id : $0402),
    (ShortName : 'bo'; Id : $0451),
    (ShortName : 'ca'; Id : $0403),
    (ShortName : 'co'; Id : $0483),
    (ShortName : 'cs'; Id : $0405),
    (ShortName : 'cy'; Id : $0452),
    (ShortName : 'da'; Id : $0406),
    (ShortName : 'de'; Id : $0407),
    (ShortName : 'el'; Id : $0408),
    (ShortName : 'en'; Id : $0409),
    (ShortName : 'es'; Id : $0C0A),
    (ShortName : 'et'; Id : $0425),
    (ShortName : 'fa'; Id : $0429),
    (ShortName : 'fi'; Id : $040B),
    (ShortName : 'fo'; Id : $0438),
    (ShortName : 'fr'; Id : $040C),
    (ShortName : 'fy'; Id : $0462),
    (ShortName : 'ga'; Id : $083C),
    (ShortName : 'gd'; Id : $0491),
    (ShortName : 'gl'; Id : $0456),
    (ShortName : 'hi'; Id : $0439),
    (ShortName : 'hr'; Id : $041A),
    (ShortName : 'hu'; Id : $040E),
    (ShortName : 'hy'; Id : $042B),
    (ShortName : 'id'; Id : $0421),
    (ShortName : 'ii'; Id : $0478),
    (ShortName : 'is'; Id : $040F),
    (ShortName : 'it'; Id : $0410),
    (ShortName : 'ja'; Id : $0411),
    (ShortName : 'ka'; Id : $0437),
    (ShortName : 'kk'; Id : $043F),
    (ShortName : 'ko'; Id : $0412),
    (ShortName : 'ku'; Id : $0492),
    (ShortName : 'ky'; Id : $0440),
    (ShortName : 'lb'; Id : $046E),
    (ShortName : 'lo'; Id : $0454),
    (ShortName : 'lt'; Id : $0427),
    (ShortName : 'lv'; Id : $0426),
    (ShortName : 'mi'; Id : $0481),
    (ShortName : 'mk'; Id : $042F),
    (ShortName : 'mn'; Id : $0450),
    (ShortName : 'mt'; Id : $043A),
    (ShortName : 'ne'; Id : $0461),
    (ShortName : 'nl'; Id : $0413),
    (ShortName : 'no'; Id : $0414),
    (ShortName : 'om'; Id : $0472),
    (ShortName : 'or'; Id : $0448),
    (ShortName : 'pa'; Id : $0446),
    (ShortName : 'pl'; Id : $0415),
    (ShortName : 'ps'; Id : $0463),
    (ShortName : 'pt'; Id : $0416),
    (ShortName : 'rm'; Id : $0417),
    (ShortName : 'ro'; Id : $0418),
    (ShortName : 'ru'; Id : $0419),
    (ShortName : 'sa'; Id : $044F),
    (ShortName : 'se'; Id : $043B),
    (ShortName : 'sk'; Id : $041B),
    (ShortName : 'sl'; Id : $0424),
    (ShortName : 'so'; Id : $0477),
    (ShortName : 'sq'; Id : $041C),
    (ShortName : 'st'; Id : $0430),
    (ShortName : 'sv'; Id : $041D),
    (ShortName : 'sw'; Id : $0441),
    (ShortName : 'ta'; Id : $0449),
    (ShortName : 'th'; Id : $041E),
    (ShortName : 'tk'; Id : $0442),
    (ShortName : 'tr'; Id : $041F),
    (ShortName : 'tt'; Id : $0444),
    (ShortName : 'ug'; Id : $0480),
    (ShortName : 'uk'; Id : $0422),
    (ShortName : 'ur'; Id : $0420),
    (ShortName : 'uz'; Id : $0443),
    (ShortName : 've'; Id : $0433),
    (ShortName : 'vi'; Id : $042A),
    (ShortName : 'wo'; Id : $0488),
    (ShortName : 'xh'; Id : $0434),
    (ShortName : 'yo'; Id : $046A),
    (ShortName : 'zh'; Id : $0804),
    (ShortName : 'zu'; Id : $0435));

  { ------------------------------------------------------------------- }
(* Name contains full path *)
function ContainsFullPath (const Name : string) : boolean;
begin
  if length(Name)>0 then Result:=(Name[1]=PathDelim) or (pos(DriveDelim,Name)>0)
  else Result:=false;
  end;

{ ------------------------------------------------------------------- }
function LangCodeToId (const LangName : TLangCodeString) : TLocaleID;
var
  i : integer;
begin
  for i:=0 to LangCount-1 do with LangTable[i] do if AnsiStartsText(ShortName,LangName) then  begin
    Result:=Id; Exit;
    end;
  Result:=0;
  end;

function LangIdToCode (id : TLocaleID) : TLangCodeString;
begin
  case id and $3FF of
  $01 : Result:='ar';
  $02 : Result:='bg';
  $03 : Result:='ca';
  $04 : Result:='zh';
  $05 : Result:='cs';
  $06 : Result:='da';
  $07 : Result:='de';
  $08 : Result:='el';
  $09 : Result:='en';
  $0a : Result:='es';
  $0b : Result:='fi';
  $0c : Result:='fr';
  $0e : Result:='hu';
  $0f : Result:='is';
  $10 : Result:='it';
  $11 : Result:='ja';
  $12 : Result:='ko';
  $13 : Result:='nl';
  $14 : Result:='no';
  $15 : Result:='pl';
  $16 : Result:='pt';
  $17 : Result:='rm';
  $18 : Result:='ro';
  $19 : Result:='ru';
  $1a : Result:='hr';
  $1b : Result:='sk';
  $1c : Result:='sq';
  $1d : Result:='sv';
  $1e : Result:='th';
  $1f : Result:='tr';
  $20 : Result:='ur';
  $21 : Result:='id';
  $22 : Result:='uk';
  $23 : Result:='be';
  $24 : Result:='sl';
  $25 : Result:='et';
  $26 : Result:='lv';
  $27 : Result:='lt';
  $2a : Result:='vi';
  $2b : Result:='hy';
  $2f : Result:='mk';
  $29 : Result:='fa';
  $30 : Result:='st';
  $33 : Result:='ve';
  $34 : Result:='xh';
  $35 : Result:='zu';
  $36 : Result:='af';
  $37 : Result:='ka';
  $38 : Result:='fo';
  $39 : Result:='hi';
  $3a : Result:='mt';
  $3b : Result:='se';
  $3c : Result:='ga';
  $3f : Result:='kk';
  $40 : Result:='ky';
  $41 : Result:='sw';
  $42 : Result:='tk';
  $43 : Result:='uz';
  $44 : Result:='tt';
  $46 : Result:='pa';
  $48 : Result:='or';
  $49 : Result:='ta';
  $4f : Result:='sa';
  $51 : Result:='bo';
  $54 : Result:='lo';
  $50 : Result:='mn';
  $52 : Result:='cy';
  $56 : Result:='gl';
  $61 : Result:='ne';
  $62 : Result:='fy';
  $63 : Result:='ps';
  $6a : Result:='yo';
  $6e : Result:='lb';
  $72 : Result:='om';
  $77 : Result:='so';
  $78 : Result:='ii';
  $80 : Result:='ug';
  $81 : Result:='mi';
  $83 : Result:='co';
  $88 : Result:='wo';
  $91 : Result:='gd';
  $92 : Result:='ku';
  else Result:='en';
    end;
  end;

{ ------------------------------------------------------------------- }
function IdToShortLanguageName (LangId : TLocaleID) : string;
var
  nc : cardinal;
  buf : array of Char;
  lct : LCTYPE;
begin
  Result:=''; nc:=0;
  if IsVista then lct:=LOCALE_SNAME else lct:=LOCALE_SISO639LANGNAME;
  nc:=GetLocaleInfo(LangId,lct,nil,nc);
  if nc>0 then begin
    SetLength(buf,nc);
    if GetLocaleInfo(LangId,lct,@buf[0],nc)>0 then
      Result:=PChar(@buf[0]);
    buf:=nil;
    end;
  if not IsVista then Result:=copy(Result,1,2);
  end;

function ShortLanguageNameToID (const LangName : TLangCodeString) : TLocaleID;
begin
  if IsVista then begin
    Result:=GetLocalId(LangName);
    //if GetLocaleInfoEx(pwidechar(WinCPToUnicode(LangName)),LOCALE_RETURN_NUMBER or LOCALE_ILANGUAGE,@nc,4)>0 then Result:=nc;
    end
  else begin
    if length(LangName)<2 then Result:=0
    else Result:=LangCodeToId(LangName);
    end;
  end;

{ ------------------------------------------------------------------- }
// Replacement for language detection in "LazGetText"
// Retrieves the selected language instead of the locale information
// Example: English system with local info setting "German" will result:
//    GetSystemDefaultUILanguage                ==> 1033 = $409 = "English (US)"
//    GetUserDefaultLangID                      ==> 1031 from regional user settings
//    GetUserDefaultUILanguage                  ==> 1031 = $407 = "German (DE)"
function GetUserLang : TLangCodeString;
var
  pli : TLocaleID;
begin
  pli:=GetUserDefaultUILanguage;       // not available with Win98
  if pli=0 then pli:=GetUserDefaultLangID;
  Result:=IdToShortLanguageName(pli);
//  Result:=LangIdToCode(pli);
  end;

// Load and save language setting
function ReadLanguageCode : TLangCodeString;
var
  s,si  : string;
  j     : integer;
  po    : boolean;

  // replace environment variable
  function ReplacePathPlaceHolder (const ps : string) : string;
  var
    n,k : integer;
    se : string;
    sv : UnicodeString;
  begin
    Result:=ps;
    n:=1;
    repeat
      n:=PosEx('%',ps,n);
      if n>0 then begin
        k:=PosEx('%',ps,n+1);
        if k>0 then begin
          sv:=copy(ps,n+1,k-n-1);
          se:=GetEnvironmentVariable(sv);
          Result:=AnsiReplaceText(Result,'%'+sv+'%',se);
          n:=k+1;
          end;
        end;
      until n=0;
    end;

begin
  po:=false; si:=''; Result:='';
  for j:=1 to ParamCount do begin   // check command line
    s:=ParamStr(j);
    if (s[1]='/') or (s[1]='-') then begin
      delete (s,1,1);
      if ReadOptionValue(s,siLangOption) then Result:=s  // laguage
      else if ReadOptionValue(s,siAltIni) then si:=ReplacePathPlaceHolder(s)  // othe ini file
      else if CompareOption(s,siPortable) then begin
        po:=true;
        if length(si)=0 then si:=PrgPath; // portable environment
        end;
      end;
    end;
  if length(si)>0 then begin
    if AnsiEndsText(PathDelim,si) then CfgName:=si+ExtractFilename(CfgName) // is path
    else begin
      s:=ExtractFilename(si);
      if po then s:=ChangeFileExt(s,'.'+CfgExt)
      else s:=ExtractFileName(CfgName);
      if ContainsFullPath(si) then si:=ExtractFilePath(si)
      else if po then begin
        if Pos(PathDelim,si)>0 then
          si:=IncludeTrailingPathDelimiter(PrgPath)+si
        else si:=PrgPath;
        end
      else begin
        if Pos(PathDelim,si)>0 then si:=ExtractFilePath(ExpandFileName(si))
        else si:=ExtractFilePath(CfgName);
        end;
      CfgName:=IncludeTrailingPathDelimiter(si)+s;
      end
    end;
  LangFromCfg:=length(Result)=0;
  if LangFromCfg then begin    //from config file
    with TMemIniFile.Create(CfgName) do begin
      Result:=ReadString(LangSekt,LangID,'');
      Free;
      end;
    end;
  end;

function GetLanguage : TLangCodeString;
begin
  Result:=ReadLanguageCode;     // from cfg file or command line
  if length(Result)=0 then Result:=GetUserLang;  // from system setting
  UserLangID:=ShortLanguageNameToID(Result);
  SelectedLanguage:=Result;
  end;

procedure SaveLanguage (const NewLangCode : TLangCodeString);
begin
  if LangFromCfg then begin
    with TMemIniFile.Create(CfgName) do begin
      WriteString(LangSekt,LangID,NewLangCode);
      try
        UpdateFile;
      finally
        Free;
        end;
      end;
    end;
  end;

{ ------------------------------------------------------------------- }
// Spracheinstellung eines laufenden Programms ändern
function ChangeLanguage (const NewLangCode : TLangCodeString) : TLangCodeString;
var
  i : integer;
begin
  SelectedLanguage:=NewLangCode;
  SaveLanguage(NewLangCode);
  UseLanguage(NewLangCode);
  if length(NewLangCode)=0 then Result:=copy(GetCurrentLanguage,1,2)  // system default
  else Result:=NewLangCode;
  UserLangID:=ShortLanguageNameToID(Result);
  with Application do for i:=0 to ComponentCount-1 do if (Components[i] is TForm) then begin
    try ReTranslateComponent(Components[i]); except; end;
    end;
  end;

{ ------------------------------------------------------------------- }
constructor TLanguageList.Create (const APath,Filename : string);
begin
  inherited Create;
//  Sorted:=true;
  MenuSizeBefore:=0; FLangCode:='';
  FPath:=IncludeTrailingPathDelimiter(APath); FLangName:=Filename;
  LoadDefaultNames;  // load default language table
  end;

destructor TLanguageList.Destroy;
begin
  inherited Destroy;
  end;

{ ------------------------------------------------------------------- }
function TLanguageList.GetLangCode (Index : integer) : TLangCodeString;
begin
  Result:=IdToShortLanguageName(TLocaleID(Objects[Index]));
  end;

function TLanguageList.LoadDefaultNames : boolean;
var
  sl      : TStringList;
  s,sn    : string;
  rs      : TResourceStream;
  ss      : TLangCodeString;
  i       : integer;
begin
  sl:=TStringList.Create; Result:=false;
  if FLangName.IsEmpty then begin // load from resource
    Result:=FindResource(HInstance,'IDR_LANGUAGES',RT_RCDATA)<>0;
    if Result then begin
    // read list of supported languages from resource
      rs:=TResourceStream.Create(HInstance,'IDR_LANGUAGES',RT_RCDATA);
      sl.LoadFromStream(rs);
      rs.Free;
      end;
    end
  else begin
    s:=FPath+FLangName;
    Result:=FileExists(s);
    if Result then sl.LoadFromFile(s);
    end;
  if Result then begin
    Clear;
    AddObject(rsSystemDefault,nil);  // system language
    for i:=0 to sl.Count-1 do begin
      s:=Trim(sl[i]);
      if (length(s)>0) and (s[1]<>'#') then begin
        ss:=ReadNxtStr(s,'=');
        sn:=Trim(ReadNxtStr(s,'#'));
        if (length(sn)>0) and (length(ss)>0) then
          AddObject(sn,pointer(ShortLanguageNameToId(ss)));
        end;
      end;
    Sort;
    if Assigned(FMenu) then SetMenu(FMenu);
    end;
  sl.Free;
  end;

{ Remove all menu items starting from the saved position MenuSizeBefore }
procedure TLanguageList.RemoveMenuItems;
begin
  if Assigned(FMenu) then with FMenu do
    while Count>MenuSizeBefore do Items[Count-1].Free;
  end;

{ Add menu items }
procedure TLanguageList.AddMenuItems;
var
  i  : integer;
  mi : TMenuItem;
begin
  if Assigned(FMenu) then begin
    (* no line if menu is empty *)
    if MenuSizeBefore>0 then
      FMenu.Add(NewLine); { insert line separator }
    for i:=0 to Count-1 do begin
      mi:=NewItem(Strings[i],0,false,True,DoLangItemClick,0,'');
      with mi do begin
        RadioItem:=true;
        GroupIndex:=123;
        Tag:=i+1;
        Checked:=false;
        OnMeasureItem:=DoLangMeasureItem;
        end;
      FMenu.Add(mi);
      end;
    end;
  end;

procedure TLanguageList.SetMenu(Menu : TMenuItem);
begin
  if Assigned(FMenu) then RemoveMenuItems;
  FMenu:=Menu; { Property-zugehörige Variable setzen }
  MenuSizeBefore:=Menu.Count; { bisherige Menügröße speichern }
  AddMenuItems; { ab sofort bleibt das Menü aktuell }
  end;

function TLanguageList.GetLangIndex (const Value: TLangCodeString) : integer;
begin
  for Result:=0 to Count-1 do
     if ShortLanguageNameToId(Value)=TLocaleID(Objects[Result]) then Exit;
  Result:=-1;
  end;

procedure TLanguageList.SetLangCode (const Value: TLangCodeString);
var
  n : integer;
begin
  n:=GetLangIndex(Value);
  if n>=0 then begin
    if assigned(FMenu) then FMenu.Items[MenuSizeBefore+n].Checked:=true;
    FLangCode:=Value;
    FCurrentLanguage:=Strings[n];
    end;
  end;

procedure TLanguageList.DoLangItemClick(Sender : TObject);
begin
  if Assigned (FOnLangItemClick) then begin
    with (Sender as TMenuItem) do begin
      Checked:=true;
      if Tag>0 then FOnLangItemClick(FMenu,IdToShortLanguageName(TLocaleID(Objects[Tag-1])));
      end;
    end;
  end;

procedure TLanguageList.DoLangMeasureItem (Sender: TObject; ACanvas: TCanvas;
    var Width, Height: Integer);
begin
  if Assigned (FOnLangMeasureItem) then FOnLangMeasureItem(Sender,ACanvas,Width,Height);
  end;

function TLanguageList.LoadLanguageNames (LangCode : TLangCodeString) : boolean;
var
  sl      : TStringList;
  s,sn    : string;
  ss      : TLangCodeString;
  i,n     : integer;
begin
  if length(LangCode)>0 then begin
    LoadDefaultNames;
    if FLangName.IsEmpty then begin  // get supported languages from resource
      for i:=1 to Count-1 do Strings[i]:=dgettext('languages',Strings[i]); // translate
      Sort;
      if Assigned(FMenu) then SetMenu(FMenu);
      end
    else begin
      s:=FPath+'locale\'+LangCode+'\LC_MESSAGES\'+FLangName;
      if not FileExists(s) then s:=FPath+'locale\'+copy(LangCode,1,2)+'\LC_MESSAGES\'+FLangName;
      Result:=FileExists(s);  // localized language table found
      if Result then begin
        sl:=TStringList.Create;
        sl.LoadFromFile(s);
        for i:=0 to sl.Count-1 do begin
          s:=Trim(sl[i]);
          if (length(s)>0) and (s[1]<>'#') then begin
            ss:=ReadNxtStr(s,'=');
            n:=GetLangIndex(ss);
            sn:=Trim(ReadNxtStr(s,'#'));
            if (length(sn)>0) and (n>=0) then Strings[n]:=sn;
            end;
          end;
        Sort;
        if Assigned(FMenu) then SetMenu(FMenu);
        sl.Free;
        end;
      end;
    SetLangCode(LangCode);
    end
  else Result:=false;
  end;

{ ------------------------------------------------------------------- }
// InitTranslation must be added to the project file before “Application.Initialize” is called
procedure InitTranslation (const CfgDir,ConfigName : string; const Domains : array of string);
var
  i  : integer;
  sc : string;

  function IsRelativePath(const Path: string): Boolean;
  begin
    if length(Path)>0 then Result:=(Path[1]<>DirectorySeparator) and (pos(DriveSeparator,Path)=0)
    else Result:=true;
    end;

begin
  if length(ConfigName)>0 then CfgName:=ConfigName
  else CfgName:=ChangeFileExt(PrgName,'.'+CfgExt);
  if IsRelativePath(CfgDir) then begin
    sc:=IncludeTrailingPathDelimiter(GetAppDataFolder)+CfgDir;
    AppSubDir:=CfgDir;
    end
  else sc:=CfgDir;
  CfgName:=IncludeTrailingPathDelimiter(sc)+CfgName;
  for i:=0 to High(Domains) do AddDomainForResourceString(Domains[i]);
  UseLanguage(GetLanguage);
  end;

procedure InitTranslation (const Domains : array of string);
begin
  InitTranslation ('','',Domains);
  end;

procedure InitTranslation (const CfgDir : string; const Domains : array of string);
begin
  InitTranslation (CfgDir,'',Domains);
  end;

{ ---------------------------------------------------------------- }
initialization
  IsVista:=(Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion >= 6);
  SelectedLanguage:=''; // default: system language
  PrgPath:=ExtractFilePath(ParamStr(0));
  PrgName:=ExtractFileName(ChangeFileExt(ParamStr(0),''));
  AppSubDir:='';
  CfgName:='';

  DllHandle:=GetModuleHandle(kernel32);
  if DllHandle<>0 then begin  // available in Vista and later
    @FGetLocaleInfoEx:=GetProcAddress(DllHandle,'GetLocaleInfoEx');
    end
  else begin
    FGetLocaleInfoEx:=nil;
    end;

end.

