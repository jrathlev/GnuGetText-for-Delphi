(* GunGetText for Delphi
   =====================
   Common functions for Translate Project Group

   © Dr. J. Rathlev, D-24222 Schwentinental (pb(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.

   J. Rathlev, September 2023
   last modified: November 2023
   *)

unit GgtUtils;

interface

uses Winapi.Windows, System.SysUtils, System.Classes, System.StrUtils;

resourcestring
  rsHelpName = 'Translate-e.chm';
  rsNoHelp = 'Help is not available on this system!';
  rsNoManual = 'Manual not found: %s';

function GetProgVersion : string;
procedure ShowHelp (const Topic : string = '');
procedure ShowManual (AHandle : THandle);

function GetCodePageList (sl : TStrings) : boolean;
function FindCodePage (sl : TStrings; cp : cardinal) : integer;

function CompareOption (const Param,Option : string) : boolean;
function ReadOptionValue (var Param : string; const Option : string) : boolean;

implementation

uses Winapi.ShellApi, InitProg, MsgDialogs, GgtConsts;

{------------------------------------------------------------------}
function GetProgVersion : string;
var
  rs : TResourceStream;
  verbuf : PVSFIXEDFILEINFO;
  verlen : cardinal;
begin
  Result:='???';
  try
    rs:=TResourceStream.CreateFromID(HInstance,1,RT_VERSION);
    if VerQueryValue(rs.Memory,'\',pointer(verbuf),verlen) then
      Result:=IntToStr(HiWord(verbuf.dwFileVersionMS))+'.'
        +IntToStr(LoWord(verbuf.dwFileVersionMS))+'.'
        +IntToStr(HiWord(verbuf.dwFileVersionLS));
  finally
    rs.Free;
    end;
  end;

{ ------------------------------------------------------------------- }
procedure ShowHelp(const Topic : string);
var
  s : string;
begin
  s:=PrgPath+rsHelpName;
  try
    if length(Topic)=0 then HtmlHelp(GetDesktopWindow,pchar(s),HH_DISPLAY_TOPIC,0)
    else HtmlHelp(GetDesktopWindow,pchar(s+'::/'+Topic),HH_DISPLAY_TOPIC,0);
  except
    ErrorDialog (rsNoHelp);
    end;
  end;

procedure ShowManual (AHandle : THandle);
var
  s : string;
begin
  s:=PrgPath+sManual;
  if FileExists(s) then ShellExecute(AHandle,'open',pchar(s),nil,nil,SW_SHOWNORMAL)
  else ErrorDialog(Format(rsNoManual,[s]));
  end;

{------------------------------------------------------------------}
// Liste der auf dem System vorhandenen Codepages erstellen
var
  CodePageList : TStringList;

function CpEnumProc(CodePage : PChar) : Cardinal ; stdcall;
var
  CpInfoEx : TCPInfoEx;
  s : string;
  Cp : cardinal;
  n : integer;
begin
  Cp := StrToIntDef(CodePage,0);
  if IsValidCodePage(Cp) then begin
    GetCPInfoEx(Cp, 0, CpInfoEx);
    s:=CpInfoEx.CodePageName;
    n:=pos(' ',s);
    if n=0 then n:=succ(length(s));
    delete(s,1,n);
    s:=Trim(s);
    if AnsiStartsStr('(',s) then delete(s,1,1);
    if AnsiEndsStr(')',s) then delete(s,length(s),1);
    CodePageList.AddObject(Format('%s - (%u)',[s,CpInfoEx.Codepage]),TObject(Cp));
    end;
  Result:=1;
  end;

function GetCodePageList (sl : TStrings) : boolean;
begin
  CodePageList:=TStringList.Create;
  CodePageList.Sorted:=true;
  Result:=false;
  try
    Result:=EnumSystemCodePages(@CpEnumProc, CP_SUPPORTED);
    if Result then sl.Assign(CodePageList);
  finally
    CodePageList.Free;
    end;
  end;

function FindCodePage  (sl : TStrings; cp : cardinal) : integer;
begin
  with sl do for Result:=0 to Count-1 do begin
    if cardinal(Objects[Result])=cp then Exit;
    end;
  Result:=-1;
  end;

{ ---------------------------------------------------------------- }
// Routinen zur Auswertung einer Befehlszeile
// prüfe, ob die ersten Zeichen einer Option mit dem Parameter übereinstimmen
function CompareOption (const Param,Option : string) : boolean;
begin
  Result:=AnsiLowercase(Param)=copy(Option,1,length(Param));
  end;

// Option vom Typ option:value einlesen
function ReadOptionValue (var Param : string; const Option : string) : boolean;
var
  i : integer;
begin
  Result:=false;
  i:=AnsiPos(':',Param);
  if i=0 then exit;
  if not CompareOption(copy(Param,1,i-1),Option) then exit;
  Delete(Param,1,i); Result:=true;
  end;


end.
