unit xgettexttools;
(**************************************************************)
(*                                                            *)
(*  (C) Copyright by Lars B. Dybdahl and Jens Berke           *)
(*  E-mail: Lars@dybdahl.dk, phone +45 70201241               *)
(*  You may distribute and modify this file as you wish       *)
(*  for free                                                  *)
(*                                                            *)
(*  See http://dybdahl.dk/dxgettext/ for more information     *)
(*                                                            *)
(**************************************************************)
// changes  (J. Rathlev, July 20223):
//   removed: linux sections
//   code redesign

interface

uses
  Winapi.Windows, System.Classes;

var
  DefCP:integer=CP_ACP;

function ConvertWhitespaceToSpaces (const s : string) : string;
function is_identifier(const ws : string) : boolean;
function poscode (const substr,line : string) : integer;
function StreamReadln (s : TStream; var line : string):boolean; // Returns false if end of line
function measureindent(const s: string): word;
function scope2comment(sl: TStrings; const name: string) : string;
function RemoveFilenameSpaces (const s : string) : string;
function StringToWidestring (const s : ansistring) : string;
function IsDirective(const directive,line : String): boolean;



implementation

uses
  System.SysUtils, gnugettext;

function measureindent(const s: string): word;
// Returns number of spaces this line used to indent this line
begin
  Result:=0;
  while (copy(s, Result + 1, 1) = ' ') do inc(Result);
  end;

function ConvertWhitespaceToSpaces (const s : string) : string;
var
  i:integer;
begin
  Result:=s;
  for i:=1 to length(s) do if Result[i]<=#32 then Result[i]:=' ';
  end;

function is_identifier(const ws : string):boolean;
var
  i : integer;
begin
  Result:=length(ws)>0;
  if Result then begin
    Result:=CharInSet(ws[1],['a'..'z','A'..'Z','_']);
    if Result then begin
      for i:=2 to length(ws) do begin
        Result:=Result and CharInSet(ws[i],['a'..'z','A'..'Z','_','0'..'9']);
        if not Result then break;
        end;
      end;
    end;
  end;

function poscode (const substr,line : string) : integer;
// Same as pos(), but ignores everything inside strings
var
  i:integer;
  ssl:integer;
  quotemode:boolean;
begin
  ssl:=length(substr);
  quotemode:=False;
  for i:=1 to length(line)-ssl+1 do begin
    if line[i]='''' then quotemode:=not quotemode;
    if (not quotemode) and (copy(line,i,ssl)=substr) then begin
      Result:=i;
      exit;
      end;
    end;
  Result:=0;
  end;

function RemoveFilenameSpaces (const s : string) : string;
var
  i:integer;
begin
  Result:=s;
  for i:=1 to length(Result) do if Result[i]=' ' then Result[i]:='_';
  end;

function StringToWidestring (const s : ansistring) : string;
var
  res:integer;
begin
  if s='' then Result:=''
  else begin
    SetLength (Result, length(s));
    res:=MultiByteToWideChar(DefCP, 0, PAnsiChar(s), length(s), PWideChar(Result),length(s));
    if res=0 then
      raise Exception.Create(_('Cannot convert ansistring to string with specified codepage'));
    SetLength (Result, res);
    end;
  end;

function StreamReadln (s : TStream; var line : string) : boolean; // Returns false if end of line. reads single-bytes and converts these.
var
  c : AnsiChar;
  aline : ansistring;
begin
  Assert (s<>nil,_('StreamReadln requires the stream to be not nil'));
  Result:=True;
  aline:='';
  while true do begin
    if s.Read(c,1)=0 then begin
      Result:=False;
      break;
      end;
    if c=#10 then break;
    if c<>#13 then aline:=aline+c;
    end;
  line:=StringToWidestring(aline);
  end;

function scope2comment(sl: TStrings; const name: string) : string;
// Converts a list of strings to a single-line comment separated by dots
var
  i : integer;
begin
  Result:='';
  with sl do for i:=0 to Count-1 do if length(Strings[i])>0 then begin
    if Result<>'' then Result:=Result+'.';
    Result:=Result+Strings[i];
    end;
  if Result<>'' then Result:=Result+'.';
  Result:=Result + name;
  end;

function IsDirective(const directive,line: String): boolean;
begin
  Result:=pos (directive, lowerCase (copy (line, 1, length(directive)))) = 1;
  end;

end.
