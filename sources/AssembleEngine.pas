(* Delphi program
   Embed translations into exe file
   based on the "dxgettext" programs by Lars B. Dybdahl

   © Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.

   Modified: J. Rathlev, Oct. 2010
   Changes for Delphi-10: J. Rathlev, Aug. 2016
   Note: All mo files must contain only pure ASCII characters (ans
   *)

unit assembleengine;

interface

uses
  System.Classes;

type
  TAssembleEngine = class
    private
      basedirectory:string;
      function RecurseDirs (list:TStringList; dir:string) : boolean;
      function FindPatchPosition(str: TFileStream): int64;
    public
      exefilename:string;
      patchcode:AnsiString;
      filemask:string;
      filelist:TStringList;  // Objects are TFileInfo
      constructor Create (const BaseDir : string);
      destructor Destroy; override;
      procedure SetGnuGettextPatchCode;
      function AssignLangList (const sl,sd : String) : boolean; //
      function PrepareFileList : boolean; // Always run that before Execute;
      procedure SkipFile (filename : string); // removes item from list
      procedure Execute;
    end;

  function IsAnsiStr (const ws : string) : boolean;

implementation

uses
  System.SysUtils, System.StrUtils, gnugettext;

type
  TFileInfo = class
    filename : ansistring; // Relative, including the first directory name
    offset   : int64;      // Position in .exe file where this file starts
    size     : int64;
    end;

{ ------------------------------------------------------------------- }
// check if string has characteres >#255
function IsAnsiStr (const ws : string) : boolean;
var
  i : integer;
begin
  Result:=false;
  for i:=1 to length(ws) do if WordRec(ws[i]).Hi<>0 then exit;
  Result:=true;
  end;

procedure StreamWrite (s:TStream;line:ansistring);
var
  nextpos:integer;
begin
  nextpos:=length(line);
  if nextpos>0 then
    if s.Write(line[1],nextpos)<>nextpos then
      raise Exception.Create (_('Error when writing to stream'));
end;

procedure StreamWriteInt64 (s:TStream; i:int64);
begin
  Assert (sizeof(i)=8);
  s.WriteBuffer(i,8);
end;

{ ------------------------------------------------------------------- }
{ Tassembleengine }
constructor TAssembleEngine.Create (const BaseDir : string);
begin
  basedirectory:=IncludeTrailingPathDelimiter(BaseDir);
  filemask:='*';
  filelist:=TStringList.Create;
end;

destructor TAssembleEngine.Destroy;
begin
  with filelist do while Count<>0 do begin
    if assigned(Objects[0]) then Objects[0].Free;
    Delete (0);
  end;
  FreeAndNil (filelist);
  inherited;
end;

procedure TAssembleEngine.Execute;
var
  str,infile:TFileStream;
  i:integer;
  nextpos:int64;
  fi:TFileInfo;
  patchposition,tableoffset:int64;
begin
  if exefilename='' then
    raise Exception.Create (_('No .exe filename specified'));

//  basedirectory:=extractfilepath(exefilename);  //JR - see create

  // Find all files to include
  if filelist.count=0 then PrepareFileList;
  filelist.Sort;

  str:=TFileStream.Create (exefilename,fmOpenReadWrite);
  try
    // Find first patch position
    patchposition:=FindPatchPosition (str);
    if patchposition=0 then
      raise Exception.Create (Format(_('Patch code "%s" was not found in .exe file.'
        +sLineBreak+'Are you sure the .exe file has been compiled with the correct libraries?'),[patchcode]));

    // Add files to the end of the exe file
    str.Seek(0, soFromEnd);
    for i:=0 to filelist.count-1 do begin
      fi:=filelist.objects[i] as TFileInfo;
      infile:=TFileStream.Create (basedirectory+fi.filename, fmOpenRead+fmShareDenyNone);
      try
        fi.offset:=str.Position;
        fi.size:=infile.Size;
        str.CopyFrom(infile,0);
      finally
        FreeAndNil (infile);
      end;
    end;

    // Write List of files
    while str.position and $ff<>0 do
      StreamWrite (str,#0);
    tableoffset:=str.Position;
    nextpos:=tableoffset;
    for i:=0 to filelist.Count-1 do begin
      while str.position<>nextpos do
        StreamWrite (str,' ');
      fi:=filelist.Objects[i] as TFileInfo;
      nextpos:=((str.Position+sizeof(nextpos)+sizeof(fi.offset)+sizeof(fi.size)+length(fi.filename))+256) and (not $ff);
      StreamWriteInt64(str,nextpos);
      StreamWriteInt64(str,fi.offset);
      StreamWriteInt64(str,fi.size);
      StreamWrite (str,fi.filename);
    end;
    while str.position<>nextpos do
      StreamWrite (str,' ');
    StreamWriteInt64(str,0);

    // In some cases, there may be more than one gnugettext.pas compiled into
    // the exe file, which makes it contain multiple patchcodes. Therefore,
    // we must iterate through all the patchcodes in the file.
    while patchposition <> 0 do begin
      str.Seek(patchposition+length(patchcode),soFromBeginning);
      StreamWriteInt64(str,TableOffset);
      patchposition:=FindPatchPosition (str);
    end;
  finally
    FreeAndNil (str);
  end;
end;

function TAssembleEngine.FindPatchPosition(str: TFileStream): int64;
// Finds the position of patchcode in the file.
const
  bufsize=100000;
var
  a,b : AnsiString;
  offset:integer;
  rd,p:Integer;
begin
  if patchcode='' then
    raise Exception.Create (_('No patch code has been specified'));
  offset:=str.Position;  // Search from the point where we left last time
  SetLength (a, bufsize);
  SetLength (b, bufsize);
  str.Read(a[1],bufsize);
  while true do begin
    rd:=str.Read(b[1],bufsize);
    p:=pos(patchcode,a+b);
    if (p<>0) and (p<bufsize+100) then begin
      Result:=offset+p-1;
      if copy(a+b,p+length(patchcode),8)<>#0#0#0#0#0#0#0#0 then
        raise Exception.Create (_('This file has already been modified. Please recompile this .exe file'));
      exit;
    end;
    if rd<>bufsize then begin
      // Prematurely ended without finding anything
      Result:=0;
      exit;
    end;
    a:=b;
    offset:=offset+bufsize;
  end;
  Result:=0;
end;

const
  sLocDir = 'locale\';
  sMsgDir = '\LC_MESSAGES\';
  sDefDom = 'default';
  sMoExt  = '.mo';

// add default.mo for predefined languages
function TAssembleEngine.AssignLangList (const sl,sd : String) : boolean;
var
  i,j   : integer;
  ss    : string;
  ll,ld : TStringList;

  procedure AddFile (const fn : string);
  var
    fi    : TFileInfo;
  begin
    if FileExists(basedirectory+fn) and (filelist.IndexOf(fn)<0) then begin
      fi:=TFileInfo.Create;
      fi.filename:=fn;
      filelist.AddObject(fn,fi);
      end;
    end;

begin
  Result:=IsAnsiStr(sl) and IsAnsiStr(sd);
  if Result then begin
    ll:=TStringList.Create; ld:=TStringList.Create;
    ll.CommaText:=sl; ld.CommaText:=sd;
    with ll do for i:=0 to Count-1 do begin
      ss:=sLocDir+Strings[i]+sMsgDir;
      AddFile(ss+sDefDom+sMoExt);
      with ld do for j:=0 to Count-1 do AddFile(ss+Strings[j]+sMoExt);
      end;
    ll.Free; ld.Free;
    end;
  end;

// search for mo fiiles
function TAssembleEngine.PrepareFileList : boolean;
begin
  if filelist.Count=0 then Result:=RecurseDirs (filelist,'')
  else Result:=true;
end;

function TAssembleEngine.RecurseDirs(list : TStringList; dir : string) : boolean;
var
  sr:TSearchRec;
  dirlist:TStringList;
  more:boolean;
  fi:TFileInfo;
begin
  Result:=true;
  dirlist:=TStringList.Create;
  try
    dirlist.Add(dir);

    while (dirlist.Count<>0) and Result do begin
      dir:=dirlist.Strings[0];
      dirlist.Delete (0);
      
      // Scan this directory for subdirectories
      more:=FindFirst (basedirectory+dir+'*',faAnyFile,sr)=0;
      while more do begin
        if (sr.Name<>'.') and (sr.Name<>'..') then begin
          if sr.Attr and faDirectory<>0 then begin
            dirlist.Add(dir+sr.Name+PathDelim);
            end;
          end;
        more:=Findnext (sr)=0;
        end;
      FindClose (sr);

      // Scan this directory for files
      more:=FindFirst (basedirectory+dir+filemask,faAnyFile,sr)=0;
      while more do begin
        if (sr.Name<>'.') and (sr.Name<>'..') then begin
          if sr.Attr and faDirectory=0 then begin
            if not AnsiSameText(dir+sr.Name,exefilename) then begin
              Result:=IsAnsiStr(sr.Name);
              if Result then begin
                fi:=TFileInfo.Create;
                fi.filename:=dir+sr.Name;
                list.AddObject (fi.filename, fi);
                end
              else Break;
              end;
            end;
          end;
        more:=Findnext (sr)=0;
        end;
      FindClose (sr);
      end;
  finally
    FreeAndNil (dirlist);
  end;
end;

procedure TAssembleEngine.SetGnuGettextPatchCode;
begin
  patchcode:='6637DB2E-62E1-4A60-AC19-C23867046A89';
end;

procedure TAssembleEngine.SkipFile(filename: string);
var
  idx:integer;
begin
  idx:=filelist.IndexOf(filename);
  if idx=-1 then raise Exception.Create ('Internal error. Filename not found in list: '+filename);
  filelist.Objects[idx].Free;
  filelist.Delete(idx); 
end;

end.
