(* Lazarus Unit
   Retrieve Windows standard folders

   © Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.

   Vers. 1 - September 2026
   last modified: September 2026
   *)
(* @abstract(Subroutines for Windows Desktop and Shell)
   @author(© Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de))
   @created(September 2026)
   @lastmod(September 2026)
*)

unit WinFolders;

{$MODE Delphi}

interface

uses
  Windows, Classes, SysUtils, ShlObj;

type
// see function GetProgramFolder
  TProgramFolder = (pfProgramFiles,pfCommonProgramFiles,pfProgramFiles86,pfCommonProgramFiles86,
    pfProgramFiles64,pfCommonProgramFiles64);


function GetDesktopFolder (Typ : integer) : string;
function GetKnownFolder (rfId : TGUID) : string;
function GetProgramFolder (pfType : TProgramFolder) : string;

function GetUserProfileFolder : string;
function GetPersonalFolder : string;
function GetAppDataFolder : string;
function GetLocalAppDataFolder : string;
function GetPicturesFolder : string;
function GetVideoFolder : string;
function GetMusicFolder : string;
function GetPublicFolder : string;
function GetUserDesktopFolder : string;
function GetUserStartupFolder : string;
function GetFavoritesFolder : string;
function GetProgramDataFolder : string;


implementation

uses ActiveX, WinDirs;

{ ------------------------------------------------------------------- }
var
  IsVista : boolean;

{ ---------------------------------------------------------------- }
function GetDesktopFolder (Typ : integer) : string;
var
  pidl          : LPItemIDList;
  FolderPath    : PWideChar;
  pMalloc       : IMalloc;
begin
  pidl:=nil;
  SHGetMalloc(pMalloc);
  if SUCCEEDED(SHGetSpecialFolderLocation(0,Typ,pidl)) then begin
    FolderPath := WideStrAlloc(max_path);
    SHGetPathFromIDListW(pidl,FolderPath);
    SetLastError(0);
    Result:=FolderPath;
    StrDispose(FolderPath);
    end
  else Result:='';
  if pidl<>nil then pMalloc.Free(pidl);
  pMalloc._Release;
  end;

// https://learn.microsoft.com/de-de/windows/win32/shell/knownfolderid#remarks
function GetKnownFolder (rfId : TGUID) : string;  // available since Vista
var
  ppszPath : PWideChar;
begin
  Result:='';
  if (Win32Platform=VER_PLATFORM_WIN32_NT) and (Win32MajorVersion>=6) and
      SUCCEEDED(SHGetKnownFolderPath(rfId,0,0,ppszPath)) then begin
    try
      Result:=ppszPath;
    finally
      CoTaskMemFree(ppszPath);
      end;
    end
  end;

function GetProgramFolder (pfType : TProgramFolder) : string;
begin
  case pfType of
  pfProgramFiles86 : begin
    Result:=GetEnvironmentVariable('ProgramFiles(x86)'); // get from environment
    if length(Result)=0 then begin
      if IsVista then Result:=GetKnownFolder(FOLDERID_ProgramFilesX86)
      else Result:=GetDesktopFolder(CSIDL_PROGRAM_FILESX86);
      end;
    end;
  pfProgramFiles64 : begin
    Result:=GetEnvironmentVariable('ProgramW6432'); // get from environment
    if length(Result)=0 then begin
      if IsVista then Result:=GetKnownFolder(FOLDERID_ProgramFiles)
      else Result:=GetDesktopFolder(CSIDL_PROGRAM_FILES);
      end;
    end;
  pfCommonProgramFiles86 : begin
    Result:=GetEnvironmentVariable('CommonProgramFiles(x86)'); // get from environment
    if length(Result)=0 then begin
      if IsVista then Result:=GetKnownFolder(FOLDERID_ProgramFilesCommonX86)
      else Result:=GetDesktopFolder(CSIDL_PROGRAM_FILES_COMMONX86);
      end;
    end;
  pfCommonProgramFiles64 : begin
    Result:=GetEnvironmentVariable('CommonProgramW6432'); // get from environment
    if length(Result)=0 then begin
      if IsVista then Result:=GetKnownFolder(FOLDERID_ProgramFilesCommon)
      else Result:=GetDesktopFolder(CSIDL_PROGRAM_FILES_COMMON);
      end;
    if length(Result)=0 then Result:=GetDesktopFolder(CSIDL_PROGRAM_FILES_COMMON);
    end;
  pfCommonProgramFiles : if length(Result)=0 then begin
      if IsVista then Result:=GetKnownFolder(FOLDERID_ProgramFilesCommon)
      else Result:=GetDesktopFolder(CSIDL_PROGRAM_FILES_COMMON);
      end;
  else if length(Result)=0 then begin
      if IsVista then Result:=GetKnownFolder(FOLDERID_ProgramFiles)
      else Result:=GetDesktopFolder(CSIDL_PROGRAM_FILES);
      end;
    end;
  end;

function GetUserProfileFolder : string;
begin
  if IsVista then Result:=GetKnownFolder(FOLDERID_PROFILE)
  else Result:=GetDesktopFolder(CSIDL_PROFILE);
  end;

function GetPersonalFolder : string;
begin
  if IsVista then Result:=GetKnownFolder(FOLDERID_Documents)
  else Result:=GetDesktopFolder(CSIDL_PERSONAL);
  end;

function GetAppDataFolder : string;
begin
  if IsVista then Result:=GetKnownFolder(FOLDERID_RoamingAppData)
  else Result:=GetDesktopFolder(CSIDL_APPDATA);
  end;

function GetLocalAppDataFolder : string;
begin
  if IsVista then Result:=GetKnownFolder(FOLDERID_LocalAppData)
  else Result:=GetDesktopFolder(CSIDL_LOCAL_APPDATA);
  end;

function GetPicturesFolder : string;
begin
  if IsVista then Result:=GetKnownFolder(FOLDERID_Pictures)
  else Result:=GetDesktopFolder(CSIDL_MYPICTURES);
  end;

function GetVideoFolder : string;
begin
  if IsVista then Result:=GetKnownFolder(FOLDERID_Videos)
  else Result:=GetDesktopFolder(CSIDL_MYVIDEO);
  end;

function GetMusicFolder : string;
begin
  if IsVista then Result:=GetKnownFolder(FOLDERID_Music)
  else Result:=GetDesktopFolder(CSIDL_MYMUSIC);
  end;

function GetPublicFolder : string;
begin
  if IsVista then Result:=GetKnownFolder(FOLDERID_Public)
  else Result:=GetDesktopFolder(CSIDL_COMMON_DOCUMENTS);
  end;

function GetUserDesktopFolder : string;
begin
  if IsVista then Result:=GetKnownFolder(FOLDERID_Desktop)
  else Result:=GetDesktopFolder(CSIDL_DESKTOP);
  end;

function GetUserStartupFolder : string;
begin
  if IsVista then Result:=GetKnownFolder(FOLDERID_Startup)
  else Result:=GetDesktopFolder(CSIDL_STARTUP);
  end;

function GetFavoritesFolder : string;
begin
  if IsVista then Result:=GetKnownFolder(FOLDERID_Favorites)
  else Result:=GetDesktopFolder(CSIDL_FAVORITES);
  end;

function GetProgramDataFolder : string;
begin
  if IsVista then Result:=GetKnownFolder(FOLDERID_ProgramData)
  else Result:=GetDesktopFolder(CSIDL_COMMON_APPDATA);
  end;


initialization
  IsVista:=(Win32Platform=VER_PLATFORM_WIN32_NT) and (Win32MajorVersion>=6);
end.

