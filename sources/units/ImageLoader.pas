(* Delphi-Unit
   Load SVG images from zip file and replace in existing SVG image lists
   =====================================================================

   The unit makes it possible to load the images for buttons, etc. from an external
   zip file when the program is started. They will replace the images specified
   on design time to enable the use of a different design.

   Supported designs:              Commandline     cfg file        zip archive
   -----------------               -----------     --------        -----------
   - default: predesigned images                   IconStyle=0
   - plain  : plain image set      /style:plain    IconStyle=1     default-plain.zip

   Requirements:
   -------------
   - All used glyphs and images are provided in SVG format and must have unique names
   - The SVG images are packed to zip files, grouping into separated files is possible
   - The program uses the SVGIconImageList package by Ethea
     https://github.com/EtheaDev/SVGIconImageList

   Usage:
   ------
   - The zip files with all used images must be located either in the directory of
     the exe file or in a subdirectory of it.
   - At least one image archive with the name default-x.zip (x = col or plain) must be
     provided.
   - The InitImageLoader procedure must be called in the dpr file before the forms
     are created:
     - "InitImageLoader;" if only the default archive is used
     - "InitImageLoader([grp1,grp2,..]);"  if additional groups are used
   - If the design should be read from a cfg file, the associated filename must be
     specified either by calling "InitTranslation" (see unit "LangUtils") or
     by using the overload version of "InitImageLoader(CfgName,...)"
   - Each form of the program must call the procedure LoadImages in the OnCreate
     event. All SVGIconImageList or SVGIconImageCollection used in the form
     must be specified as list: "LoadImages ([AImageIcons1,AImageIcons2,...);"
     Each image from the lists will be replaced by the image of same name from
     the zip archive.

   Example:
   --------
   DPR file:  InitImageLoader(['dialogs']);
     will load "default-x.zip" and "dialogs-x.zip"
   PAS file of each form: ImageLoader.LoadImages([imlSmall.SVGIconItems,imlGlyphs.SVGIconItems]);
     will replace images with matching names

   © Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.

   May 2025
   last modified: February 2026
   *)
(* @abstract(Load SVG images from zip file and replace in existing SVG image lists
   @note(requires SVGIconImageList package by Ethea - @url(https://github.com/EtheaDev/SVGIconImageList)))
   @author(© Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de))
   @created(May 2025)
   @lastmod(February 2026)
*)

unit ImageLoader;

interface

uses System.IniFiles, SVGIconItems;

const
  iniStyle = 'IconStyle';

type
  TImageStyle = (isDefault,isPlain);
  TStyleType = (stMiss,stNormal,stCommand);

procedure InitImageLoader; overload;
procedure InitImageLoader (const AImgGroups : array of string); overload;
procedure InitImageLoader (const SubDir : string); overload;
procedure InitImageLoader (const SubDir : string; const AImgGroups : array of string); overload;
procedure InitImageLoader (const ConfigName,SubDir : string; const AImgGroups : array of string); overload;

function GetImageStyle : TImageStyle;
procedure SetImageStyle (AImgStyle  : TImageStyle);
function GetStyleType : TStyleType;

function LoadImageStyle (ACfgFile : TCustomIniFile) : TImageStyle;
procedure SaveImageStyle (ACfgFile : TCustomIniFile; Value : TImageStyle);

procedure LoadImages (AImageIcons : array of TSVGIconItems); overload;
procedure LoadImages (const AImgGroup : string; AImageIcons : array of TSVGIconItems); overload;

implementation

uses System.Classes, System.SysUtils, System.StrUtils, System.Zip, Vcl.Forms,
  StringUtils, PathUtils,  InitProg;

const
  ImgStyleSuffix : array [TImageStyle] of string = ('none','plain');

type
  TZipImages = class (TCollectionItem)
  private
    Data : TMemoryStream;
  public
    ImgGroup : string;
    ZipFile : TZipFile;
  end;

  TZipImageCollection = class (TCollection)
  public
    constructor Create;
    function AddZipImages (const AImgGroup,AZipName : string) : TZipImages;
    function GetZipFile (const AImgGroup : string) : TZipFile;
    end;

constructor TZipImageCollection.Create;
begin
  inherited Create(TZipImages);
  end;

function TZipImageCollection.AddZipImages (const AImgGroup,AZipName : string) : TZipImages;
var
  fs : TFileStream;
begin
  if FileExists(AZipName) then begin
    Result:=TZipImages(Add);
    with Result do begin
      ImgGroup:=AImgGroup;
      Data:=TMemoryStream.Create;
      try
        fs:=TFileStream.Create(AZipName,fmOpenRead or fmShareDenyWrite);
        try
          fs.Position:=0;
          Data.CopyFrom(fs,fs.Size);
        finally
          fs.Free;
          end;
      except
        end;
      if Data.Size>0 then begin
        Data.Position:=0;
        ZipFile:=TZipFile.Create;
        ZipFile.Open(Data,zmRead);
        end
      else ZipFile:=nil;
      end
    end
  else Result:=nil;
  end;

function TZipImageCollection.GetZipFile (const AImgGroup : string) : TZipFile;
var
  i,n : integer;
begin
  n:=-1;
  for i:=0 to Count-1 do if AnsiSameText(TZipImages(Items[i]).ImgGroup,AImgGroup) then begin
    n:=i; Break;
    end;
  if n>=0 then Result:=TZipImages(Items[i]).ZipFile else Result:=nil;
  end;

var
  FSubDir : string;
  FImgStyle  : TImageStyle;
  FStyleType : TStyleType; // style undefined, user defined or set from command line
  ImageArchives : TZipImageCollection;

const
// command line
  siIconStyle = 'design';     // image style
  siAltIni = 'ini';           // Ort für alternative Ini-Datei
  siPortable = 'portable';    // starte als portables Programm

// cfg file (ini formatted)
  CfgExt = 'cfg';
  CfgSekt  = 'Config';
  imgDefault = 'default';

// Check if ImageStyle is specified in command line or cfg file
function ReadImageStyle : TImageStyle;
var
  s,si,st  : string;
  j     : integer;
  po    : boolean;
  imst  : TImageStyle;

  // replace environment variable
  function ReplacePathPlaceHolder (const ps : string) : string;
  var
    n,k : integer;
    se,sv : string;
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
  po:=false; si:=''; st:=''; FStyleType:=stNormal; Result:=isDefault;
  for j:=1 to ParamCount do begin   // check command line
    s:=ParamStr(j);
    if (s[1]='/') or (s[1]='-') then begin
      delete (s,1,1);
      if ReadOptionValue(s,siIconStyle) then st:=s  // icon style
      else if ReadOptionValue(s,siAltIni) then si:=ReplacePathPlaceHolder(s)  // alternate ini path
      else if CompareOption(s,siPortable) then begin
        po:=true;
        if length(si)=0 then si:=ExtractFilePath(Application.ExeName) // portable environment
        end;
      end;
    end;

  if length(st)=0 then begin    // from cfg file
    if FileExists(CfgName) then with TMemIniFile.Create(CfgName) do begin
      Result:=TImageStyle(ReadInteger(CfgSekt,iniStyle,0));
      if not ValueExists(CfgSekt,iniStyle) then FStyleType:=stMiss;
      Free;
      end
    else begin // search in install directory
      st:=PrgPath+ExtractFileName(CfgName);
      if FileExists(st) then with TMemIniFile.Create(st) do begin
        Result:=TImageStyle(ReadInteger(CfgSekt,iniStyle,0));
        if not ValueExists(CfgSekt,iniStyle) then FStyleType:=stMiss;
        Free;
        end;
      end;
    end
  else begin    // change default style
    FStyleType:=stCommand;
    for imst:=Low(TImageStyle) to High(TImageStyle) do
      if AnsiLowercase(st)=copy(ImgStyleSuffix[imst],1,length(st)) then Result:=imst;
//      if ImgStyleSuffix[imst]=AnsiLowerCase(copy(st,0,1)) then Result:=imst;
    end;
  end;

procedure InitImageLoader (const SubDir : string; const AImgGroups : array of string);
var
  zn : string;

  function GetZipName(const ImgGroup : string) : string;
  begin
    Result:=AddPath(MakeAbsolutePath(PrgPath,FSubDir),ImgGroup)+'-'+ImgStyleSuffix[FImgStyle]+'.zip';
    end;

begin
  FSubDir:=SubDir;
  FImgStyle:=ReadImageStyle;
  with ImageArchives do begin
    BeginUpdate;
    AddZipImages(imgDefault,GetZipName(imgDefault));
    for zn in AImgGroups do AddZipImages(zn,GetZipName(zn));
    EndUpdate;
    end;
  end;

procedure InitImageLoader (const AImgGroups : array of string); overload;
begin
  InitImageLoader('',AImgGroups);
  end;

procedure InitImageLoader (const SubDir : string); overload;
begin
  InitImageLoader(Subdir,[]);
  end;

procedure InitImageLoader;
begin
  InitImageLoader('',[]);
  end;

procedure InitImageLoader (const ConfigName,SubDir : string; const AImgGroups : array of string); overload;
begin
  if not CfgName.IsEmpty then Exit;  // already defined by InitTranslation
  if length(ConfigName)>0 then CfgName:=ConfigName
  else CfgName:=ChangeFileExt(PrgName,'.'+CfgExt);
  CfgName:=IncludeTrailingPathDelimiter(GetAppPath)+CfgName;
  InitImageLoader(SubDir,AImgGroups);
  end;

function LoadImageStyle (ACfgFile : TCustomIniFile) : TImageStyle;
begin
  Result:=TImageStyle(ACfgFile.ReadInteger(CfgSekt,iniStyle,0));
  end;

procedure SaveImageStyle (ACfgFile : TCustomIniFile; Value : TImageStyle);
begin
  ACfgFile.WriteInteger(CfgSekt,iniStyle,integer(Value));
  end;

function GetImageStyle : TImageStyle;
begin
  Result:=FImgStyle;
  end;

procedure SetImageStyle (AImgStyle  : TImageStyle);
begin
  FImgStyle:=AImgStyle;
  end;

function GetStyleType : TStyleType;
begin
  Result:=FStyleType;
  end;

procedure LoadImages (AImageIcons : array of TSVGIconItems);
begin
  LoadImages('',AImageIcons);
  end;

procedure LoadImages (const AImgGroup : string; AImageIcons : array of TSVGIconItems); overload;
var
  i,zi   : integer;
  zn     : string;
  zf     : TZipFile;
  zh     : TZipHeader;
  sdata  : TMemoryStream;
  il     : TSVGIconItems;
begin
  if FImgStyle=isDefault then Exit;
  if length(AImgGroup)=0 then zn:=imgDefault else zn:=AImgGroup;
  zf:=ImageArchives.GetZipFile(zn);
  if assigned(zf) then begin
    for il in AImageIcons do begin
      with il do for i:=0 to Count-1 do begin
        zi:=zf.IndexOf(Items[i].IconName+'.svg');
        if zi>=0 then begin
          sdata:=TMemoryStream.Create;
          zf.Read(zi,TStream(sdata),zh);
          sdata.Position:=0;
          Items[i].SVG.LoadFromStream(sdata);
          sdata.Free;
          end;
        end;
      end;
    end;
  end;

procedure CloseImageArchives;
var
  i : integer;
begin
  with ImageArchives do for i:=0 to Count-1 do with TZipImages(Items[i]) do begin
    with ZipFile do begin
      Close; Free;
      end;
    Data.Free;
    end;
  end;

initialization
  FSubDir:='';
  FImgStyle:=isDefault;
  ImageArchives:=TZipImageCollection.Create;
finalization
  CloseImageArchives;
  ImageArchives.Free;
end.
