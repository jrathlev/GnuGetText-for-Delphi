(* Delphi 10 Unit
   Subroutines to retrieve information about Windows version
   =========================================================

   © Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.

   Vers. 1 - July 2016
   last updated: January 2024
   *)

unit SystemInfo;

interface

uses WinApi.Windows, System.SysUtils;

type
  TSysVersionInfo = record
    SystemName,
    SubVersion        : string;
    MajorVersion,
    MinorVersion,
    Build,
    ServicePackMajor,
    ServicePackMinor  : integer;
    ProductType       : cardinal;
    SuiteMask         : word;
    IsX64             : boolean;
    end;

function GetOSVersionInfo (var vi : TSysVersionInfo) : boolean;
function GetOSVersionString  : string;
function GetOSVersionLongString  : string;
function GetOSVersionShortString  : string;

function CheckOSVersion (AMajor : Integer) : boolean; overload;
function CheckOSVersion (AMajor, AMinor : Integer) : boolean; overload;
function CheckOSBuild (ABuild : Integer) : boolean;

function IsWindows11 : boolean;
function IsScreenReaderActive : boolean;

implementation

uses System.RTLConsts, System.DateUtils, System.Win.Registry, SysInfoConsts;

const
  CVersionStr: array[Boolean] of PResStringRec = (@SVersionStr, @SSPVersionStr);
  CVersionLongStr: array[Boolean] of PResStringRec = (@rSVersionLongStr, @rSSPVersionLongStr);
  CVersionShortStr: array[Boolean] of PResStringRec = (@rSVersionShortStr, @rSSPVersionShortStr);
  CEditionStr: array[Boolean] of PResStringRec = (@SVersion32, @SVersion64);

  rkCurrentVersion = 'SOFTWARE\Microsoft\Windows NT\CurrentVersion';
  rvProductName = 'ProductName';
  rvCurrentVersion = 'CurrentVersion';  // Windows 8
  rvCurrentMajorVersionNumber = 'CurrentMajorVersionNumber';    // Windows 10
  rvCurrentMinorVersionNumber = 'CurrentMinorVersionNumber';
  rvCurrentBuildNumber = 'CurrentBuildNumber';
  rvReleaseID = 'ReleaseId';
  rvDisplayVersion = 'DisplayVersion';

type
  TGetProductInfo = function (dwOSMajorVersion, dwOSMinorVersion, dwSpMajorVersion,
    dwSpMinorVersion: DWORD; out pdwReturnedProductType: DWORD): BOOL; stdcall;

{ --------------------------------------------------------------- }
// Format without raising an exception on errors
function TryFormat(const AFormat: string; const Args: array of const): string;
begin
  try
    Result:=Format(AFormat,Args);
  except
    on E:Exception do Result:=rsFormatError+AFormat;
    end;
  end;

{ --------------------------------------------------------------- }
function GetProductInfo (dwOSMajorVersion, dwOSMinorVersion, dwSpMajorVersion,
    dwSpMinorVersion: DWORD; var pdwReturnedProductType: DWORD): boolean;
var
  KernelModule : HMODULE;
  GetProductInfo : TGetProductInfo; // erst ab Win Vista
begin
  KernelModule:=GetModuleHandle(kernel32);
  GetProductInfo:=GetProcAddress(KernelModule,'GetProductInfo');
  if assigned(GetProductInfo) then begin
    try
      Result:=GetProductInfo(dwOSMajorVersion, dwOSMinorVersion, dwSpMajorVersion,
        dwSpMinorVersion,pdwReturnedProductType);
    except
      Result:=false;
      end;
    end
  else Result:=false;
  end;

function IsWindowsServer: Boolean;
var
  osvi: TOSVersionInfoEx;
  dwlConditionMask: ULONGLONG;
begin
  FillChar(osvi, SizeOf(TOSVersionInfoEX), 0);
  osvi.wProductType := VER_NT_WORKSTATION;
  dwlConditionMask := VerSetConditionMask(0, VER_PRODUCT_TYPE, VER_EQUAL);
  Result := VerifyVersionInfo(osvi, VER_PRODUCT_TYPE, dwlConditionMask) = FALSE;
end;

function IsWindows64 : boolean;
var
  KernelModule: HMODULE;
  GetNativeSystemInfoFunc: procedure(var lpSystemInfo: WinApi.Windows.TSystemInfo); stdcall;
  SysInfo: WinApi.Windows.TSystemInfo;
begin
  Result:=False;
  KernelModule:=GetModuleHandle(kernel32);
  GetNativeSystemInfoFunc:=GetProcAddress(KernelModule,'GetNativeSystemInfo');
  if Assigned(GetNativeSystemInfoFunc) then begin
    GetNativeSystemInfoFunc(SysInfo);
    Result:=SysInfo.wProcessorArchitecture<>0;
    end;
  end;

function ProductStr (ProductType : cardinal) : string;
begin
  case ProductType of
    PRODUCT_PROFESSIONAL,
    PRODUCT_PROFESSIONAL_N:         Result:=rsProfessionalN;
    PRODUCT_PROFESSIONAL_WMC:       Result:=rsProfessionalMC;
    PRODUCT_BUSINESS,
    PRODUCT_BUSINESS_N:             Result:=rsBusiness;
    PRODUCT_CLUSTER_SERVER:         Result:=rsClusterServer;
    PRODUCT_DATACENTER_SERVER:      Result:=rsDataCenterFull;
    PRODUCT_DATACENTER_SERVER_CORE: Result:=rsDataCenterCode;
    PRODUCT_ENTERPRISE,
    PRODUCT_ENTERPRISE_N:           Result:=rsEnterprise;
    PRODUCT_ENTERPRISE_SERVER:      Result:=rsEnterpriseServerF;
    PRODUCT_ENTERPRISE_SERVER_CORE: Result:=rsEnterpriseServerC;
    PRODUCT_ENTERPRISE_SERVER_IA64: Result:=rsEnterpriseServerIT;
    PRODUCT_HOME_BASIC,
    PRODUCT_HOME_BASIC_N:           Result:=rsHomeBasic;
    PRODUCT_HOME_PREMIUM,
    PRODUCT_HOME_PREMIUM_N:         Result:=rsHomePremium;
    PRODUCT_HOME_PREMIUM_SERVER:    Result:=rsHomePremiumS;
    PRODUCT_HOME_SERVER:            Result:=rsHomeServer;
    PRODUCT_HYPERV:                 Result:=rsHyperV;
    PRODUCT_MEDIUMBUSINESS_SERVER_MANAGEMENT: Result:=rsEssBusinessMan;
    PRODUCT_MEDIUMBUSINESS_SERVER_SECURITY:   Result:=rsEssBusinessSec;
    PRODUCT_MEDIUMBUSINESS_SERVER_MESSAGING:  Result:=rsEssBusinessMess;
    PRODUCT_SERVER_FOR_SMALLBUSINESS:         Result:=rsEssServer;
    PRODUCT_SERVER_FOR_SMALLBUSINESS_V:       Result:=rsEssServerV;
    PRODUCT_SERVER_FOUNDATION:                Result:=rsServerFound;
    PRODUCT_SMALLBUSINESS_SERVER:             Result:=rsSmallBusiness;
    PRODUCT_SMALLBUSINESS_SERVER_PREMIUM:     Result:=rsSmallBusinessP;
    PRODUCT_STANDARD_SERVER:              Result:=rsStandardServer;
    PRODUCT_STANDARD_SERVER_CORE:         Result:=rsStandardServerC;
    PRODUCT_STARTER:                      Result:=rsStarter;
    PRODUCT_STORAGE_ENTERPRISE_SERVER:    Result:=rsStgEnterprise;
    PRODUCT_STORAGE_EXPRESS_SERVER:       Result:=rsStgExpress;
    PRODUCT_STORAGE_STANDARD_SERVER:      Result:=rsStgStandard;
    PRODUCT_STORAGE_WORKGROUP_SERVER:     Result:=rsStgWorkgroup;
    PRODUCT_UNDEFINED:                    Result:='';
    PRODUCT_ULTIMATE,
    PRODUCT_ULTIMATE_N:                   Result:=rsUltimate;
    PRODUCT_WEB_SERVER:                   Result:=rsWebServer;
    PRODUCT_WEB_SERVER_CORE:              Result:=rsWebServerC;
    PRODUCT_UNLICENSED:                   Result:=rsUnlicensed;
    else Result := '';
    end;
  if length(Result)>0 then Result:=' '+Result;
  end;

function GetWOW64Key (Key : cardinal) : cardinal;   //set to 64-bit view
begin
  if IsWindows64 then Result:=Key or KEY_WOW64_64KEY
  else Result:=Key;
  end;

function GetSubVersion : string;
begin
  Result:='';
  with TRegistry.Create(GetWOW64Key(KEY_READ)) do begin     // HKEY_CURRENT_USER
    RootKey:=HKEY_LOCAL_MACHINE;
    try
      if OpenKeyReadOnly(rkCurrentVersion) then begin
        Result:=ReadString(rvDisplayVersion);
        if (length(Result)=0) then Result:=ReadString(rvReleaseId);
        end;
    except
      end;
    Free;
    end;
  end;

// do not call for legacy version 6.1 (Windows 7) and below
function  GetVersionFromRegistry (var vi : TSysVersionInfo) : boolean;
var
  n,k  : integer;
  s  : string;
begin
  Result:=false;
  with TRegistry.Create(GetWOW64Key(KEY_READ)) do begin     // HKEY_CURRENT_USER
    RootKey:=HKEY_LOCAL_MACHINE;
    try
      if OpenKey(rkCurrentVersion,true) then with vi do begin
        Result:=true;
//        Name:=ReadString(rvProductName);
        if ValueExists(rvCurrentMajorVersionNumber) then begin // Windows 10
          MajorVersion:=ReadInteger(rvCurrentMajorVersionNumber);
          MinorVersion:=ReadInteger(rvCurrentMinorVersionNumber);
          if TryStrToInt(ReadString(rvCurrentBuildNumber),n) then Build:=n;
          SubVersion:=ReadString(rvDisplayVersion);
          if (length(SubVersion)=0) then SubVersion:=ReadString(rvReleaseId);
          end
        else begin   // Windows 8
          s:=ReadString(rvCurrentVersion);
          k:=pos('.',s);
          if k>0 then begin
            if TryStrToInt(copy(s,1,k-1),n) then MajorVersion:=n;
            if TryStrToInt(copy(s,k,length(s)-k+1),n) then MinorVersion:=n;
            if TryStrToInt(ReadString(rvCurrentBuildNumber),n) then Build:=n;
            end;
          end;
        end;
    except
      end;
    Free;
    end;
  end;

// GetVersion replaces the similar function in TOSVersion to fix this issue:
//   TOSVersion always returns 0 for the build number of newer systems
//   call to GetNetWkstaMajorMinor was removed, uses instead always GetProductVersion
function GetOSVersionInfo (var vi : TSysVersionInfo) : boolean;
var
  os : TOSVERSIONINFOEX;
  si: TSystemInfo;
  ReturnedProductType,
  MajorNum, MinorNum, BuildNum: DWORD;
begin
  Result:=false;
  ZeroMemory(@os,SizeOf(TOSVERSIONINFOEX));
  os.dwOSVersionInfoSize:=sizeof(os);
  if GetVersionEx (os) then begin
    with os do begin
      vi.MajorVersion:=dwMajorVersion;
      vi.MinorVersion:=dwMinorVersion;
      vi.SubVersion:='';  // used in Windows 10
      vi.Build:=dwBuildNumber;
      vi.ServicePackMajor:=wServicePackMajor;
      vi.ServicePackMinor:=wServicePackMinor;
      vi.SuiteMask:=wSuiteMask;
      vi.ProductType:=wProductType;
      ZeroMemory(@si, SizeOf(si));
      if CheckWin32Version(5, 1) then // GetNativeSystemInfo not supported on Windows 2000
        GetNativeSystemInfo(si);
      vi.IsX64:=si.wProcessorArchitecture = PROCESSOR_ARCHITECTURE_AMD64;
      end;

    with vi do begin
      SystemName:='';
      if (MajorVersion > 6) or ((MajorVersion = 6) and (MinorVersion > 1)) then begin
        if not GetVersionFromRegistry (vi) then begin
          if GetProductVersion(kernelbase, MajorNum, MinorNum, BuildNum) then begin  // try kernel32.dll
            MajorVersion := MajorNum;
            MinorVersion := MinorNum;
            if MajorVersion>=10 then SubVersion:=GetSubVersion;
            Build := BuildNum;
            end;
          end;
        end;

        if (MajorVersion >= 6) and GetProductInfo(MajorVersion,MinorVersion,
            ServicePackMajor,ServicePackMinor,ReturnedProductType) then begin
          ProductType:=ReturnedProductType;
          end
        else ProductType:=PRODUCT_UNDEFINED;

      if length(SystemName)=0 then begin
        SystemName := SWindows;
        case MajorVersion of
          10: case MinorVersion of
                0: if Build<22000 then SystemName := SWindows10 else SystemName:=rsWindows11;
              end;
          6:  case MinorVersion of
                0: if os.wProductType = VER_NT_WORKSTATION then
                     SystemName := SWindowsVista
                   else
                     SystemName := SWindowsServer2008;
                1: if os.wProductType = VER_NT_WORKSTATION then
                     SystemName := SWindows7
                   else
                     SystemName := SWindowsServer2008R2;
                2: if os.wProductType = VER_NT_WORKSTATION then
                     SystemName := SWindows8
                   else
                     SystemName := SWindowsServer2012;
                3: if not IsWindowsServer then
                     SystemName := SWindows8Point1
                   else
                     SystemName := SWindowsServer2012R2;
              end;
          5:  case MinorVersion of
                0: SystemName := SWindows2000;
                1: SystemName := SWindowsXP;
                2: begin
                    if (os.wProductType = VER_NT_WORKSTATION) and
                       (si.wProcessorArchitecture = PROCESSOR_ARCHITECTURE_AMD64) then
                      SystemName := SWindowsXP
                    else begin
                      if GetSystemMetrics(SM_SERVERR2) = 0 then
                        SystemName := SWindowsServer2003
                      else
                        SystemName := SWindowsServer2003R2
                    end;
                  end;
                end;
            end;
        end;
      end;
    Result:=true;
    end;
  end;

function GetOSVersionString  : string;
var
  vi : TSysVersionInfo;
begin
  if GetOSVersionInfo(vi) then begin
    with vi do if length(SubVersion)=0 then
      Result:=TryFormat(LoadResString(CVersionStr[ServicePackMajor <> 0]),
       [SystemName,MajorVersion,MinorVersion,Build,ServicePackMajor,LoadResString(CEditionStr[IsX64])])
    else Result:=TryFormat(rsVersion10Str,[SystemName,SubVersion,Build,LoadResString(CEditionStr[IsX64])]);
    end
  else Result:='';
  end;

function GetOSVersionLongString  : string;
var
  vi : TSysVersionInfo;
begin
  if GetOSVersionInfo(vi) then begin
    with vi do if length(SubVersion)=0 then
      Result:=TryFormat(LoadResString(CVersionLongStr[ServicePackMajor <> 0]),
        [SystemName,ProductStr(ProductType),MajorVersion,MinorVersion,Build,ServicePackMajor,LoadResString(CEditionStr[IsX64])])
    else Result:=TryFormat(rsVersion10LongStr,[SystemName,ProductStr(ProductType),SubVersion,Build,LoadResString(CEditionStr[IsX64])]);
    end
  else Result:='';
  end;

function GetOSVersionShortString  : string;
var
  vi : TSysVersionInfo;
begin
  if GetOSVersionInfo(vi) then begin
    with vi do if length(SubVersion)=0 then
      Result:=TryFormat(LoadResString(CVersionShortStr[ServicePackMajor <> 0]),
        [SystemName, ServicePackMajor, LoadResString(CEditionStr[IsX64])])
    else Result:=TryFormat(rsVersion10ShortStr,[SystemName,SubVersion,LoadResString(CEditionStr[IsX64])]);
    end
  else Result:='';
  end;

function CheckOSVersion (AMajor, AMinor : Integer) : boolean;
var
  vi : TSysVersionInfo;
begin
  if GetOSVersionInfo(vi) then
    Result:=(vi.MajorVersion>AMajor) or ((vi.MajorVersion=AMajor) and (vi.MinorVersion>=AMinor))
  else Result:=false;
  end;

function CheckOSVersion (AMajor : Integer) : boolean;
begin
  Result:=CheckOSVersion (AMajor,0);
  end;

function CheckOSBuild (ABuild : Integer) : boolean;
var
  vi : TSysVersionInfo;
begin
  if GetOSVersionInfo(vi) then Result:=vi.Build>=ABuild
  else Result:=false;
  end;

function IsWindows11 : boolean;
var
  vi : TSysVersionInfo;
begin
  if GetOSVersionInfo(vi) then with vi do begin
    Result:=(MajorVersion=10) and (Build>=22000);
    end
  else Result:=false;
  end;

function IsScreenReaderActive : boolean;
var
  scstat : Bool;
begin
  if SystemParametersInfo(SPI_GETSCREENREADER,0,@scstat,0) then Result:=scstat
  else Result:=false;
  end;

end.
