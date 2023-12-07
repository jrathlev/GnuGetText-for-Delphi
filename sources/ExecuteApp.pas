(* Delphi Unit
   Start console application

   © Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.

   July 2016
   *)

unit ExecuteApp;

interface

uses Winapi.Windows, System.SysUtils, System.Classes;

function StartProgram(const Appl,Para,Dir : string; ErrorOutput : TStrings = nil) : integer;

implementation

uses Vcl.Forms, Vcl.Controls, System.StrUtils;

const
  Lf = #10;
  CrLf = #13#10;

{ ---------------------------------------------------------------- }
function StartProgram(const Appl,Para,Dir : string; ErrorOutput : TStrings) : integer;
// Returns 0  = success
//         <0 = system error
//         >0 = exitcode
const
  BUFSIZE = 1024;
var
  si        : TStartupInfo;
  pi        : TProcessInformation;
  ec        : dword;
  saAttr    : TSecurityAttributes;
  hChildStdoutRd,
  hChildStdoutWr  : THandle;
  dwRead    : dword;
  chBuf     : array [0..BUFSIZE] of AnsiChar;
  sa        : string;
  pwd       : PChar;
begin
  Screen.Cursor:=crHourglass;
// Set the bInheritHandle flag so pipe handles are inherited.
  with saAttr do begin
    nLength:=sizeof(SECURITY_ATTRIBUTES);
    bInheritHandle:=TRUE;
    lpSecurityDescriptor:=nil;
    end;

// Create a pipe for the child process's STDOUT.
// default buffer (0) size results in timeout if many errors occur
  if not CreatePipe(hChildStdoutRd,hChildStdoutWr,@saAttr, 16384) then begin
    Result:=2; exit;
    end;
  SetHandleInformation(hChildStdoutRd, HANDLE_FLAG_INHERIT, 0);

// Create process to start the program "Appl"
  FillChar(si, SizeOf(TStartupInfo), 0);
  with si do begin
    cb := Sizeof(TStartupInfo);
    dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
    wShowWindow:=SW_HIDE;
    hStdError:=hChildStdoutWr;
    end;
  if length(Dir)>0 then pwd:=pchar(Dir) else pwd:=nil;

  if CreateProcess(nil,                // Anwendungsname
                   pchar('"'+Appl+'" '+Para),
                   nil,                // Security
                   nil,                // Security
                   true,               // use InheritHandles
                   NORMAL_PRIORITY_CLASS, // Priorität
                   nil,                // Environment
                   pwd,                // Verzeichnis
                   si,pi) then begin
    ec:=50;    // wait max. 5 s
    while (WaitForSingleObject(pi.hProcess,100)<>WAIT_OBJECT_0) and (ec>0) do begin
      dec(ec);
      Application.ProcessMessages;
      end;
    if ec=0 then Result:=-WAIT_TIMEOUT
    else begin
      GetExitCodeProcess(pi.hProcess,ec);
      CloseHandle(pi.hProcess);
  // Close the write end of the pipe before reading from the
  // read end of the pipe.
      if not CloseHandle(hChildStdoutWr) then begin
        Screen.Cursor:=crDefault;
        Result:=2; exit;
        end;
      chBuf[BUFSIZE]:=#0; sa:='';
  // Read output from the child process, and write to parent's STDOUT.
      while ReadFile(hChildStdoutRd,chBuf[0],BUFSIZE,dwRead,nil)
            and (dwRead=BUFSIZE) do begin
        sa:=sa+chBuf;
        end;
      if dwRead>0 then begin
        chBuf[dwread]:=#0;
        sa:=sa+chBuf;
        end;
      if assigned(ErrorOutput) then
        ErrorOutput.Text:=AnsiReplaceStr(AnsiReplaceStr(sa,CrLf,Lf),Lf,CrLf);    // replace lf with Cr+Lf
      Result:=ec;
      end;
    end
  else Result:=-GetLastError;
  Screen.Cursor:=crDefault;
  end;

end.
