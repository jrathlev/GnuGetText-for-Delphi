(* Delphi program
   VCL interface to msgfmt and msgunfmt
   based on the "dxgettext" programs by Lars B. Dybdahl

   © Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.

   July 2016
   Last modified: July 2023: new command line options

   Command line:
   -------------
   Calling:  ggfmt <filename> [options]
     <filename>  - Name of file to be converted
                   Default: Generate binary message catalog from textual translation description.
     /u          - Convert binary message catalog to .po file
                   Default extension of output file: .po
     /e:<ext>    - Extension of output file if other than default
   *)

program ggfmt;

uses
  GnuGetText in 'units\GnuGetText.pas',
  System.Classes,
  System.SysUtils,
  WinApi.Windows,
  Vcl.Forms,
  Vcl.Dialogs,
  WinUtils, MsgDialogs,
  ExtSysUtils,
  ggtutils,
  ExecuteApp in 'ExecuteApp.pas',
  SelectDlg in 'units\SelectDlg.pas' {SelectDialog},
  ShowText in 'units\ShowText.pas' {ShowtextDialog};

{$R *.res}
{$IFDEF WIN32}
  {$R *-32.res}
{$ELSE}
  {$R *-64.res}
{$ENDIF}

const
  MsgFmt = 'msgfmt.exe';
  MsgUFmt = 'msgunfmt.exe';
  PoExt = '.po';
  MoExt = '.mo';

{ ---------------------------------------------------------------- }
var
  AppOutput : TStringList;
  cmdline,Filename,
  Title,
  AppPath,ProgramName,
  OutExt,sp,se    :string;
  BackConvert : boolean;
  res,i  : integer;
begin
  AddDomains(['delphi10','units']);

  BackConvert:=false;
  Filename:=''; OutExt:=MoExt; se:='';
  AppPath:=extractfilepath(paramstr(0));
  Title:=_('Convert translation')+' (ggfmt '+GetProgVersion+')';
  if ParamCount>0 then begin
    for i:=1 to ParamCount do begin
      sp:=ParamStr(i);
      if (sp[1]='/') or (sp[1]='-') then begin
        Delete(sp,1,1);
        if ReadOptionValue(sp,'e') then se:=sp
        else if CompareOption(sp,'u') then backconvert:=true;
        end
      else if length(Filename)=0 then Filename:=sp;
      end;
    if BackConvert then begin
      ProgramName:=MsgUFmt; OutExt:=PoExt
      end
    else begin
      ProgramName:=MsgFmt; OutExt:=MoExt
      end;
    if length(se)>0 then OutExt:='.'+se;
    sp:=ChangeFileExt(filename,OutExt);

    Application.Initialize;
    Application.CreateForm(TSelectDialog, SelectDialog);
    Application.CreateForm(TShowtextDialog, ShowtextDialog);
    AppOutput:=TStringlist.Create;
    try
      cmdline:=Filename+' -o "'+sp+'"';
      res:=StartProgram(AppPath+ProgramName,cmdline,'',AppOutput);
      if res<0 then ErrorDialog(Title,Format(_('Execution of "%s" failed: '),[programname])
                       +sLineBreak+SystemErrorMessage(abs(res)))
      else if res>0 then begin
        i:=SelectOption(Title,programname+_(' reports some errors, no processing has been done!'),
          mtError,[],[_('Show errors')],'',0,-1,_('OK'));
        if i=0 then ShowTextDialog.Execute(CenterPos,Format(_('Error report from "%s"'),[programname]),'',
                               AppOutput,1,stShowModal,[sbPrint,sbSearch]);
        end
      else begin
        if BackConvert then se:=_('Binary file was converted to "%s"!')
        else se:=_('Binary file "%s" was generated!');
        InfoDialog(Title,Format(se,[ExtractFilename(sp)]));
        end;
    finally
      FreeAndNil (AppOutput);
      end;
    end
  else ErrorDialog(Title,_('No file specified!'));
  Application.Terminate;
end.
