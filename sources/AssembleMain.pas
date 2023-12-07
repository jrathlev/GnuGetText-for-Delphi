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

   calling: ggassemble.exe <filename> [options]
         <filename>   - Name of exe file where the mo files are to be embedded
         /lang:<list> - List of languages to be embedded
                        <list> comma separated list of language codes (ISO 639)
                        Example: /lang:de,fr,es
         /dom:<list>  - List of additional domains (other than default)
                        <list> comma separated list of domain names
                        Example: /dom:delphi10,dialogs,units
         /force       - No interactive window, start embedding immediately

   Vers. 3     - July 2023: redesign, new command line options
   Last modified: August 2023 J. Rathlev, D-24222 Schwentinental
   *)

unit AssembleMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.CheckLst, assembleengine, Vcl.Buttons;

type
  TfrmAssemble = class(TForm)
    ListBoxTranslations: TCheckListBox;
    LabelExplanation: TLabel;
    LabelExeHeading: TLabel;
    LabelExeName: TLabel;
    bbAll: TBitBtn;
    bbNone: TBitBtn;
    laVersion: TLabel;
    ButtonOK: TBitBtn;
    ButtonCancel: TBitBtn;
    OpenDialog: TOpenDialog;
    btnHelp: TBitBtn;
    btnManual: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure bbAllClick(Sender: TObject);
    procedure bbNoneClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure btnManualClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    engine : TAssembleEngine;
    force  : boolean;
    ErrMsg : string;
  public
    { Public declarations }
  end;

var
  frmAssemble: TfrmAssemble;

implementation

uses gnugettext, ggtutils, MsgDialogs;

{$R *.dfm}

{ ------------------------------------------------------------------- }
procedure TfrmAssemble.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  try HtmlHelp(0,nil,HH_CLOSE_ALL,0); except end;
  end;

procedure TfrmAssemble.FormCreate(Sender: TObject);
var
  ExeFile,
  sp,sl,sd  : string;
  i  : integer;
  ok : boolean;
begin
  TranslateComponent (self);
  Caption:=Caption+' (ggassemble)';
  laVersion.Caption:='Version: '+GetProgVersion;
  ErrMsg:=''; ok:=true;
  if ParamCount=0 then with OpenDialog do begin
    Title:=_('Select the exe file in which you want to embed the translations');
    InitialDir:=GetCurrentDir;
    if Execute then ExeFile:=Filename else ok:=false;
    end
  else ExeFile:=ExpandFileName(paramstr(1));
  if ok then begin
    if FileExists(ExeFile) then begin
      ok:=IsAnsiStr(ExeFile);
      if ok then begin
        force:=false; sl:=''; sd:='';
        if ParamCount>1 then for i:=2 to ParamCount do begin
          sp:=ParamStr(i);
          if (sp[1]='/') or (sp[1]='-') then begin
            Delete(sp,1,1);
            if CompareOption(sp,'force') then force:=true
            else if ReadOptionValue(sp,'lang') then sl:=sp
            else if ReadOptionValue(sp,'domain') then sd:=sp;
            end
          end;
        engine:=TAssembleengine.Create(ExtractFilePath(ExeFile));
        with engine do begin
          exefilename:=ExeFile;
          SetGnuGettextPatchCode;
          filemask:='*.mo';
          if length(sl)>0 then ok:=AssignLangList(sl,sd) else ok:=false;
          if not ok then begin
            ok:=PrepareFileList;
            force:=false;
            end;
          end;
        if ok then begin
          ListBoxTranslations.Items.Text:=engine.filelist.Text;
          for i:=0 to ListBoxTranslations.Items.Count-1 do ListBoxTranslations.Checked[i]:=True;
          LabelExeName.Caption:=ExeFile;
          end;
        ListBoxTranslations.ItemIndex:=0;
        end;
      if not ok then ErrMsg:=_('Filenames with Unicode characters are not supported');
      end
    else ErrMsg:=Format(_('File not found: "%s"'),[ExeFile]);
    end
  else ErrMsg:='#';
  end;

procedure TfrmAssemble.FormDestroy(Sender: TObject);
begin
  if assigned(Engine) then FreeAndNil(engine);
  end;

procedure TfrmAssemble.FormShow(Sender: TObject);
var
  s : string;
begin
  if ErrMsg='#' then Close
  else begin
    if length(ErrMsg)>0 then s:=ErrMsg
    else if ListBoxTranslations.Count=0 then s:=_('No translations (.mo files) found')
    else s:='';
    if length(s)>0 then begin
      ErrorDialog(s); Close;
      end
    else if force then begin
      s:='';
      try
        engine.Execute;
      except
        on E:Exception do s:=E.Message;
        end;
      if length(s)>0 then ErrorDialog(s)
      else InfoDialog(Format(_('Translations were embedded in "%s"'),[ExtractFilename(engine.exefilename)]));
      Close;
      end;
    end;
  end;

procedure TfrmAssemble.ButtonOKClick(Sender: TObject);
var
  i : integer;
  s : string;
begin
  screen.cursor:=crHourglass;
  try
    for i:=0 to ListBoxTranslations.Items.Count-1 do begin
      if not ListBoxTranslations.Checked[i] then
        engine.SkipFile (ListBoxTranslations.Items.Strings[i]);
      end;
    s:='';
    try
      engine.Execute;
    except
      on E:Exception do s:=E.Message;
      end;
    if length(s)>0 then ErrorDialog(s)
    else InfoDialog(Format(_('Translations were embedded in "%s"'),[ExtractFilename(engine.exefilename)]));
  finally
    screen.cursor:=crDefault;
    end;
  Close;
  end;

procedure TfrmAssemble.bbAllClick(Sender: TObject);
var
  i : integer;
begin
  with ListBoxTranslations do for i:=0 to Count-1 do Checked[i]:=true;
  end;

procedure TfrmAssemble.bbNoneClick(Sender: TObject);
var
  i : integer;
begin
  with ListBoxTranslations do for i:=0 to Count-1 do Checked[i]:=false;
  end;

procedure TfrmAssemble.btnHelpClick(Sender: TObject);
begin
  ShowHelp('basics.html#assem');
  end;

procedure TfrmAssemble.btnManualClick(Sender: TObject);
begin
  ShowManual(Application.Handle);
  end;

procedure TfrmAssemble.ButtonCancelClick(Sender: TObject);
begin
  Close;
  end;

end.

