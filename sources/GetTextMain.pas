unit GetTextMain;
(**************************************************************)
(*                                                            *)
(*  (C) Copyright by Lars B. Dybdahl                          *)
(*  E-mail: Lars@dybdahl.dk, phone +45 70201241               *)
(*  You may distribute and modify this file as you wish       *)
(*  for free                                                  *)
(*                                                            *)
(*  See http://dybdahl.dk/dxgettext/ for more information     *)
(*                                                            *)
(**************************************************************)

// Vers. 3     - July 2023: redesign, new commandline parameters
//   calling: ggdxgettext.exe <filename> [options]
//         <filename>    - Name of basic directory or file to be scanned
//         /dom:<domain> - use this domain instead of default

// Last modified: October 2023 J. Rathlev, D-24222 Schwentinental

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  xgettext, GgtConsts, Vcl.Buttons;

type
  TfrmGetText = class(TForm)
    MemoProgress: TMemo;
    LabelProgress: TLabel;
    TimerActivate: TTimer;
    laVersion: TLabel;
    ButtonOK: TBitBtn;
    procedure ButtonOKClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure TimerActivateTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    FOverwrite : boolean;
    procedure Progress (const CurrentTask,CurrentFileName:string;LineNumber:Integer);
    procedure Warning (WarningType:TWarningType;const Msg,Line,Filename:string;LineNumber:Integer);
    procedure OverwriteQuestion (sender: TObject; const aFileName: String; var Overwrite: boolean);
  public
    { Public declarations }
  end;

var
  frmGetText: TfrmGetText;

implementation

uses System.IniFiles, gnugettext, ggtutils, GetTextConfig;

{$R *.dfm}

procedure TfrmGetText.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  try HtmlHelp(0,nil,HH_CLOSE_ALL,0); except end;
  end;

procedure TfrmGetText.FormCreate(Sender: TObject);
begin
  TranslateComponent (self);
  TimerActivate.Enabled:=True;
  Caption:=Caption+' (ggdxgettext)';
  laVersion.Caption:='Version: '+GetProgVersion;
  FOverwrite:=false;
  end;

procedure TfrmGetText.OverwriteQuestion(sender: TObject;
  const aFileName: String; var Overwrite: boolean);
begin
  Overwrite:=FOverwrite
    or (MessageDlg(Format(_('Do you want to overwrite "%s"?'),[aFilename]),mtConfirmation,[mbYes,mbNo],0)=mrYes);
  end;

procedure TfrmGetText.ButtonOKClick(Sender: TObject);
begin
  Close;
  end;

procedure TfrmGetText.FormResize(Sender: TObject);
begin
//  ButtonOK.Left:=(Width-ButtonOK.Width) div 2;
  end;

procedure TfrmGetText.Progress(const CurrentTask, CurrentFileName: string;
  LineNumber: Integer);
begin
  LabelProgress.Caption:=CurrentTask;
  LabelProgress.Update;
  if LineNumber<=1 then begin
    MemoProgress.Lines.Add(CurrentTask);
    Application.ProcessMessages;
    end;
  end;

procedure TfrmGetText.Warning(WarningType: TWarningType; const Msg, Line,
  Filename: string; LineNumber: Integer);
begin
  with MemoProgress do begin
    Lines.Add('* '+Msg);
    if LineNumber>0 then Lines.Add('  '+Format(_('Line: %u'),[Linenumber]));
    if length(Line)>0 then Lines.Add('  '+Format(_('Last line read: %s'),[Line]));
//    Lines.Add('');
    end;
  end;

procedure TfrmGetText.TimerActivateTimer(Sender: TObject);
var
  frmConfig        : TfrmConfig;
  xgt              : TXGetText;
  ProjectFile,
  ProjectDir,Domain,
  IniName,sn       : string;
  i                : integer;
  ok               : boolean;

  procedure Explode (line:string;sl:TStrings);
  var
    i,last : integer;
    item : string;
  begin
    last:=1;
    line:=line+' ';
    for i:=1 to length(line) do begin
      if line[i]<=#32 then begin
        item:=trim(copy(line,last,i-last));
        if item<>'' then sl.Add (item);
        last:=i;
      end;
    end;
  end;

  begin
  TimerActivate.Enabled:=False;
  try
    Domain:=''; ProjectDir:=''; ProjectFile:='';
    for i:=1 to ParamCount do begin
      sn:=ParamStr(i);
      if (sn[1]='/') or (sn[1]='-') then begin
        Delete(sn,1,1);
        if ReadOptionValue(sn,'domain') then Domain:=sn
        end
      else ProjectDir:=ExpandFileName(sn); // root directory with source files to be scanned
      end;
    frmConfig:=TfrmConfig.Create (self);
    with frmConfig do begin
      if FileExists(ProjectDir) then begin          // is single file
       ProjectFile:=ExtractFileName(ProjectDir);
       ProjectDir:=ExtractFilePath(ProjectDir);
       end
      else if DirectoryExists(ProjectDir) then ProjectDir:=IncludeTrailingPathDelimiter(ProjectDir)
      else ProjectDir:='';
      IniName:=ProjectDir+DxGetTextIni;
      CheckBoxRecurse.Checked:=length(ProjectFile)=0;
      CheckBoxSaveSettings.Checked:=fileexists(IniName);
      if CheckBoxSaveSettings.Checked then LoadFromIni(IniName);
      EditBasepath.Text:=ProjectDir;
      if length(ProjectFile)>0 then EditMask.Text:=ProjectFile;
      if length(Domain)>0 then SetDomain(Domain);
      end;
    with frmConfig do begin
      ok:=ShowModal=mrOK;
      if ok then begin
        SaveToIni(length(ProjectFile)=0);
        FOverwrite:=cbOverwrite.Checked;
        try
          xgt:=TXGetText.Create(Format('Delphi Get Text (%s)',[GetProgVersion]));
          with xgt do begin
            ExePath:=ExtractFilePath(Application.ExeName);
            Recurse:=CheckBoxRecurse.Checked;
            SetExcludeDirs(ExcludeDirs.Text);
            UpdateIgnore:=CBCreateIgnore.Checked;
            UseIgnoreFile:=CBRemoveIgnore.Checked;
            DestinationPath:=IncludeTrailingPathDelimiter(EditBasepath.Text);  // JR
            AddBaseDirectory(DestinationPath);
            AllowNonAscii:=rgEncoding.ItemIndex>0; //not cbCheckNonAscii.Checked;
            case rgEncoding.ItemIndex of
            1 : CodePage:=cpLatin1;
            2 : CodePage:=cpUtf8;
            else CodePage:=0;
              end;
            if rbOther.Checked then defaultDomain:=OutputName.Text;    // JR
            Explode(EditMask.Text,filemasks);
            OrderbyMsgid:=cbOrder.Checked;
            IgnoreListFile:=DestinationPath+sIgnoreList;
            OnProgress:=Progress;
            OnWarning:=Warning;
            OnOverwrite:=OverwriteQuestion;
            Execute;
            end;
        finally
          FreeAndNil(xgt);
          end;
        end;
      end;
  finally
    FreeAndNil (frmConfig);
    end;
  if ok then begin
    if MemoProgress.Lines.Count=0 then MemoProgress.Lines.Add(_('No warnings or errors'));
    LabelProgress.Caption:=_('Finished');
    ButtonOK.Enabled:=True;
    end
  else Close;
  end;

end.
