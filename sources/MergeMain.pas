(* Delphi program
   merge translation - uses UTF-8 encoding by default

   © Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de)
   based on the "dxgettext" programs by Lars B. Dybdahl

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.

   July 2016
   Vers. 2.8.1 - February 2021: template stored as relative path
   Vers. 2.8.2 - November 2022: error output added
   Vers. 3     - July 2023: redesign
                 requires MsgMerge.exe Windows binary from
                 https://mlocati.github.io/articles/gettext-iconv-windows.html
   Vers. 3.1   - August 2023: enhanced header management, UTF-8 as defdault encoding,
                 uses always internal merge function (instead of msgmerge)
   Vers. 3.2   - October 2024: optional merging of AutoComments and HistComments
                               optional merging with similat msgids

   Command line:
   -------------
   Calling:  ggmerge <filename> [options]
     <filename>  - Name of file to be merged
     /noedit     - Do not open editor after merging

   Last modified: October 2024
   *)

unit MergeMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.Buttons, Vcl.ComCtrls;

type
  TfrmMerge = class(TForm)
    EditTranslation: TLabeledEdit;
    EditTemplate: TLabeledEdit;
    cbCreateBackup: TCheckBox;
    cbSaveSettings: TCheckBox;
    laVersion: TLabel;
    ButtonChooseTemplate: TSpeedButton;
    ButtonChooseTranslation: TSpeedButton;
    btnMerge: TBitBtn;
    OpenDialog: TOpenDialog;
    btnClose: TBitBtn;
    cbCodePage: TComboBox;
    gbEncoding: TGroupBox;
    rbUtf: TRadioButton;
    rbOther: TRadioButton;
    btnHelp: TBitBtn;
    btnManual: TBitBtn;
    cbMergeAutoComments: TCheckBox;
    cbMergeHistory: TCheckBox;
    cbMergeSimilar: TCheckBox;
    edSimLength: TEdit;
    udSimLength: TUpDown;
    procedure btnMergeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ButtonChooseTranslationClick(Sender: TObject);
    procedure ButtonChooseTemplateClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure rbUtfClick(Sender: TObject);
    procedure rbOtherClick(Sender: TObject);
    procedure btnManualClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    wx,wy : integer;
    ErrorOutput       : TStringList;
    CodePage          : cardinal;
    noedit            : boolean;
    procedure LoadFromIni (const IniName : string);
    function SelectPoFile : boolean;
    function GetGenerator : string;
    procedure ShowCodePage;
    function MergeTranslation (const translationfilename,templatefilename,outputfilename : string) : boolean;
  public
    { Public declarations }
  end;

var
  frmMerge: TfrmMerge;

implementation

uses
  Winapi.shellapi, System.IniFiles, System.StrUtils, gnugettext, poparser,
  GgtConsts, GgtUtils, FileCopy;

{$R *.dfm}

{ ---------------------------------------------------------------- }
const
  ggmSect = 'ggmerge';

  iniBackup   = 'createbackup';
  iniSAscii   = 'supportnonascii';
  iniSimLen = 'SimMeasure';
  iniSimilar = 'MergeSimilar';
  iniAuto = 'MergeAutoComments';
  iniHist = 'ObsoleteTranslations';

  IniExt = '.ini';

{ ---------------------------------------------------------------- }
procedure TfrmMerge.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  try HtmlHelp(0,nil,HH_CLOSE_ALL,0); except end;
  end;

procedure TfrmMerge.FormCreate(Sender: TObject);
var
  i  : integer;
  sn : string;
begin
  TranslateComponent (self);
  noedit:=false;
  for i:=1 to ParamCount do begin
    sn:=ParamStr(i);
    if (sn[1]='/') or (sn[1]='-') then begin
      Delete(sn,1,1);
      if CompareOption(sn,'noedit') then Noedit:=true;
      end
    else EditTranslation.Text:=ExpandFileName(sn); // file to be merged
    end;
  laVersion.Caption:='Version: '+GetProgVersion;
  FormResize (self);
  Caption:=Caption+' (ggmerge)';
  wx:=Left; wy:=Top+Height+10;
  ErrorOutput:=TStringList.Create;
  end;

procedure TfrmMerge.FormDestroy(Sender: TObject);
begin
  ErrorOutput.Free;
  end;

procedure TfrmMerge.FormResize(Sender: TObject);
begin
//  ButtonGo.Left:=(Width-ButtonGo.Width) div 2;
end;

procedure TfrmMerge.ShowCodePage;
begin
  if CodePage=CpUtf8 then rbUtf.Checked:=true
  else begin
    rbOther.Checked:=true;
    with cbCodePage do ItemIndex:=FindCodePage(Items,CodePage);
    end;
  cbCodePage.Enabled:=rbOther.Checked;
  end;

procedure TfrmMerge.FormShow(Sender: TObject);
var
  ok : boolean;
begin
  with EditTranslation do begin
    ok:=FileExists(Text);
    if not ok then ok:=SelectPoFile;
    end;
  if ok then begin
    // create list of available code pages
    with cbCodePage do if GetCodePageList(Items) then begin
      Enabled:=true;
      ItemIndex:=FindCodePage(Items,CpIso8859);
      end
    else begin
      rbOther.Enabled:=false;
      Enabled:=false;
      Clear;
      Items.Add(_('No code pages available'));
      ItemIndex:=0;
      end;
    LoadFromIni(ChangeFileExt(EditTranslation.Text,IniExt));
    ShowCodePage;
    end
  else Close;
  end;

procedure TfrmMerge.LoadFromIni (const IniName : string);
begin
  cbSaveSettings.Checked:=FileExists(ininame);
  if cbSaveSettings.Checked then begin
    with TMemIniFile.Create (ininame) do begin
      Left:=ReadInteger(ggmSect,iniLeft,Left);       // JR
      Top:=ReadInteger(ggmSect,iniTop,Top);
      EditTemplate.Text:=ExpandFileName(ReadString(ggmSect,iniTempl,''));
      cbCreateBackup.Checked:=ReadBool(ggmSect,iniBackup,cbCreateBackup.Checked);
      udSimLength.Position:=ReadInteger(ggmSect,iniSimLen,defSimMeasure);
      cbMergeSimilar.Checked:=ReadBool(ggmSect,iniSimilar,false);
      cbMergeAutoComments.Checked:=ReadBool(ggmSect,iniAuto,false);
      cbMergeHistory.Checked:=ReadBool(ggmSect,iniHist,false);
      CodePage:=ReadInteger(ggmSect,iniCode,CpUtf8);
      Free;
      end;
    end;
  end;

function TfrmMerge.SelectPoFile : boolean;
begin
    with OpenDialog do begin
    Title:=_('Select po file to be merged');
    if length(EditTranslation.Text)>0 then InitialDir:=ExtractFilePath(EditTranslation.Text)
    else InitialDir:='';
    FileName:=EditTranslation.Text;
    DefaultExt:='po';
    Filter:=_('Translation files (*.po)|*.po|All files (*.*)|*.*');
    Result:=Execute;
    if Result then EditTranslation.Text:=FileName;
    end;
  end;

procedure TfrmMerge.rbOtherClick(Sender: TObject);
begin
  with cbCodePage do CodePage:=cardinal(Items.Objects[ItemIndex]);
  ShowCodePage;
  end;

procedure TfrmMerge.rbUtfClick(Sender: TObject);
begin
  CodePage:=CpUtf8;
  ShowCodePage;
  end;

procedure TfrmMerge.btnCloseClick(Sender: TObject);
begin
  Close;
  end;

procedure TfrmMerge.ButtonChooseTranslationClick(Sender: TObject);
begin
  SelectPoFile;
  LoadFromIni(ChangeFileExt(EditTranslation.Text,IniExt));
  ShowCodePage;
  end;

procedure TfrmMerge.ButtonChooseTemplateClick(Sender: TObject);
begin
  with OpenDialog do begin
    Title:=_('Select template file');
    if length(EditTemplate.Text)>0 then InitialDir:=ExtractFilePath(EditTemplate.Text)
    else if length(EditTranslation.Text)>0 then InitialDir:=ExtractFilePath(EditTranslation.Text)
    else InitialDir:='';
    FileName:=ExtractFileName(EditTemplate.Text);
    DefaultExt:='po';
    Filter:=_('Template files (*.po;*.pot)|*.po;*.pot|All files (*.*)|*.*');
    if Execute then EditTemplate.Text:=FileName;
    end;
  end;

function TfrmMerge.GetGenerator : string;
begin
  Result:=ChangeFileExt(ExtractFileName(Application.ExeName),'')+' '+GetProgVersion;
  end;

// from former unit "msgmergedxengine"
function TfrmMerge.MergeTranslation (const translationfilename,templatefilename,outputfilename : string) : boolean;
var
  translist : TPoEntryList;
  pe,petr   : TPoEntry;
  parser    : TPoParser;
  PoHeader  : TPoHeader;
  tf        : TextFile;
  fs        : TFileStream;
  scd,s     : string;
  n         : integer;
begin
  FileMode:=fmOpenRead;
  translist:=TPoEntryList.Create;
  Result:=true;
  try
    n:=translist.LoadFromFile(translationfilename);
    translist.SimMeasure:=udSimLength.Position;
    if n>0 then begin
      ShowMessagePos(Format(_('Error in line %u of file "%s"'),[n,translationfilename]),wx,wy);
      Result:=false;
      end;
    if Result then begin
      scd:=CurrentTimestamp;
      AssignFile (tf,templatefilename,CodePage);    // read UTF-8 or selected encoding
      Reset (tf);
      try
        parser:=TPoParser.Create;
        try
          fs:=TFileStream.Create (outputfilename,fmCreate);
          try
            while true do begin
              pe:=parser.ReadNextEntry(tf);  // template
              if pe=nil then break;
              if pe.MsgId.IsEmpty then with PoHeader do begin
                GetFromString(pe.MsgStr);
                if not Items[hiCreationDate].IsEmpty then scd:=Items[hiCreationDate];
                end;
              petr:=translist.FindEntry(pe.MsgId);
              if petr<>nil then begin
                if pe.MsgId.IsEmpty then with translist do begin  // header
                  Header[hiCreationDate]:=scd;
                  Header[hiRevisionDate]:=CurrentTimestamp;
                  Header[hiXGenerator]:=GetGenerator;
                  UpdateHeader(petr);
                  pe.Assign(petr);
                  end
                else begin  // normal entry
                  pe.MsgStr:=petr.MsgStr;
                  pe.Fuzzy:=petr.Fuzzy;
                  if not cbMergeAutoComments.Checked then begin
                    pe.AutoCommentList.Clear; pe.HistCommentList.Clear;
                    end;
                  pe.UserCommentList.Text:=petr.UserCommentList.Text;
                  end;
                petr.Merged:=true;
                end
              else if not pe.MsgId.IsEmpty then begin
                if cbMergeSimilar.Checked then begin
                  petr:=translist.FindSoundEntry(pe.MsgId); // check for case independent msgid
                  if petr<>nil then begin
                    pe.MsgStr:=petr.MsgStr;
                    pe.Fuzzy:=true;
                    if not cbMergeAutoComments.Checked then begin
                      pe.AutoCommentList.Clear; pe.HistCommentList.Clear;
                      end;
                    pe.UserCommentList.Text:=petr.UserCommentList.Text;
                    petr.Merged:=true;
                    end;
                  end;
                end;
              pe.WriteToStream(fs);
              end;
            if cbMergeHistory.Checked then begin
            // check for obsolete history comments
              petr:=translist.FindFirst;
              while (petr<>nil) do begin
                with petr do if (copy(MsgId,1,2)=HistMarker) then begin
                  if (HistCommentList.Count>0) then begin
                    s:=Trim(HistCommentList[0].Substring(3));
                    if AnsiStartsText('MSGID',s) then begin
                      delete(s,1,5);
                      s:=AnsiDequotedStr(Trim(s),'"');
                      if translist.IsEntry(s) then Merged:=true;
                      end;
                    end;
                  end;
                petr:=translist.FindNext(petr);
                end;
            // copy history comments
              petr:=translist.FindFirst;
              while (petr<>nil) do begin
                with petr do if not Merged then begin   // check for entries no longer used in template
                  if (copy(MsgId,1,2)=HistMarker) then WriteToStream(fs)
                  else begin
                    HistCommentList.Add('#~ msgid '+String2PO(MsgId));
                    HistCommentList.Add('#~ msgstr '+String2PO(MsgStr));
                    s:=MsgId;
                    MsgId:=HistMarker+MsgId;
                    WriteToStream(fs,false);
                    MsgId:=s;
                    end;
                  end;
                petr:=translist.FindNext(petr);
                end;
              end;
          finally
            FreeAndNil (fs);
            end;
        finally
          FreeAndNil (parser);
          end;
      finally
        CloseFile (tf);
        end;
      end;
  finally
    FreeAndNil (translist);
    end;
  end;

procedure TfrmMerge.btnHelpClick(Sender: TObject);
begin
  ShowHelp('basics.html#merge');
  end;

procedure TfrmMerge.btnManualClick(Sender: TObject);
begin
  ShowManual(Application.Handle);
  end;

procedure TfrmMerge.btnMergeClick(Sender: TObject);
var
//  res : integer;
  translation,translationbackup,
  template,tempfilename : string;
  ok : boolean;
begin
  screen.cursor:=crHourglass;
  template:=EditTemplate.Text;
  try
    if not fileexists(template) then
      raise Exception.Create (_('The specified template file does not exist.'));
    translation:=ExpandFileName(EditTranslation.Text);
    if not FileExists(translation) then begin // copy header after merging
      try
        CopyFileTS(template,translation); // Init translation
      except
        Exit;
        end;
      end;
//    if not fileexists(translation) then
//      raise Exception.Create (_('The specified translation file does not exist.'));
    tempfilename:=ChangeFileExt(translation,'.pox');
//    if rgEncoding.ItemIndex>0 then begin
// Always use internal function.
      ok:=MergeTranslation(translation,template,tempfilename);
//      end
//    else begin
//      // ASCII only. Use external msgmerge.exe
//      // Check if translation starts with BOM - remove it because msgmerge cannot process it
//      if CheckUtf8Bom(translation)<0 then with TStringList.Create do begin
//        Sorted:=false; WriteBOM:=false;
//        LoadFromFile(translation); SaveToFile(translation);
//        Free;
//        end;
////      AppOutput:=TStringList.Create;
////      try
//      res:=StartProgram('msgmerge.exe','-q "'+translation+'" "'+template+'" -o "'+tempfilename,
//        extractfilepath(paramstr(0)),ErrorOutput);
//      if res<0 then ShowMessagePos(Format(_('Execution of "%s" failed:'),['msgmerge.exe'])
//        +sLineBreak+SysErrorMessage(abs(res)),wx,wy)
//      else if res>0 then begin
////        ShowMessagePos('"msgmerge.exe"'+_(' reports some errors, no merging has been done!'),wx,wy);
//        ShowStringListDialog.Execute(Caption,'"msgmerge.exe"'+_(' reports some errors, no merging has been done!'),ErrorOutput)
//        end;
//      ok:=res=0;
//      end;
    if ok then begin
      translationbackup:=changefileext(translation,'.~po');
      if Fileexists (translationbackup) then
        Deletefile (translationbackup);
      if not RenameFile(translation,translationbackup) then
        raise Exception.Create (Format(_('Cannot rename %s to %s'),[translation,translationbackup]));
      if not RenameFile(tempfilename,translation) then
        raise Exception.Create (Format(_('Cannot rename %s to %s'),[tempfilename,translation]));
      if fileexists(tempfilename) then
        deletefile (tempfilename);
      if not cbCreateBackup.Checked then
        deletefile (translationbackup);
      if not noedit and (MessageDlgPos(_('The template was merged into the translation file.'+sLineBreak+
                      'Do you want to open the translation file now?'+sLineBreak+
                      '(This requires you to have a .po file editor installed)'),
                      mtConfirmation,[mbYes,mbNo],0,wx,wy)=mrYes) then
        ShellExecute (Application.Handle,'open',PChar(translation),nil,nil,SW_RESTORE);
      end;
    if cbSaveSettings.Checked then begin
      with TMemIniFile.Create (ChangeFileExt(translation,'.ini')) do begin
        WriteInteger(ggmSect,iniLeft,Left);       // JR
        WriteInteger(ggmSect,iniTop,Top);
        WriteString(ggmSect,iniTempl,ExtractRelativePath(IncludeTrailingPathDelimiter(GetCurrentDir),template));
        WriteBool(ggmSect,iniBackup,cbCreateBackup.Checked);
        WriteInteger(ggmSect,iniSimLen,udSimLength.Position);
        WriteBool(ggmSect,iniSimilar,cbMergeSimilar.Checked);
        WriteBool(ggmSect,iniAuto,cbMergeAutoComments.Checked);
        WriteBool(ggmSect,iniHist,cbMergeHistory.Checked);
        WriteInteger(ggmSect,iniCode,CodePage);
//        with rgEncoding do begin
//          WriteBool(ggmSect,iniSAscii,ItemIndex<>1);
//          WriteBool(ggmSect,iniCode,ItemIndex=0);
//          end;
        UpdateFile;
        Free;
        end;
      end;
    Close;
  finally
    screen.cursor:=crDefault;
  end;
end;

end.

