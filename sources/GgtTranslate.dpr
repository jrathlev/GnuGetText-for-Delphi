program GgtTranslate;

uses
  GnuGetText in 'units\GnuGetText.pas',
  LangUtils in 'units\LangUtils.pas',
  Forms,
  Graphics,
  TransMain in 'TransMain.pas' {frmTransMain},
  ShellDirDlg in 'units\ShellDirDlg.pas' {ShellDirDialog},
  SelectListItems in 'units\SelectListItems.pas' {SelectListItemsDialog},
  FileListDlg in 'units\FileListDlg.pas' {FileListDialog},
  SelectDlg in 'units\SelectDlg.pas' {SelectDialog},
  ShowText in 'units\ShowText.pas' {ShowtextDialog},
  EditHistListDlg in 'units\EditHistListDlg.pas' {EditHistListDialog},
  ListSelectDlg in 'units\ListSelectDlg.pas' {ListSelectDialog},
  PoStatDlg in 'PoStatDlg.pas' {PoStatDialog},
  GgtConsts in 'GgtConsts.pas';

{$R *.res}
{$IFDEF WIN32}
  {$R *-32.res}
{$ELSE}
  {$R *-64.res}
{$ENDIF}

begin
  TP_GlobalIgnoreClass(TFont);
  // Subdirectory in AppData for user configuration files and supported languages
  InitTranslation(DefIniPath,'',['delphi10','units']);

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTransMain, frmTransMain);
  Application.CreateForm(TShellDirDialog, ShellDirDialog);
  Application.CreateForm(TSelectListItemsDialog, SelectListItemsDialog);
  Application.CreateForm(TFileListDialog, FileListDialog);
  Application.CreateForm(TSelectDialog, SelectDialog);
  Application.CreateForm(TShowtextDialog, ShowtextDialog);
  Application.CreateForm(TListSelectDialog, ListSelectDialog);
  Application.CreateForm(TPoStatDialog, PoStatDialog);
  Application.Run;
end.
