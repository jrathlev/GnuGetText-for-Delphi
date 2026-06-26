program GgtTranslate;

uses
  GnuGetText in 'Units\GnuGetText.pas',
  LangUtils in 'Units\LangUtils.pas',
  SVGIconItems in 'SVG\SVGIconItems.pas',
  SVGIconImage in 'SVG\SVGIconImage.pas',
  ImageLoader in 'Units\ImageLoader.pas',
  Vcl.Forms,
  Vcl.Graphics,
  Vcl.Themes,
  Vcl.Styles,
  TransMain in 'TransMain.pas' {frmTransMain},
  ShellDirDlg in 'dialogs-svg\ShellDirDlg.pas' {ShellDirDialog},
  SelectListItems in 'dialogs-svg\SelectListItems.pas' {SelectListItemsDialog},
  FileListDlg in 'dialogs-svg\FileListDlg.pas' {FileListDialog},
  ShowText in 'dialogs-svg\ShowText.pas' {ShowtextDialog},
  EditHistListDlg in 'dialogs-svg\EditHistListDlg.pas' {EditHistListDialog},
  PoStatDlg in 'PoStatDlg.pas' {PoStatDialog},
  GgtConsts in 'GgtConsts.pas',
  EditStringListDlg in 'Dialogs-SVG\EditStringListDlg.pas' {EditStringListDialog},
  EditDirListDlg in 'dialogs-svg\EditDirListDlg.pas' {EditDirListDialog};

{$R *.res}
{$IFDEF WIN32}
  {$R *-32.res}
{$ELSE}
  {$R *-64.res}
{$ENDIF}

begin
  TP_GlobalIgnoreClass(TFont);
  TP_GlobalIgnoreClass(TSVGIconItem);
  TP_GlobalIgnoreClassProperty(TSVGIconImage,'SVGText');
  // Subdirectory in AppData for user configuration files and supported languages
  InitTranslation(DefIniPath,GgtConfigName,['delphi10','units']);
  InitImageLoader('images',['dialogs']);

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTransMain, frmTransMain);
  Application.CreateForm(TShellDirDialog, ShellDirDialog);
  Application.CreateForm(TSelectListItemsDialog, SelectListItemsDialog);
  Application.CreateForm(TFileListDialog, FileListDialog);
  Application.CreateForm(TShowtextDialog, ShowtextDialog);
  Application.CreateForm(TPoStatDialog, PoStatDialog);
  Application.CreateForm(TEditStringListDialog, EditStringListDialog);
  Application.CreateForm(TEditDirListDialog, EditDirListDialog);
  Application.Run;
end.
