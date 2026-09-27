program FileList;

{$R 'languages.rc'}

uses
  LazGetText in '..\units\LazGetText.pas',
  LazLangUtils in '..\units\LazLangUtils.pas',
  Forms,
  Graphics, Interfaces,
  FListMain in 'FListMain.pas' {HauptForm},
  FileFilterDlg in '..\dialogs\FileFilterDlg.pas' {FilterDialog};

{$R *.RES}

begin
  TP_GlobalIgnoreClass(TFont);
  // No subdirectory in AppData for user configuration files and supported languages
  InitTranslation(['lclstrconsts','units']);

  Application.Initialize;
  Application.CreateForm(THauptForm, HauptForm);
  Application.CreateForm(TFileFilterDialog, FileFilterDialog);
  Application.Run;
end.
