program PoCompare;

uses
  GnuGetText in 'units\GnuGetText.pas',
  LangUtils in 'units\LangUtils.pas',
  Forms,
  Graphics,
  GgtConsts in 'GgtConsts.pas',
  PoCompMain in 'PoCompMain.pas' {frmMain},
  EditStringListDlg in 'units\EditStringListDlg.pas' {EditStringListDialog};

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
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TEditStringListDialog, EditStringListDialog);
  Application.Run;
end.
