program PoToIss;

uses
  GnuGetText in 'units\GnuGetText.pas',
  LangUtils in 'units\LangUtils.pas',
  Vcl.Forms,
  Vcl.Graphics,
  GgtConsts in 'GgtConsts.pas',
  PoToIssMain in 'PoToIssMain.pas' {frmMain},
  ShellDirDlg in 'units\ShellDirDlg.pas' {ShellDirDialog};

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
  Application.CreateForm(TShellDirDialog, ShellDirDialog);
  Application.Run;
end.
