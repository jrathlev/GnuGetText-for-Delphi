program PoFromPas;

uses
  GnuGetText in 'Units\GnuGetText.pas',
  LangUtils in 'Units\LangUtils.pas',
  SVGIconItems in 'SVG\SVGIconItems.pas',
  SVGIconImage in 'SVG\SVGIconImage.pas',
  ImageLoader in 'Units\ImageLoader.pas',
  Vcl.Forms,
  Vcl.Graphics,
  GgtConsts in 'GgtConsts.pas',
  PoFromPasMain in 'PoFromPasMain.pas' {frmMain},
  ShellDirDlg in 'dialogs-svg\ShellDirDlg.pas' {ShellDirDialog},
  Vcl.Themes,
  Vcl.Styles;

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
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TShellDirDialog, ShellDirDialog);
  Application.Run;
end.
