program TransUnit;

uses
  GnuGetText in 'Units\GnuGetText.pas',
  LangUtils in 'Units\LangUtils.pas',
  SVGIconItems in 'SVG\SVGIconItems.pas',
  SVGIconImage in 'SVG\SVGIconImage.pas',
  ImageLoader in 'Units\ImageLoader.pas',
  Vcl.Forms,
  Vcl.Graphics,
  GgtConsts in 'GgtConsts.pas',
  PoToDfmMain in 'PoToDfmMain.pas' {MainForm},
  ShellFileDlg in 'Dialogs-Svg\ShellFileDlg.pas' {ShellFileDialog},
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
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TShellFileDialog, ShellFileDialog);
  Application.Run;
end.
