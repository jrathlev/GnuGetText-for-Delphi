program PoCompare;

uses
  GnuGetText in 'Units\GnuGetText.pas',
  LangUtils in 'Units\LangUtils.pas',
  SVGIconItems in 'SVG\SVGIconItems.pas',
  SVGIconImage in 'SVG\SVGIconImage.pas',
  ImageLoader in 'Units\ImageLoader.pas',
  Vcl.Forms,
  Vcl.Graphics,
  GgtConsts in 'GgtConsts.pas',
  PoCompMain in 'PoCompMain.pas' {frmMain},
  EditStringListDlg in 'dialogs-svg\EditStringListDlg.pas' {EditStringListDialog},
  EditHistListDlg in 'Dialogs-svg\EditHistListDlg.pas' {EditHistListDialog},
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
  Application.CreateForm(TEditStringListDialog, EditStringListDialog);
  Application.CreateForm(TEditHistListDialog, EditHistListDialog);
  Application.Run;
end.
