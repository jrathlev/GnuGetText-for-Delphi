program PoStat;

uses
  GnuGetText in 'units\GnuGetText.pas',
  LangUtils in 'units\LangUtils.pas',
  Vcl.Forms,
  Vcl.Graphics,
  GgtConsts in 'GgtConsts.pas',
  PoStatMain in 'PoStatMain.pas' {frmMain};

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
  Application.Run;
end.
