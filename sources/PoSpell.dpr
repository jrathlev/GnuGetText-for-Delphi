program PoSpell;

uses
  GnuGetText in 'units\GnuGetText.pas',
  LangUtils in 'units\LangUtils.pas',
  Vcl.Forms,
  Vcl.Graphics,
  Vcl.Dialogs,
  GgtConsts in 'GgtConsts.pas',
  PoSpellMain in 'PoSpellMain.pas' {frmMain},
  SpellChecker in 'SpellChecker.pas',
  ListSelectDlg in 'units\ListSelectDlg.pas' {ListSelectDialog};

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

  if not HunspellDllLoaded then begin
    MessageDlg(_('Error loading Hunspell library!'),mtError,[mbClose],0);
    Exit;
    end;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TListSelectDialog, ListSelectDialog);
  Application.Run;
end.
