program ggdxgettext;
(**************************************************************)
(*                                                            *)
(*  (C) Copyright by Lars B. Dybdahl                          *)
(*  E-mail: Lars@dybdahl.dk, phone +45 70201241               *)
(*  You may distribute and modify this file as you wish       *)
(*  for free                                                  *)
(*                                                            *)
(*  See http://dybdahl.dk/dxgettext/ for more information     *)
(*                                                            *)
(**************************************************************)

//   calling: ggdxgettext.exe <filename> [options]
//         <filename>    - Name of basic directory or file to be scanned
//         /dom:<domain> - use this domain instead of default

// Changes: J. Rathlev, D-24222 Schwentinental
// Last modified: January 2026 

uses
  GnuGetText in 'Units\GnuGetText.pas',
  LangUtils in 'Units\LangUtils.pas',
  SVGIconItems in 'SVG\SVGIconItems.pas',
  SVGIconImage in 'SVG\SVGIconImage.pas',
  ImageLoader in 'Units\ImageLoader.pas',
  Vcl.Forms,
  Vcl.Graphics,
  GgtConsts in 'GgtConsts.pas',
  GetTextMain in 'GetTextMain.pas' {frmGetText},
  GetTextConfig in 'GetTextConfig.pas' {frmConfig},
  ShowMessageDlg in 'Dialogs-SVG\ShowMessageDlg.pas' {$R *.res},
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

  if paramcount=0 then begin
    ErrorDialog(_('This program needs one parameter, which must be a directory path or a file name'));
    exit;
  end;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
//  Application.ShowMainForm:=false;
  Application.CreateForm(TfrmGetText, frmGetText);
  Application.Run;
end.
