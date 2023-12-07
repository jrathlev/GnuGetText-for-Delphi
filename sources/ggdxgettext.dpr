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

// Last modified: October 2023 J. Rathlev, D-24222 Schwentinental

uses
  GnuGetText in 'units\GnuGetText.pas',
  Vcl.Forms,
  Vcl.Dialogs,
  GetTextConfig in 'GetTextConfig.pas' {frmConfig},
  GetTextMain in 'GetTextMain.pas' {frmGetText};

{$R *.res}
{$IFDEF WIN32}
  {$R *-32.res}
{$ELSE}
  {$R *-64.res}
{$ENDIF}

begin
  AddDomains(['delphi10','units']);

  if paramcount=0 then begin
    MessageDlg(_('This program needs one parameter, which must be a directory path or a file name'),mtError,[mbClose],0);
    exit;
  end;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmGetText, frmGetText);
  Application.Run;
end.
