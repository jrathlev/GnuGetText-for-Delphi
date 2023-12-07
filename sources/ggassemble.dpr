(* Delphi program
   embed translations into exe file
   © Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.

   July 2016
   *)

program ggassemble;

uses
  GnuGetText in 'units\GnuGetText.pas',
  Vcl.Forms,
  Vcl.Dialogs,
  AssembleMain in 'AssembleMain.pas' {frmAssemble};

{$R *.res}
{$IFDEF WIN32}
  {$R *-32.res}
{$ELSE}
  {$R *-64.res}
{$ENDIF}

begin
  AddDomains(['delphi10','units']);

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmAssemble, frmAssemble);
  Application.Run;
end.
