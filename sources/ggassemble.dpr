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
   last modified: January 2026
   *)

program ggassemble;

uses
  GnuGetText in 'units\GnuGetText.pas',
  LangUtils in 'units\LangUtils.pas',
  SVGIconItems in 'svg\SVGIconItems.pas',
  SVGIconImage in 'svg\SVGIconImage.pas',
  ImageLoader in 'units\ImageLoader.pas',
  Vcl.Forms,
  Vcl.Graphics,
  Vcl.Styles,
  GgtConsts in 'GgtConsts.pas',
  AssembleMain in 'AssembleMain.pas' {frmAssemble};

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
  Application.CreateForm(TfrmAssemble, frmAssemble);
  Application.Run;
end.
