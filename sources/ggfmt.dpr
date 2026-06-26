(* Delphi program
   VCL interface to msgfmt and msgunfmt
   based on the "dxgettext" programs by Lars B. Dybdahl

   © Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.

   July 2016
   Last modified: July 2023: new command line options

   Command line:
   -------------
   Calling:  ggfmt <filename> [options]
     <filename>  - Name of file to be converted
                   Default: Generate binary message catalog from textual translation description.
     /u          - Convert binary message catalog to .po file
                   Default extension of output file: .po
     /e:<ext>    - Extension of output file if other than default
   *)

program GgFmt;

uses
  GnuGetText in 'Units\GnuGetText.pas',
  LangUtils in 'Units\LangUtils.pas',
  SVGIconItems in 'SVG\SVGIconItems.pas',
  SVGIconImage in 'SVG\SVGIconImage.pas',
  ImageLoader in 'Units\ImageLoader.pas',
  Vcl.Forms,
  Vcl.Graphics,
  Vcl.Styles,
  GgtConsts in 'GgtConsts.pas',
  GgFmtMain in 'GgFmtMain.pas' {frmMain},
  ShowText in 'dialogs-svg\ShowText.pas' {ShowtextDialog};

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
  Application.CreateForm(TShowtextDialog, ShowtextDialog);
  Application.Run;
end.
