(* GunGetText for Delphi
   =====================
   common types and constants

   © Dr. J. Rathlev, D-24222 Schwentinental (pb(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.

   J. Rathlev, September 2023
   last modified: September 2023
   *)

unit GgtConsts;

interface

const
  DefIniPath  = 'GgtTranslate\';

  CopRgt = '© 2010 - 2024 Dr. J. Rathlev, 24222 Schwentinental';
  EmailAdr = 'kontakt(a)rathlev-home.de';

// Code pages
  cpAscii = 20127;
  cpLatin1 = 1252;
  CpIso8859 = 28591;
  cpUtf8 = 65001;

  PoExt = 'po';
  PoxExt = 'pox';
  MoExt = 'mo';
  IniExt = 'ini';
  PasExt = 'pas';
  DfmExt = 'dfm';

  PoEdit = 'poedit.exe';
  DxGetTextIni = 'dxgettext.ini';
  XGetTextExe = 'xgettext.exe';

  sManual = 'manual.pdf';

  defDelphiMask = '*.pas *.dfm *.inc *.rc *.xfm *.dpr';
  defLazarusMask = '*.pas *.lfm *.inc *.rc *.lpr';
  defCppMask = '*.cpp *.c *.dfm *.inc';

  (* INI-Sektionen *)
  CfgSekt  = 'Config';
  DirSekt  = 'Directories';
  FileSekt = 'Files';
  TransSekt = 'Translations';
  TemplSekt = 'Templates';

  (* INI-Variablen *)
  iniTop      = 'Top';
  iniLeft     = 'Left';
  IniWidth    = 'Width';
  IniHeight   = 'Height';
  iniTempl    = 'Template';
  iniTrans    = 'Translation';
  iniRecurse  = 'recurse';
  iniExclude  = 'exclude';
  iniDefault  = 'nodefault';
  iniUpdate   = 'updateignore';
  iniIgnore   = 'useignore';
  iniAAscii   = 'allownonascii';
  iniOutput   = 'output';
  iniMask     = 'mask';
  iniOvwr     = 'overwrite';
  iniOrder    = 'sort';
  iniCode     = 'CodePage';

  sIgnoreList = 'IgnoreObjects.txt';

implementation

end.
