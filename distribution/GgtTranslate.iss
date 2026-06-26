; Setup script for Gnu Gettext Translate
; ======================================
;  © Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de)

;  The contents of this file may be used under the terms of the
;  Mozilla Public License ("MPL") or
;  GNU Lesser General Public License Version 2 or later (the "LGPL")

;  Software distributed under this License is distributed on an "AS IS" basis,
;  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
;  the specific language governing rights and limitations under the License.

#define ApplicationVersion GetVersionNumbersString('..\Release\Win32\GgtTranslate.exe')
#define Year GetDateTimeString('yyyy','','')
#define GgtName "Gnu Gettext for Delphi"
#define ProgramName "GgtTranslate"
#define ProgramAuthor "Dr. J. Rathlev, 24222 Schwentinental, Germany"
#define ProgramWebURL "http://www.rathlev-home.de/?tools/progtools.html"
#define OutputFile "ggt-translate-setup-4"

[Setup]
PrivilegesRequired=admin
AppName={#ProgramName}
AppVerName={#GgtName}
AppVersion={#GgtName} {#ApplicationVersion}
AppPublisher={#ProgramAuthor}
AppPublisherURL={#ProgramWebURL}
AppSupportURL={#ProgramWebURL}
AppUpdatesURL={#ProgramWebURL}
AppCopyright=© 2014-{#Year} {#ProgramAuthor}
VersionInfoVersion={#ApplicationVersion}
VersionInfoDescription={#GgtName} Setup
DefaultDirName={autopf}\Delphi GetText
DefaultGroupName={#GgtName}
AllowNoIcons=yes
OutputDir=.
OutputBaseFilename={#OutputFile}
SetupIconFile=Translate.ico
UninstallDisplayIcon={app}\Translate-u.ico
WizardImageFile=install-left.bmp
WizardSmallImageFile=..\..\Common\Install-small.bmp
WizardStyle=modern dynamic 
Compression=lzma2
SolidCompression=yes
DisableWelcomePage=no
DisableDirPage=auto
DisableProgramGroupPage=auto

[Languages]
Name: "en"; MessagesFile: "compiler:Default.isl"; LicenseFile:"..\..\Common\license-en.rtf"; InfoBeforeFile:"..\docs\translate.rtf";
Name: "de"; MessagesFile: "compiler:Languages\German.isl"; LicenseFile:"..\..\Common\license-de.rtf"; InfoBeforeFile:"..\docs\translate.rtf";

[CustomMessages]
en.FileAssoc=File associations:
en.DescContext=Add "Delphi GetText" functions in context menus
en.ButtonDesign=Select button design for all users:
en.SimpleButtons=Use simple icons
en.PoTools=&Po tools
en.CompileContext=&Compile to mo file ..
en.MergeContext=&Merge with template ..
en.DecompContext=&Decompile to po file ..
en.EmbedContext=Embed &translations ..
en.TemplateContext=&Create translation template ..
en.ShowStatContext=Show &statistics ..
en.SpellCheckContext=Spell &check ..
en.PoCompContext=&Compare ..
en.ImportContext=&Import ..
en.ConvertMessages=&Convert CustomMessages to Pascal ..
en.IssContext=Convert for &InnoSetup script

de.FileAssoc=Dateizuordnungen:
de.DescContext=Füge "Delphi GetText"-Funktionen zu den Kontext-Menüs hinzu
de.ButtonDesign=Design der Schaltflächen f+r alle Benutzer:
de.SimpleButtons=Einfache Icons verwenden
de.PoTools=Po-Werkzeuge
de.CompileContext=in MO-Datei &übersetzen ..
de.MergeContext=mit Schablone &zusammenführen ..
de.DecompContext=in PO-Datei &zurückführen ..
de.EmbedContext=&Übersetzungen integrieren ..
de.TemplateContext=&Übersetzungs-Schablone erstellen ..
de.ShowStatContext=&Statistik anzeigen ..
de.SpellCheckContext=&Rechtschreibprüfung ..
de.PoCompContext=&Vergleichen ..
de.ImportContext=&Importieren ..
de.ConvertMessages=CustomMessages nach Pascal &konvertieren
de.IssContext=Für &InnoSetup-Skript konvertieren

[Tasks]
Name: "fileassoc"; Description: "{cm:DescContext}"; GroupDescription: "{cm:FileAssoc}"; 
; Name: "simpledesign"; Description: "{cm:SimpleButtons}"; GroupDescription: "{cm:ButtonDesign}"; Flags: unchecked;

; refer to: https://blog.sverrirs.com/2014/05/creating-cascading-menu-items-in.html
[Registry]
Root: HKCR; Subkey: "{code:GetKey|PO}\Shell\Po.Tools"; Flags: deletekey
Root: HKCR; Subkey: "{code:GetKey|PO}\Shell\Po.Tools"; ValueType: string; ValueName: ""; ValueData: ""; Tasks: fileassoc; Flags: deletevalue uninsdeletekey
Root: HKCR; Subkey: "{code:GetKey|PO}\Shell\Po.Tools"; ValueType: string; ValueName: "MUIVerb"; ValueData: "{cm:PoTools}"; Tasks: fileassoc; Flags: deletevalue
Root: HKCR; Subkey: "{code:GetKey|PO}\Shell\Po.Tools"; ValueType: string; ValueName: "Subcommands"; ValueData: ""; Tasks: fileassoc; Flags: deletevalue
Root: HKCR; Subkey: "{code:GetKey|PO}\Shell\Po.Tools"; ValueType: string; ValueName: "Icon"; ValueData: """{app}\ggttranslate.exe"""; Tasks: fileassoc; Flags: deletevalue

Root: HKCR; Subkey: "{code:GetKey|PO}\Shell\Po.Tools\Shell"; ValueType: string; ValueName: ""; ValueData: ""; Tasks: fileassoc; Flags: deletevalue 
Root: HKCR; Subkey: "{code:GetKey|PO}\Shell\Po.Tools\Shell\a.Merge template"; ValueType: string; ValueName: ""; ValueData: "{cm:MergeContext}"; Tasks: fileassoc; Flags: deletevalue
Root: HKCR; Subkey: "{code:GetKey|PO}\Shell\Po.Tools\Shell\a.Merge template"; ValueType: string; ValueName: "Icon"; ValueData: """{app}\ggmerge.exe"""; Tasks: fileassoc; Flags: deletevalue
Root: HKCR; Subkey: "{code:GetKey|PO}\Shell\Po.Tools\Shell\a.Merge template\Command"; ValueType: string; ValueName: ""; ValueData: """{app}\ggmerge.exe"" ""%1"""; Tasks: fileassoc; Flags: deletevalue

Root: HKCR; Subkey: "{code:GetKey|PO}\shell\Po.Tools\Shell\b.Compile to mo file"; ValueType: string; ValueName: ""; ValueData: "{cm:CompileContext}"; Tasks: fileassoc; Flags: deletevalue
Root: HKCR; Subkey: "{code:GetKey|PO}\shell\Po.Tools\Shell\b.Compile to mo file"; ValueType: string; ValueName: "Icon"; ValueData: """{app}\ggfmt.exe"""; Tasks: fileassoc; Flags: deletevalue
Root: HKCR; Subkey: "{code:GetKey|PO}\shell\Po.Tools\Shell\b.Compile to mo file\Command"; ValueType: string; ValueName: ""; ValueData: """{app}\ggfmt.exe"" ""%1"""; Tasks: fileassoc; Flags: deletevalue

Root: HKCR; Subkey: "{code:GetKey|PO}\shell\Po.Tools\Shell\c.Compare"; ValueType: string; ValueName: ""; ValueData: "{cm:PoCompContext}"; Tasks: fileassoc; Flags: deletevalue
Root: HKCR; Subkey: "{code:GetKey|PO}\shell\Po.Tools\Shell\c.Compare"; ValueType: string; ValueName: "Icon"; ValueData: """{app}\pocompare.exe"""; Tasks: fileassoc; Flags: deletevalue
Root: HKCR; Subkey: "{code:GetKey|PO}\shell\Po.Tools\Shell\c.Compare\Command"; ValueType: string; ValueName: ""; ValueData: """{app}\pocompare.exe"" ""%1"""; Tasks: fileassoc; Flags: deletevalue

Root: HKCR; Subkey: "{code:GetKey|PO}\shell\Po.Tools\Shell\d.Import"; ValueType: string; ValueName: ""; ValueData: "{cm:ImportContext}"; Tasks: fileassoc; Flags: deletevalue
Root: HKCR; Subkey: "{code:GetKey|PO}\shell\Po.Tools\Shell\d.Import"; ValueType: string; ValueName: "Icon"; ValueData: """{app}\poimport.exe"""; Tasks: fileassoc; Flags: deletevalue
Root: HKCR; Subkey: "{code:GetKey|PO}\shell\Po.Tools\Shell\d.Import\Command"; ValueType: string; ValueName: ""; ValueData: """{app}\poimport.exe"" ""%1"""; Tasks: fileassoc; Flags: deletevalue

Root: HKCR; Subkey: "{code:GetKey|PO}\shell\Po.Tools\Shell\e.Spell check"; ValueType: string; ValueName: ""; ValueData: "{cm:SpellCheckContext}"; Tasks: fileassoc; Flags: deletevalue
Root: HKCR; Subkey: "{code:GetKey|PO}\shell\Po.Tools\Shell\e.Spell check"; ValueType: string; ValueName: "Icon"; ValueData: """{app}\pospell.exe"""; Tasks: fileassoc; Flags: deletevalue
Root: HKCR; Subkey: "{code:GetKey|PO}\shell\Po.Tools\Shell\e.Spell check\Command"; ValueType: string; ValueName: ""; ValueData: """{app}\pospell.exe"" ""%1"""; Tasks: fileassoc; Flags: deletevalue

Root: HKCR; Subkey: "{code:GetKey|PO}\shell\Po.Tools\Shell\f.Show statistics"; ValueType: string; ValueName: ""; ValueData: "{cm:ShowStatContext}"; Tasks: fileassoc; Flags: deletevalue
Root: HKCR; Subkey: "{code:GetKey|PO}\shell\Po.Tools\Shell\f.Show statistics"; ValueType: string; ValueName: "Icon"; ValueData: """{app}\postat.exe"""; Tasks: fileassoc; Flags: deletevalue
Root: HKCR; Subkey: "{code:GetKey|PO}\shell\Po.Tools\Shell\f.Show statistics\Command"; ValueType: string; ValueName: ""; ValueData: """{app}\postat.exe"" ""%1"""; Tasks: fileassoc; Flags: deletevalue

Root: HKCR; Subkey: "{code:GetKey|PO}\shell\Po.Tools\Shell\g.Convert to ISS"; ValueType: string; ValueName: ""; ValueData: "{cm:IssContext}"; Tasks: fileassoc; Flags: deletevalue
Root: HKCR; Subkey: "{code:GetKey|PO}\shell\Po.Tools\Shell\g.Convert to ISS"; ValueType: string; ValueName: "Icon"; ValueData: """{app}\potoiss.exe"""; Tasks: fileassoc; Flags: deletevalue
Root: HKCR; Subkey: "{code:GetKey|PO}\shell\Po.Tools\Shell\g.Convert to ISS\Command"; ValueType: string; ValueName: ""; ValueData: """{app}\potoiss.exe"" ""%1"""; Tasks: fileassoc; Flags: deletevalue

Root: HKCR; Subkey: "{code:GetKey|MO}\shell\Decompile to po file"; ValueType: string; ValueName: ""; ValueData: "{cm:DeCompContext}"; Tasks: fileassoc; Flags: deletevalue uninsdeletekey
Root: HKCR; Subkey: "{code:GetKey|MO}\shell\Decompile to po file"; ValueType: string; ValueName: "Icon"; ValueData: """{app}\ggfmt.exe"""; Tasks: fileassoc; Flags: deletevalue
Root: HKCR; Subkey: "{code:GetKey|MO}\shell\Decompile to po file\Command"; ValueType: string; ValueName: ""; ValueData: """{app}\ggfmt.exe"" ""%1"" -u"; Tasks: fileassoc; Flags: deletevalue

Root: HKCR; Subkey: "{code:GetKey|EXE}\shell\Embed translations"; ValueType: string; ValueName: ""; ValueData: "{cm:EmbedContext}"; Tasks: fileassoc; Flags: deletevalue uninsdeletekey
Root: HKCR; Subkey: "{code:GetKey|EXE}\shell\Embed translations"; ValueType: string; ValueName: "Icon"; ValueData: """{app}\ggassemble.exe"""; Tasks: fileassoc; Flags: deletevalue
Root: HKCR; Subkey: "{code:GetKey|EXE}\shell\Embed translations\Command"; ValueType: string; ValueName: ""; ValueData: """{app}\ggassemble.exe"" ""%1"""; Tasks: fileassoc; Flags: deletevalue

Root: HKCR; Subkey: "{code:GetKey|DLL}\shell\Embed translations"; ValueType: string; ValueName: ""; ValueData: "{cm:EmbedContext}"; Tasks: fileassoc; Flags: deletevalue uninsdeletekey
Root: HKCR; Subkey: "{code:GetKey|DLL}\shell\Embed translations"; ValueType: string; ValueName: "Icon"; ValueData: """{app}\ggassemble.exe"""; Tasks: fileassoc; Flags: deletevalue
Root: HKCR; Subkey: "{code:GetKey|DLL}\shell\Embed translations\Command"; ValueType: string; ValueName: ""; ValueData: """{app}\ggassemble.exe"" ""%1"""; Tasks: fileassoc; Flags: deletevalue

Root: HKCR; Subkey: "{code:GetKey|BPL}\shell\Embed translations"; ValueType: string; ValueName: ""; ValueData: "{cm:EmbedContext}"; Tasks: fileassoc; Flags: deletevalue uninsdeletekey
Root: HKCR; Subkey: "{code:GetKey|BPL}\shell\Embed translations"; ValueType: string; ValueName: "Icon"; ValueData: """{app}\ggassemble.exe"""; Tasks: fileassoc; Flags: deletevalue
Root: HKCR; Subkey: "{code:GetKey|BPL}\shell\Embed translations\Command"; ValueType: string; ValueName: ""; ValueData: """{app}\ggassemble.exe"" ""%1"""; Tasks: fileassoc; Flags: deletevalue

Root: HKCR; Subkey: "{code:GetKey|ISS}\shell\Convert custommessages"; ValueType: string; ValueName: ""; ValueData: "{cm:ConvertMessages}"; Tasks: fileassoc; Flags: deletevalue uninsdeletekey
Root: HKCR; Subkey: "{code:GetKey|ISS}\shell\Convert custommessages"; ValueType: string; ValueName: "Icon"; ValueData: """{app}\isstopas.exe"""; Tasks: fileassoc; Flags: deletevalue
Root: HKCR; Subkey: "{code:GetKey|ISS}\shell\Convert custommessages\Command"; ValueType: string; ValueName: ""; ValueData: """{app}\isstopas.exe"" ""%1"""; Tasks: fileassoc; Flags: deletevalue

Root: HKCR; Subkey: "{code:GetKey|PAS}\shell\Create translation template"; ValueType: string; ValueName: ""; ValueData: "{cm:TemplateContext}"; Tasks: fileassoc; Flags: deletevalue uninsdeletekey
Root: HKCR; Subkey: "{code:GetKey|PAS}\shell\Create translation template"; ValueType: string; ValueName: "Icon"; ValueData: """{app}\ggdxgettext.exe"""; Tasks: fileassoc; Flags: deletevalue
Root: HKCR; Subkey: "{code:GetKey|PAS}\shell\Create translation template\Command"; ValueType: string; ValueName: ""; ValueData: """{app}\ggdxgettext.exe"" ""%1"""; Tasks: fileassoc; Flags: deletevalue

Root: HKCR; Subkey: "{code:GetKey|DFM}\shell\Create translation template"; ValueType: string; ValueName: ""; ValueData: "{cm:TemplateContext}"; Tasks: fileassoc; Flags: deletevalue uninsdeletekey
Root: HKCR; Subkey: "{code:GetKey|DFM}\shell\Create translation template"; ValueType: string; ValueName: "Icon"; ValueData: """{app}\ggdxgettext.exe"""; Tasks: fileassoc; Flags: deletevalue
Root: HKCR; Subkey: "{code:GetKey|DFM}\shell\Create translation template\Command"; ValueType: string; ValueName: ""; ValueData: """{app}\ggdxgettext.exe"" ""%1"""; Tasks: fileassoc; Flags: deletevalue

Root: HKCR; Subkey: "{code:GetKey|DPR}\shell\Create translation template"; ValueType: string; ValueName: ""; ValueData: "{cm:TemplateContext}"; Tasks: fileassoc; Flags: deletevalue uninsdeletekey
Root: HKCR; Subkey: "{code:GetKey|DPR}\shell\Create translation template"; ValueType: string; ValueName: "Icon"; ValueData: """{app}\ggdxgettext.exe"""; Tasks: fileassoc; Flags: deletevalue
Root: HKCR; Subkey: "{code:GetKey|DPR}\shell\Create translation template\Command"; ValueType: string; ValueName: ""; ValueData: """{app}\ggdxgettext.exe"" ""%1"""; Tasks: fileassoc; Flags: deletevalue

Root: HKCR; Subkey: "Folder\shell\Create translation template"; ValueType: string; ValueName: ""; ValueData: "{cm:TemplateContext}"; Tasks: fileassoc; Flags: deletevalue uninsdeletekey
Root: HKCR; Subkey: "Folder\shell\Create translation template"; ValueType: string; ValueName: "Icon"; ValueData: """{app}\ggdxgettext.exe"""; Tasks: fileassoc; Flags: deletevalue
Root: HKCR; Subkey: "Folder\shell\Create translation template\Command"; ValueType: string; ValueName: ""; ValueData: """{app}\ggdxgettext.exe"" ""%1"""; Tasks: fileassoc; Flags: deletevalue

[InstallDelete]
; Type: files; Name: "{app}\GgtTranslate\GgtUtils.cfg";

[Files]
Source: "..\Release\Win32\GgtTranslate.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\Release\Win32\PoCompare.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\Release\Win32\PoComment.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\Release\Win32\PoFromPas.exe"; DestDir: "{app}"; Flags: ignoreversion
; Source: "..\Release\Win32\PoStrip.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\Release\Win32\PoSpell.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\Release\Win32\Hunspellx86.dll"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\Release\Win32\PoImport.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\Release\Win32\PoStat.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\Release\Win32\TransUnit.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\Release\Win32\IssToPas.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\Release\Win32\PoToIss.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\Release\Win32\ggdxgettext.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\Release\Win32\ggmerge.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\Release\Win32\ggfmt.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\Release\Win32\ggassemble.exe"; DestDir: "{app}"; Flags: ignoreversion
; Source: "..\Release\Win32\msgmerge.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\Release\Win32\msgfmt.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\Release\Win32\msgunfmt.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\Release\Win32\xgettext.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\Release\Win32\Lang\languages.*"; DestDir: "{app}\Lang"; Flags: ignoreversion
Source: "..\Release\Win32\locale\*.mo"; DestDir: "{app}\locale"; Flags: recursesubdirs ignoreversion restartreplace
Source: "..\Release\Win32\language.cfg"; DestDir: "{app}"; Flags: recursesubdirs ignoreversion restartreplace
Source: "..\Images\*-plain.zip"; DestDir: "{app}\images"; Flags: ignoreversion
Source: "GnuGetText\GnuGetText.*"; DestDir: "{app}\GnuGetText"; Flags: ignoreversion
Source: "GnuGetText\GnuGetTextInit.*"; DestDir: "{app}\GnuGetText"; Flags: ignoreversion
Source: "GnuGetText\GgtDummy.*"; DestDir: "{app}\GnuGetText"; Flags: ignoreversion
Source: "GnuGetText\CompCpp.bat"; DestDir: "{app}\GnuGetText"; Flags: ignoreversion
Source: "GnuGetText\IgnoreObjects.txt"; DestDir: "{app}"; Flags: ignoreversion
Source: "Dict\de_DE_frami.*"; DestDir: "{app}\Dict"; Flags: ignoreversion
Source: "..\docs\commandline.txt"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\docs\translate.rtf"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\help\translate-*.chm"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\docs\manual.pdf"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\docs\ggexclude.pdf"; DestDir: "{app}"; Flags: ignoreversion
Source: "Samples\*.*"; DestDir: "{app}\Samples"; Flags: recursesubdirs ignoreversion
Source: "Translate-u.ico"; DestDir: "{app}"; Flags: ignoreversion
; Source: "GgtUtils.cfg"; DestDir: "{app}\GgtTranslate"; Tasks: simpledesign; Flags: ignoreversion
; NOTE: Don't use "Flags: ignoreversion" on any shared system files

[Icons]
Name: "{group}\{#GgtName}"; Filename: "{app}\GgtTranslate.exe"; Check: not WizardNoIcons 
Name: "{group}\{cm:UninstallProgram,{#GgtName}}"; Filename: "{uninstallexe}"; IconFilename: "{app}\Translate-u.ico"; Check: not WizardNoIcons

[Code]
function GetKey (Ext : String) : String;
var
  App : String;
begin
  if not RegQueryStringValue(HKEY_CLASSES_ROOT, '.'+Ext, '', App) then begin
    App:=Ext+'file';
    RegWriteStringValue(HKEY_CLASSES_ROOT, '.'+Ext, '', App);
    end;
  Result:=App;
end;
