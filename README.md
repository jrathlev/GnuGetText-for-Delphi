### Gnu GetText Tools for Delphi 

[based on **DxGetText** by Lars B. Dybdahl](http://sourceforge.net/projects/dxgettext/)

In addition to the revised original programs, some auxiliary programs with 
Windows GUI are provided.

#### Revised original programs
- **ggdxgettext:** Extract strings from Delphi sources and create po template,  
_new features:_ selectable text domains, excludable subirectories, enhanced command line options
- **ggmerge:** Merge translation with updated template  
_new features:_ selectable files, UTF-8 encoding by default, enhanced command line options
- **ggfmt:** Convert po file to mo file and reverse  
_new features:_ enhanced command line options 
- **ggassemble:** Embed po translations into exe file  
_new features:_ enhanced command line options 
- **GnuGetText** and **GnuGetTextInit:** GNU gettext translation system for integration 
  in Delphi and C++ Builder applications 

#### Windows GUI programs:
- **GgtTranslate**: GUI for translations
- **PoCompare:** Compare and merge two translations
- **PoFromPas:** Insert strings from Delphi units (resourcestring) to po file (msgstr) 
- **TransUnit:** Create translated Delphi unit using strings from po file
- **PoSpell:** Spell checker for translated strings (msgstr) using HunSpell
- **PoStat:** Show po file header and statistics
- **PoComment:** Insert translations (msgstr) as comments to a template
- **PoToIss:** Convert strings from a po file into format used in InnoSetup scripts
- **IssToPas:** Convert [CustomMessages] section of iss file to Delphi unit (resourcestring) 
- **German Translations** for programs and Windows context menus

[Download of Windows executable package](https://www.rathlev-home.de/tools/download/ggt-translate-setup.exe)

[English homepage](https://www.rathlev-home.de/?tools/prog-e.html#gettext) / 
[German homepage](https://www.rathlev-home.de/?tools/progtools.html#language)

#### Notes:
The following programs from the **GnuGetText** package are required
- xgettext.exe
- msgfmt.exe
- msgunfmt.exe

[Sources](https://www.gnu.org/software/gettext/) / [Windows binaries](https://mlocati.github.io/gettext-iconv-windows/) 

