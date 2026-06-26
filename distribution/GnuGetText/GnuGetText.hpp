// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GnuGetText.pas' rev: 30.00 (Windows)

#ifndef GnugettextHPP
#define GnugettextHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.Classes.hpp>
#include <System.StrUtils.hpp>
#include <System.SysUtils.hpp>
#include <System.TypInfo.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gnugettext
{
//-- forward type declarations -----------------------------------------------
struct TLanguageTableEntry;
class DELPHICLASS EGnuGettext;
class DELPHICLASS EGGProgrammingError;
class DELPHICLASS EGGComponentError;
class DELPHICLASS EGGIOError;
class DELPHICLASS EGGAnsi2WideConvError;
class DELPHICLASS TMoFile;
class DELPHICLASS TDomain;
class DELPHICLASS TExecutable;
__interface IGnuGettextInstanceWhenNewLanguageListener;
typedef System::DelphiInterface<IGnuGettextInstanceWhenNewLanguageListener> _di_IGnuGettextInstanceWhenNewLanguageListener;
class DELPHICLASS TGnuGettextInstance;
//-- type declarations -------------------------------------------------------
struct DECLSPEC_DRECORD TLanguageTableEntry
{
public:
	System::UnicodeString FileName;
	System::UnicodeString ResourceId;
};


typedef System::RawByteString RawUtf8String;

typedef System::UnicodeString DomainString;

typedef System::UnicodeString LanguageString;

typedef System::UnicodeString ComponentNameString;

typedef System::UnicodeString FilenameString;

typedef System::UnicodeString MsgIdString;

typedef System::UnicodeString TranslatedUnicodeString;

typedef void __fastcall (__closure *TTranslator)(System::TObject* obj);

#pragma pack(push,4)
class PASCALIMPLEMENTATION EGnuGettext : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EGnuGettext(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EGnuGettext(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EGnuGettext(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EGnuGettext(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EGnuGettext(NativeUInt Ident, System::TVarRec const *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EGnuGettext(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EGnuGettext(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EGnuGettext(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGnuGettext(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGnuGettext(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGnuGettext(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGnuGettext(NativeUInt Ident, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EGnuGettext(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION EGGProgrammingError : public EGnuGettext
{
	typedef EGnuGettext inherited;
	
public:
	/* Exception.Create */ inline __fastcall EGGProgrammingError(const System::UnicodeString Msg) : EGnuGettext(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EGGProgrammingError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High) : EGnuGettext(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EGGProgrammingError(NativeUInt Ident)/* overload */ : EGnuGettext(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EGGProgrammingError(System::PResStringRec ResStringRec)/* overload */ : EGnuGettext(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EGGProgrammingError(NativeUInt Ident, System::TVarRec const *Args, const int Args_High)/* overload */ : EGnuGettext(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EGGProgrammingError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High)/* overload */ : EGnuGettext(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EGGProgrammingError(const System::UnicodeString Msg, int AHelpContext) : EGnuGettext(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EGGProgrammingError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High, int AHelpContext) : EGnuGettext(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGGProgrammingError(NativeUInt Ident, int AHelpContext)/* overload */ : EGnuGettext(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGGProgrammingError(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : EGnuGettext(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGGProgrammingError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : EGnuGettext(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGGProgrammingError(NativeUInt Ident, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : EGnuGettext(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EGGProgrammingError(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION EGGComponentError : public EGnuGettext
{
	typedef EGnuGettext inherited;
	
public:
	/* Exception.Create */ inline __fastcall EGGComponentError(const System::UnicodeString Msg) : EGnuGettext(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EGGComponentError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High) : EGnuGettext(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EGGComponentError(NativeUInt Ident)/* overload */ : EGnuGettext(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EGGComponentError(System::PResStringRec ResStringRec)/* overload */ : EGnuGettext(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EGGComponentError(NativeUInt Ident, System::TVarRec const *Args, const int Args_High)/* overload */ : EGnuGettext(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EGGComponentError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High)/* overload */ : EGnuGettext(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EGGComponentError(const System::UnicodeString Msg, int AHelpContext) : EGnuGettext(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EGGComponentError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High, int AHelpContext) : EGnuGettext(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGGComponentError(NativeUInt Ident, int AHelpContext)/* overload */ : EGnuGettext(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGGComponentError(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : EGnuGettext(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGGComponentError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : EGnuGettext(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGGComponentError(NativeUInt Ident, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : EGnuGettext(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EGGComponentError(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION EGGIOError : public EGnuGettext
{
	typedef EGnuGettext inherited;
	
public:
	/* Exception.Create */ inline __fastcall EGGIOError(const System::UnicodeString Msg) : EGnuGettext(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EGGIOError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High) : EGnuGettext(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EGGIOError(NativeUInt Ident)/* overload */ : EGnuGettext(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EGGIOError(System::PResStringRec ResStringRec)/* overload */ : EGnuGettext(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EGGIOError(NativeUInt Ident, System::TVarRec const *Args, const int Args_High)/* overload */ : EGnuGettext(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EGGIOError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High)/* overload */ : EGnuGettext(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EGGIOError(const System::UnicodeString Msg, int AHelpContext) : EGnuGettext(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EGGIOError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High, int AHelpContext) : EGnuGettext(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGGIOError(NativeUInt Ident, int AHelpContext)/* overload */ : EGnuGettext(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGGIOError(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : EGnuGettext(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGGIOError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : EGnuGettext(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGGIOError(NativeUInt Ident, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : EGnuGettext(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EGGIOError(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION EGGAnsi2WideConvError : public EGnuGettext
{
	typedef EGnuGettext inherited;
	
public:
	/* Exception.Create */ inline __fastcall EGGAnsi2WideConvError(const System::UnicodeString Msg) : EGnuGettext(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EGGAnsi2WideConvError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High) : EGnuGettext(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EGGAnsi2WideConvError(NativeUInt Ident)/* overload */ : EGnuGettext(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EGGAnsi2WideConvError(System::PResStringRec ResStringRec)/* overload */ : EGnuGettext(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EGGAnsi2WideConvError(NativeUInt Ident, System::TVarRec const *Args, const int Args_High)/* overload */ : EGnuGettext(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EGGAnsi2WideConvError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High)/* overload */ : EGnuGettext(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EGGAnsi2WideConvError(const System::UnicodeString Msg, int AHelpContext) : EGnuGettext(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EGGAnsi2WideConvError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High, int AHelpContext) : EGnuGettext(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGGAnsi2WideConvError(NativeUInt Ident, int AHelpContext)/* overload */ : EGnuGettext(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGGAnsi2WideConvError(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : EGnuGettext(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGGAnsi2WideConvError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : EGnuGettext(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGGAnsi2WideConvError(NativeUInt Ident, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : EGnuGettext(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EGGAnsi2WideConvError(void) { }
	
};

#pragma pack(pop)

typedef void __fastcall (__closure *TOnDebugLine)(System::TObject* Sender, const System::UnicodeString Line, bool &Discard);

typedef int __fastcall (*TGetPluralForm)(int Number);

typedef void __fastcall (__closure *TDebugLogger)(System::AnsiString line);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TMoFile : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	bool doswap;
	
public:
	int Users;
	__fastcall TMoFile(const System::UnicodeString filename, const __int64 Offset, __int64 Size, const bool xUseMemoryMappedFiles);
	__fastcall virtual ~TMoFile(void);
	System::RawByteString __fastcall gettext(const System::RawByteString msgid, bool &found);
	__property bool isSwappedArchitecture = {read=doswap, nodefault};
	
private:
	unsigned N;
	unsigned O;
	unsigned T;
	int startindex;
	int startstep;
	bool FUseMemoryMappedFiles;
	NativeUInt mo;
	NativeUInt momapping;
	char *momemoryHandle;
	char *momemory;
	unsigned __fastcall autoswap32(unsigned i);
	unsigned __fastcall CardinalInMem(char * baseptr, unsigned Offset);
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TDomain : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	bool Enabled;
	System::UnicodeString vDirectory;
	void __fastcall setDirectory(const System::UnicodeString dir);
	
public:
	TDebugLogger DebugLogger;
	System::UnicodeString Domain;
	__property System::UnicodeString Directory = {read=vDirectory, write=setDirectory};
	__fastcall TDomain(void);
	__fastcall virtual ~TDomain(void);
	void __fastcall SetLanguageCode(const System::UnicodeString langcode);
	void __fastcall SetFilename(const System::UnicodeString filename);
	void __fastcall GetListOfLanguages(System::Classes::TStrings* list);
	System::UnicodeString __fastcall GetTranslationProperty(System::UnicodeString Propertyname);
	System::RawByteString __fastcall gettext(const System::RawByteString msgid);
	
private:
	TMoFile* mofile;
	System::UnicodeString SpecificFilename;
	System::UnicodeString curlang;
	bool OpenHasFailedBefore;
	void __fastcall OpenMoFile(void);
	void __fastcall CloseMoFile(void);
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TExecutable : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	virtual void __fastcall Execute(void) = 0 ;
public:
	/* TObject.Create */ inline __fastcall TExecutable(void) : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TExecutable(void) { }
	
};

#pragma pack(pop)

__interface IGnuGettextInstanceWhenNewLanguageListener  : public System::IInterface 
{
	virtual void __fastcall WhenNewLanguage(const System::UnicodeString LanguageID) = 0 ;
};

class PASCALIMPLEMENTATION TGnuGettextInstance : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TOnDebugLine fOnDebugLine;
	
public:
	bool Enabled;
	int DesignTimeCodePage;
	__fastcall TGnuGettextInstance(void);
	__fastcall virtual ~TGnuGettextInstance(void);
	void __fastcall UseLanguage(System::UnicodeString LanguageCode);
	void __fastcall GetListOfLanguages(const System::UnicodeString domain, System::Classes::TStrings* list);
	virtual System::UnicodeString __fastcall gettext(const System::UnicodeString szMsgId)/* overload */;
	System::UnicodeString __fastcall gettext_NoExtract(const System::UnicodeString szMsgId);
	System::UnicodeString __fastcall gettext_NoOp(const System::UnicodeString szMsgId);
	virtual System::UnicodeString __fastcall ngettext(const System::UnicodeString singular, const System::UnicodeString plural, int Number)/* overload */;
	System::UnicodeString __fastcall ngettext_NoExtract(const System::UnicodeString singular, const System::UnicodeString plural, int Number);
	System::UnicodeString __fastcall GetCurrentLanguage(void);
	System::UnicodeString __fastcall GetTranslationProperty(const System::UnicodeString Propertyname);
	System::UnicodeString __fastcall GetTranslatorNameAndEmail(void);
	void __fastcall TP_Ignore(System::TObject* AnObject, const System::UnicodeString name);
	void __fastcall TP_IgnoreClass(System::TClass IgnClass);
	void __fastcall TP_IgnoreClassProperty(System::TClass IgnClass, System::UnicodeString propertyname);
	void __fastcall TP_GlobalIgnoreClass(System::TClass IgnClass);
	void __fastcall TP_GlobalIgnoreClassProperty(System::TClass IgnClass, System::UnicodeString propertyname);
	void __fastcall TP_GlobalHandleClass(System::TClass HClass, TTranslator Handler);
	void __fastcall TranslateProperties(System::TObject* AnObject, System::UnicodeString textdomain = System::UnicodeString());
	void __fastcall TranslateComponent(System::Classes::TComponent* AnObject, const System::UnicodeString TextDomain = System::UnicodeString());
	void __fastcall RetranslateComponent(System::Classes::TComponent* AnObject, const System::UnicodeString TextDomain = System::UnicodeString());
	virtual System::UnicodeString __fastcall dgettext(const System::UnicodeString szDomain, const System::UnicodeString szMsgId)/* overload */;
	System::UnicodeString __fastcall dgettext_NoExtract(const System::UnicodeString szDomain, const System::UnicodeString szMsgId);
	virtual System::UnicodeString __fastcall dngettext(const System::UnicodeString szDomain, const System::UnicodeString singular, const System::UnicodeString plural, int Number)/* overload */;
	System::UnicodeString __fastcall dngettext_NoExtract(const System::UnicodeString szDomain, const System::UnicodeString singular, const System::UnicodeString plural, int Number);
	void __fastcall textdomain(const System::UnicodeString szDomain);
	System::UnicodeString __fastcall getcurrenttextdomain(void);
	void __fastcall bindtextdomain(const System::UnicodeString szDomain, const System::UnicodeString szDirectory);
	void __fastcall bindtextdomainToFile(const System::UnicodeString szDomain, const System::UnicodeString filename);
	System::UnicodeString __fastcall LoadResString(System::PResStringRec ResStringRec);
	void __fastcall DebugLogToFile(const System::UnicodeString filename, bool append = false);
	void __fastcall DebugLogPause(bool PauseEnabled);
	__property TOnDebugLine OnDebugLine = {read=fOnDebugLine, write=fOnDebugLine};
	void __fastcall RegisterWhenNewLanguageListener(_di_IGnuGettextInstanceWhenNewLanguageListener Listener);
	void __fastcall UnregisterWhenNewLanguageListener(_di_IGnuGettextInstanceWhenNewLanguageListener Listener);
	
protected:
	void __fastcall TranslateStrings(System::Classes::TStrings* sl, const System::UnicodeString TextDomain);
	virtual void __fastcall WhenNewLanguage(const System::UnicodeString LanguageID);
	virtual void __fastcall WhenNewDomain(const System::UnicodeString TextDomain);
	virtual void __fastcall WhenNewDomainDirectory(const System::UnicodeString TextDomain, const System::UnicodeString Directory);
	
private:
	System::UnicodeString curlang;
	TGetPluralForm curGetPluralForm;
	System::UnicodeString curmsgdomain;
	System::Sysutils::TMultiReadExclusiveWriteSynchronizer* savefileCS;
	System::TextFile savefile;
	System::Classes::TStringList* savememory;
	System::UnicodeString DefaultDomainDirectory;
	System::Classes::TStringList* domainlist;
	System::Classes::TStringList* TP_IgnoreList;
	System::Classes::TList* TP_ClassHandling;
	System::Classes::TList* TP_GlobalClassHandling;
	TExecutable* TP_Retranslator;
	System::Classes::TInterfaceList* fWhenNewLanguageListeners;
	TExecutable* __fastcall TP_CreateRetranslator(void);
	void __fastcall FreeTP_ClassHandlingItems(void);
	void __fastcall TranslateProperty(System::TObject* AnObject, System::Typinfo::PPropInfo PropInfo, System::Classes::TStrings* TodoList, const System::UnicodeString TextDomain);
	TDomain* __fastcall Getdomain(const System::UnicodeString domain, const System::UnicodeString DefaultDomainDirectory, const System::UnicodeString CurLang);
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE System::DynamicArray<TLanguageTableEntry> GLanguageTable;
#define DefaultTextDomain L"default"
extern DELPHI_PACKAGE System::UnicodeString ExecutableFilename;
static const bool PreferExternal = true;
static const bool UseMemoryMappedFiles = true;
static const bool ReReadMoFileOnSameLanguage = true;
#define VCSVersion L"$LastChangedRevision: 220 $"
static const bool AutoCreateHooks = true;
extern DELPHI_PACKAGE TGnuGettextInstance* DefaultInstance;
extern DELPHI_PACKAGE System::UnicodeString __fastcall gettext(const System::UnicodeString szMsgId);
extern DELPHI_PACKAGE System::UnicodeString __fastcall gettext_NoExtract(const System::UnicodeString szMsgId);
extern DELPHI_PACKAGE System::UnicodeString __fastcall gettext_NoOp(const System::UnicodeString szMsgId);
extern DELPHI_PACKAGE System::UnicodeString __fastcall _(const System::UnicodeString szMsgId);
extern DELPHI_PACKAGE System::UnicodeString __fastcall dgettext(const System::UnicodeString szDomain, const System::UnicodeString szMsgId);
extern DELPHI_PACKAGE System::UnicodeString __fastcall dgettext_NoExtract(const System::UnicodeString szDomain, const System::UnicodeString szMsgId);
extern DELPHI_PACKAGE System::UnicodeString __fastcall dngettext(const System::UnicodeString szDomain, const System::UnicodeString singular, const System::UnicodeString plural, int Number);
extern DELPHI_PACKAGE System::UnicodeString __fastcall ngettext(const System::UnicodeString singular, const System::UnicodeString plural, int Number);
extern DELPHI_PACKAGE System::UnicodeString __fastcall ngettext_NoExtract(const System::UnicodeString singular, const System::UnicodeString plural, int Number);
extern DELPHI_PACKAGE void __fastcall textdomain(const System::UnicodeString szDomain);
extern DELPHI_PACKAGE System::UnicodeString __fastcall getcurrenttextdomain(void);
extern DELPHI_PACKAGE void __fastcall bindtextdomain(const System::UnicodeString szDomain, const System::UnicodeString szDirectory);
extern DELPHI_PACKAGE void __fastcall TP_Ignore(System::TObject* AnObject, const System::UnicodeString name);
extern DELPHI_PACKAGE void __fastcall TP_GlobalIgnoreClass(System::TClass IgnClass);
extern DELPHI_PACKAGE void __fastcall TP_IgnoreClass(System::TClass IgnClass);
extern DELPHI_PACKAGE void __fastcall TP_IgnoreClassProperty(System::TClass IgnClass, const System::UnicodeString propertyname);
extern DELPHI_PACKAGE void __fastcall TP_GlobalIgnoreClassProperty(System::TClass IgnClass, const System::UnicodeString propertyname);
extern DELPHI_PACKAGE void __fastcall TP_GlobalHandleClass(System::TClass HClass, TTranslator Handler);
extern DELPHI_PACKAGE void __fastcall TranslateComponent(System::Classes::TComponent* AnObject, const System::UnicodeString TextDomain = System::UnicodeString());
extern DELPHI_PACKAGE void __fastcall RetranslateComponent(System::Classes::TComponent* AnObject, const System::UnicodeString TextDomain = System::UnicodeString());
extern DELPHI_PACKAGE System::UnicodeString __fastcall GetTranslatorNameAndEmail(void);
extern DELPHI_PACKAGE void __fastcall UseLanguage(const System::UnicodeString LanguageCode);
extern DELPHI_PACKAGE System::UnicodeString __fastcall GetCurrentLanguage(void);
extern DELPHI_PACKAGE System::WideString __fastcall LoadResString(System::PResStringRec ResStringRec);
extern DELPHI_PACKAGE System::UnicodeString __fastcall LoadResStringW(System::PResStringRec ResStringRec);
extern DELPHI_PACKAGE void __fastcall AddDomainForResourceString(const System::UnicodeString domain);
extern DELPHI_PACKAGE void __fastcall RemoveDomainForResourceString(const System::UnicodeString domain);
extern DELPHI_PACKAGE void __fastcall AddDomains(System::UnicodeString const *Domains, const int Domains_High);
extern DELPHI_PACKAGE void __fastcall AddDomainForComponent(const System::UnicodeString domain);
extern DELPHI_PACKAGE void __fastcall RemoveDomainForComponent(const System::UnicodeString domain);
extern DELPHI_PACKAGE void __fastcall HookIntoResourceStrings(bool enabled = true, bool SupportPackages = false);
}	/* namespace Gnugettext */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GNUGETTEXT)
using namespace Gnugettext;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GnugettextHPP
