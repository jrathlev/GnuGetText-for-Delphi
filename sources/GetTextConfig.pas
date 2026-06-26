unit GetTextConfig;
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
(* further development by J. Rathlev

   last modified: April 2026
   *)

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.Buttons, Vcl.Menus, System.ImageList, Vcl.ImgList, SVGIconImageListBase,
  SVGIconImageList, JrButtons;

type
  TfrmConfig = class(TForm)
    EditMask: TLabeledEdit;
    CheckBoxRecurse: TCheckBox;
    EditBasepath: TLabeledEdit;
    CheckBoxSaveSettings: TCheckBox;
    cbCreateIgnore: TCheckBox;
    cbRemoveIgnore: TCheckBox;
    gbDomain: TGroupBox;
    rbDefault: TRadioButton;
    rbOther: TRadioButton;
    OutputName: TLabeledEdit;
    ExcludeDirs: TLabeledEdit;
    laVersion: TLabel;
    cbOverwrite: TCheckBox;
    FileOpenDialog: TFileOpenDialog;
    pmMask: TPopupMenu;
    cbOrder: TCheckBox;
    rgEncoding: TRadioGroup;
    imlGlyphs: TSVGIconImageList;
    ButtonOK: TJrButton;
    ButtonCancel: TJrButton;
    btnHelp: TJrButton;
    btnManual: TJrButton;
    bbBaseDir: TJrSpeedButton;
    bbExclude: TJrSpeedButton;
    btDefMask: TJrSpeedButton;
    laProg: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure rbDefaultClick(Sender: TObject);
    procedure bbBaseDirClick(Sender: TObject);
    procedure bbExcludeClick(Sender: TObject);
    procedure btDefMaskClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure btnManualClick(Sender: TObject);
    procedure FormAfterMonitorDpiChanged(Sender: TObject; OldDPI,
      NewDPI: Integer);
    procedure pmiMeasureItem(Sender: TObject; ACanvas: TCanvas; var Width,
      Height: Integer);
    procedure pmiDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect;
      Selected: Boolean);
    procedure CheckBoxRecurseClick(Sender: TObject);
  private
    { Private declarations }
    PixelsPerInchOnCreate : integer;
    procedure DoMaskClick (Sender : TObject);
    procedure ShowExclude;
  public
    { Public declarations }
    procedure LoadFromIni(const AIniName : string);
    function SaveToIni(const AIniName : string; SaveMask : boolean) : boolean;
    procedure SetDomain(const DomName : string);
  end;

implementation

uses
  System.Inifiles, System.UITypes, gnugettext, GgtConsts, GgtUtils, WinUtils,
  PathUtils, WinShell, ImageLoader, StyleUtils;

{$R *.dfm}

const
  ggtSect = 'ggdxgettext';
  dxSect = 'dxgettext';

  defOutput = 'default';
  Masks : array [0..2] of string =(defDelphiMask,defLazarusMask,defCppMask);

procedure TfrmConfig.FormCreate(Sender: TObject);

  function AddMenuItem (const ACaption,AName : string) : TMenuItem;
  begin
    Result:=NewItem(ACaption,0,false,true,DoMaskClick,0,AName);
    Result.OnDrawItem:=pmiDrawItem;
    Result.OnMeasureItem:=pmiMeasureItem;
    pmMask.Items.Add(Result);
    end;

begin
  TranslateComponent (self);
  ImageLoader.LoadImages([imlGlyphs.SVGIconItems]);
  PixelsPerInchOnCreate:=PixelsPerInch;
  imlGlyphs.DPIChanged(self,PixelsPerInchOnDesign,PixelsPerInch);
  laProg.Caption:='ggdxgettext';
  laVersion.Caption:='Version: '+GetProgVersion;
  AddMenuItem(_('Delphi files'),Format('miMask%u',[0]));
  AddMenuItem(_('Lazarus files'),Format('miMask%u',[1]));
  AddMenuItem(_('Cpp Builder files'),Format('miMask%u',[2]));
  EditMask.Text:='';
  end;

procedure TfrmConfig.LoadFromIni(const AIniName : string);
begin
  with TMemIniFile.Create (AIniName) do begin
    Left:=ReadInteger(ggtSect,iniLeft,Left);       // JR
    Top:=ReadInteger(ggtSect,iniTop,Top);
    CheckBoxRecurse.Checked:=ReadBool(ggtSect,iniRecurse,false);
    ExcludeDirs.Enabled:=CheckBoxRecurse.Checked;
    ExcludeDirs.Text:=ReadString(ggtSect,iniExclude,'');
    cbCreateIgnore.Checked:=ReadBool(ggtSect,iniUpdate,false);
    cbRemoveIgnore.Checked:=ReadBool(ggtSect,iniIgnore,false);
//    cbCheckNonAscii.Checked:=not ReadBool(ggtSect,iniAAscii,true);
    rgEncoding.ItemIndex:=ReadInteger(ggtSect,iniCode,0);
    if ReadBool(ggtSect,iniDefault,False) then begin
      rbOther.Checked:=true;
      with OutputName do begin
        Text:=ReadString(dxSect,iniOutput,defOutput);
        Enabled:=true;
        end;
      end
    else begin
      rbDefault.Checked:=true;
      with OutputName do begin
        Text:=''; Enabled:=false;
        end;
      end;
    EditMask.Text:=ReadString(dxSect,iniMask,defDelphiMask);
    OutputName.Text:=ReadString(dxSect,iniOutput,defOutput);
    cbOrder.Checked:=ReadBool(dxSect,iniOrder,false);
    cbOverwrite.Checked:=ReadBool(dxSect,iniOvwr,false);
    Free;
    end;
  end;

function TfrmConfig.SaveToIni(const AIniName : string; SaveMask : boolean) : boolean;
begin
  Result:=true;
  if CheckBoxSaveSettings.Checked and (length(AIniName)>0) then begin
    with TMemIniFile.Create (AIniName) do begin
      try
        WriteInteger(ggtSect,iniLeft,Left);       // JR
        WriteInteger(ggtSect,iniTop,Top);
        WriteBool(ggtSect,iniRecurse,CheckBoxRecurse.Checked);
        WriteString(ggtSect,iniExclude,ExcludeDirs.Text);
        WriteBool(ggtSect,iniUpdate,cbCreateIgnore.Checked);
        WriteBool(ggtSect,iniIgnore,cbRemoveIgnore.Checked);
  //      WriteBool(ggtSect,iniAAscii,not cbCheckNonAscii.Checked);
        WriteInteger(ggtSect,iniCode,rgEncoding.ItemIndex);
        WriteBool(ggtSect,iniDefault,rbOther.Checked);
        if rbOther.Checked then WriteString(dxSect,iniOutput,OutputName.Text)
        else WriteString(dxSect,iniOutput,'');
        if SaveMask then WriteString(dxSect,iniMask,EditMask.Text);
        WriteBool(dxSect,iniOrder,cbOrder.Checked);
        WriteBool(dxSect,iniOvwr,cbOverwrite.Checked);
        try
          UpdateFile;
        except
          Result:=false;
        end;
      finally
        Free;
        end;
      end;
    end;
  end;

procedure TfrmConfig.ShowExclude;
begin
  bbExclude.Enabled:=CheckBoxRecurse.Checked;
  ExcludeDirs.Enabled:=CheckBoxRecurse.Checked;
  end;

procedure TfrmConfig.SetDomain(const DomName : string);
begin
  if AnsiSameText(DomName,defOutput) then begin
    rbDefault.Checked:=true;
    OutputName.Text:='';
    end
  else begin
    rbOther.Checked:=true;
    OutputName.Text:=DomName;
    end;
  end;

procedure TfrmConfig.pmiDrawItem(Sender: TObject; ACanvas: TCanvas;
  ARect: TRect; Selected: Boolean);
var
  d : integer;
begin
  with ACanvas do begin
    if Selected then Brush.Color:=GetSysColor(clHighlight) else Brush.Color:=GetSysColor(clMenu);
    if (Sender as TMenuItem).Caption=cLineCaption then with ARect do begin
      FillRect(ARect);
      if StylesEnabled then Pen.Color:=GetSysColor(clInActiveBorder) else Pen.Color:=clActiveBorder;
      d:=Top+Height div 2;
      MoveTo(Height,d); LineTo(Width-Height,d);
      end
    else begin
      with Font do begin
        Size:=SizeScale(Size,PixelsPerInchOnCreate,self);
        if Selected then Color:=GetSysColor(clHighlightText) else Color:=GetSysColor(clMenuText);
        end;
      TextRect(ARect,ARect.Height,ARect.Top+MulDiv(ARect.Height,3,22),(Sender as TMenuItem).Caption);
      end;
    end;
  end;

procedure TfrmConfig.pmiMeasureItem(Sender: TObject; ACanvas: TCanvas;
  var Width, Height: Integer);
begin
  Width:=SizeScale(Width,PixelsPerInchOnCreate,self); Height:=SizeScale(Height,PixelsPerInchOnCreate,self);
  end;

procedure TfrmConfig.FormAfterMonitorDpiChanged(Sender: TObject; OldDPI,
  NewDPI: Integer);
begin
  imlGlyphs.DPIChanged(Sender,OldDPI,NewDPI);
  end;

procedure TfrmConfig.FormShow(Sender: TObject);
begin
  if length(EditBasepath.Text)=0 then bbBaseDirClick(self);
  end;

procedure TfrmConfig.rbDefaultClick(Sender: TObject);
begin
  OutputName.Enabled:=rbOther.Checked;
  end;

procedure TfrmConfig.DoMaskClick (Sender : TObject);
var
  s : string;
  n : integer;
begin
  s:=(Sender as TMenuItem).Name;
  system.delete (s,1,6);
  if TryStrToInt(s,n) then EditMask.Text:=Masks[n];
  end;

procedure TfrmConfig.btDefMaskClick(Sender: TObject);
var
  pt : TPoint;
begin
  with btDefMask do pt:=self.ClientToScreen(Point(Left,Top+Height));
  with pt do pmMask.Popup(x,y);
  end;

procedure TfrmConfig.btnHelpClick(Sender: TObject);
begin
  ShowHelp('basics.html#dxgettext');
  end;

procedure TfrmConfig.btnManualClick(Sender: TObject);
var
  pt : TPoint;
begin
  ShowManual(Application.Handle);
  end;

procedure TfrmConfig.CheckBoxRecurseClick(Sender: TObject);
begin
   ShowExclude;
  end;

procedure TfrmConfig.bbBaseDirClick(Sender: TObject);
var
  sd : string;
begin
  sd:=EditBasepath.Text;
  if length(sd)=0 then sd:=GetPersonalFolder;
  with FileOpenDialog do begin
    Title:=_('Select basic directory');
    Options := [fdoPickFolders,fdoForceFileSystem];
    OkButtonLabel:=_('Select');
    DefaultFolder:=sd;
    FileName:=sd;
    if Execute then EditBasepath.Text:=Filename;
    end;
  end;

procedure TfrmConfig.bbExcludeClick(Sender: TObject);
var
  sd : string;
  i  : integer;
begin
  sd:=EditBasepath.Text;
  if length(sd)=0 then sd:=GetPersonalFolder;
  with FileOpenDialog do begin
    Title:=_('Select subdirectories to be excluded');
    Options := [fdoPickFolders,fdoAllowMultiSelect,fdoForceFileSystem];
    OkButtonLabel:=_('Select');
    DefaultFolder:=sd;
    FileName:=sd;
    if Execute then begin
      sd:=MakeRelativePath(EditBasepath.Text,Files[0]);
      for i:=1 to Files.Count-1 do sd:=sd+','+MakeRelativePath(EditBasepath.Text,Files[i]);
      if length(ExcludeDirs.Text)>0 then ExcludeDirs.Text:=ExcludeDirs.Text+',';
      ExcludeDirs.Text:=ExcludeDirs.Text+sd;
      end;
    end;
  end;

end.
