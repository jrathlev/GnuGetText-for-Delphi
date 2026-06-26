(* Delphi unit
   Message Dialogs
   ===============
   Messages are accessible to screenreaders (uses "TStaticText")

   Note: Parameter "Msg" can hold a caption string separated by "|"

   © J. Rathlev, D.24222 Schwentinental (kontakt(a)rathlev-home.de))

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.
   
   Vers. 2 - July 2022
   Vers. 2 - Jan. 2025: uses SVG images for buttons and icons
                        https://github.com/EtheaDev/SVGIconImageList

   last modified: July 2025
   *)

unit ShowMessageDlg;

interface

uses Winapi.Windows, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Forms,
  Vcl.Controls, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls, System.ImageList,
  Vcl.ImgList, Vcl.Dialogs, SVGIconImage, SVGIconImageListBase, SVGIconImageList,
  JrButtons;

type
  TMsgDialogType = (mdtInformation,mdtWarning,mdtError,mdtConfirm,mdtConfirmError,
    mdtConfirmRetry,mdtConfirmCancel,mdtConfirmYesNo,mdtCustom);

  TShowMsgDialog = class(TForm)
    Timer: TTimer;
    stInfo: TStaticText;
    imlGlyphs: TSVGIconImageList;
    imgIcon: TSVGIconImage;
    imlIcons: TSVGIconImageList;
    btClose: TJrButton;
    btCenter: TJrButton;
    btLeft: TJrButton;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TimerTimer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormAfterMonitorDpiChanged(Sender: TObject; OldDPI,
      NewDPI: Integer);
  private
    { Private declarations }
    RunTimer : boolean;
    FDlgType : TMsgDialogType;
    FCaption : string;
  public
    { Public declarations }
    function Execute (const APos : TPoint; const AHint : string; Delay : integer = 0;
                      DlgType : TMsgDialogType= mdtCustom; DefaultButton : TMsgDlgBtn = mbYes;
                      AMonitor : TDefaultMonitor = dmActiveForm) : TModalResult;
  end;

procedure ShowHintInfo (const AHint : string; DlgType : TMsgDialogType = mdtCustom);

procedure ErrorDialog (const Msg : string; Delay : integer = 0); overload;
procedure ErrorDialog (const Pos : TPoint; const Msg : string;
                       AMonitor : TDefaultMonitor = dmActiveForm); overload;
procedure ErrorDialog (const Pos : TPoint; const Msg : string; Delay : integer;
                       AMonitor : TDefaultMonitor = dmActiveForm); overload;

procedure InfoDialog (const Msg : string; Delay : integer = 0); overload;
procedure InfoDialog (const Pos : TPoint; const Msg : string;
                      AMonitor : TDefaultMonitor = dmActiveForm); overload;
procedure InfoDialog (const Pos : TPoint; const Msg : string; Delay : integer;
                      AMonitor : TDefaultMonitor = dmActiveForm); overload;

function ConfirmDialog (const Msg : string) : boolean; overload;     // Yes - No
function ConfirmDialog (const Pos : TPoint; const Msg : string; DefaultButton : TMsgDlgBtn = mbYes;
                        AMonitor : TDefaultMonitor = dmActiveForm) : boolean; overload;

function ConfirmCancelDialog (const Msg : string) : boolean; overload;  // Yes - Cancel
function ConfirmCancelDialog (const Pos : TPoint; const Msg : string; DefaultButton : TMsgDlgBtn = mbCancel;
                        AMonitor : TDefaultMonitor = dmActiveForm) : boolean; overload;

function ConfirmErrorDialog (const Msg : string) : boolean; overload;   // Yes - Cancel
function ConfirmErrorDialog (const Pos : TPoint; const Msg : string; DefaultButton : TMsgDlgBtn = mbCancel;
                       AMonitor : TDefaultMonitor = dmActiveForm) : boolean; overload;

function ConfirmRetryDialog (const Msg : string) : boolean; overload;   // Retry - Cancel
function ConfirmRetryDialog (const Pos : TPoint; const Msg : string; DefaultButton : TMsgDlgBtn = mbRetry;
                       AMonitor : TDefaultMonitor = dmActiveForm) : boolean; overload;

function ConfirmYesNoDialog (const Msg : string) : TModalResult; overload; // Yes - No - Cancel
function ConfirmYesNoDialog (const Pos : TPoint; const Msg : string; DefaultButton : TMsgDlgBtn = mbNo;
                        AMonitor : TDefaultMonitor = dmActiveForm) : TModalResult; overload;

//function OldMessageDialog(const Msg: string; DlgType: TMsgDlgType;
//                       Buttons: TMsgDlgButtons) : integer; overload;
//function OldMessageDialog(const Pos : TPoint; const Msg: string; DlgType: TMsgDlgType;
//                       Buttons: TMsgDlgButtons) : integer;  overload;

var
  ShowMsgDialog: TShowMsgDialog;

{ ---------------------------------------------------------------- }
implementation

{$R *.DFM}

uses GnuGetText, System.DateUtils, System.Math, Vcl.Consts, WinUtils, StringUtils, ImageLoader;

{ ---------------------------------------------------------------- }
procedure TShowMsgDialog.FormCreate(Sender: TObject);
begin
  TranslateComponent (self,'dialogs-svg');
  ImageLoader.LoadImages('dialogs',[imlGlyphs.SVGIconItems,imlIcons.SVGIconItems]);
  imlGlyphs.DPIChanged(self,PixelsPerInchOnDesign,Monitor.PixelsPerInch);
  imlIcons.DPIChanged(self,PixelsPerInchOnDesign,Monitor.PixelsPerInch);
  end;

procedure TShowMsgDialog.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if RunTimer or (Key=VK_F11) then ModalResult:=mrCancel;
  end;

procedure TShowMsgDialog.FormShow(Sender: TObject);
var
  tw,th,cw,ch : integer;
  cSize       : TSize;
begin
  Timer.Enabled:=RunTimer;
  cw:=btCenter.Width+btClose.Width;
  if FDlgType=mdtConfirmYesNo then cw:=cw+btLeft.Width;
  if FDlgType=mdtCustom then ch:=stInfo.Top
  else ch:=imgIcon.Height;
  cSize:=GetMaxTextExtent(FCaption,stInfo.Font);
  tw:=Max(cw,cSize.Width);
  th:=Max(ch,cSize.Height);
  ClientWidth:=2*stInfo.Top+stInfo.Left+tw;
  stInfo.Caption:=FCaption;
  stInfo.Height:=th;
  ClientHeight:=2*stInfo.Top+th+btClose.Height;
  FitToScreen(Screen,self);
  end;

procedure TShowMsgDialog.FormAfterMonitorDpiChanged(Sender: TObject; OldDPI,
  NewDPI: Integer);
begin
  imlGlyphs.DPIChanged(Sender,OldDPI,NewDPI);
  imlIcons.DPIChanged(Sender,OldDPI,NewDPI);
  end;

procedure TShowMsgDialog.TimerTimer(Sender: TObject);
begin
  ModalResult:=mrCancel;
  end;

const
  Captions: array[TMsgDlgType] of Pointer = (@SMsgDlgWarning, @SMsgDlgError,
    @SMsgDlgInformation, @SMsgDlgConfirm, nil);

type
  TGlyphType = (gtClose,gtCancel,gtOk,gtNo,gtRetry);

{ ---------------------------------------------------------------- }
// Delay > 0: close automatically after Delay seconds
function TShowMsgDialog.Execute (const APos : TPoint; const AHint : string; Delay : integer;
                                 DlgType : TMsgDialogType; DefaultButton : TMsgDlgBtn;
                                 AMonitor : TDefaultMonitor) : TModalResult;
var
  n,tw        : integer;
  sh,sc       : string;
  dt          : TMsgDlgType;
begin
  if length(AHint)=0 then Exit;
  DefaultMonitor:=AMonitor;
  btLeft.Visible:=false; btCenter.Visible:=false;
  with btClose do begin
    Visible:=true; Cancel:=true; Default:=false;
    ModalResult:=mrCancel;
    end;
  ActiveControl:=stInfo; //btClose;
  case DlgType of
  mdtWarning,
  mdtInformation   : begin
                     if DlgType=mdtInformation then dt:=mtInformation else dt:=mtWarning;
                     with btClose do begin
                       Caption:=SMsgDlgClose;
                       Default:=true;
                       ImageIndex:=integer(gtClose)
                       end;
                     end;
  mdtError         : begin
                     dt:=mtError;
                     with btClose do begin
                       Caption:=SMsgDlgClose;
                       Default:=true;
                       ImageIndex:=integer(gtClose)
                       end;
                     end;
  mdtConfirm,
  mdtConfirmCancel : begin
                     dt:=mtConfirmation;
                     with btCenter do begin
                       Visible:=true;
                       Caption:=SMsgDlgYes;
                       ImageIndex:=integer(gtOk);
                       Default:=DefaultButton=mbYes;
                       ModalResult:=mrYes;
                       end;
                     with btClose do begin
                       if DlgType=mdtConfirm then begin
                         Caption:=SMsgDlgNo;
                         Default:=DefaultButton=mbNo;
                         ImageIndex:=integer(gtNo);
                         ModalResult:=mrNo;
                         end
                       else begin
                         Caption:=SMsgDlgCancel;
                         ImageIndex:=integer(gtCancel);
                         ModalResult:=mrCancel;
                         end;
                       end;
                     end;
  mdtConfirmError  : begin
                     dt:=mtError;
                     with btCenter do begin
                       Visible:=true;
                       Caption:=SMsgDlgYes;
                       ImageIndex:=integer(gtOk);
                       Default:=DefaultButton=mbYes;
                       ModalResult:=mrOk;
                       end;
                     with btClose do begin
                       Caption:=SMsgDlgCancel;
                       ImageIndex:=integer(gtCancel);
                       end;
                     end;
  mdtConfirmRetry  : begin
                     dt:=mtError;
                     with btCenter do begin
                       Visible:=true;
                       Caption:=SMsgDlgRetry;
                       ImageIndex:=integer(gtRetry);
                       Default:=DefaultButton=mbRetry;
                       ModalResult:=mrRetry;
                       end;
                     with btClose do begin
                       Caption:=SMsgDlgCancel;
                       ImageIndex:=integer(gtCancel);
                       end;
                     end;
  mdtConfirmYesNo : begin
                     dt:=mtConfirmation;
                     with btLeft do begin
                       Visible:=true;
                       Caption:=SMsgDlgYes;
                       ImageIndex:=integer(gtOk);
                       Default:=DefaultButton=mbYes;
                       ModalResult:=mrYes;
                       end;
                     with btCenter do begin
                       Visible:=true;
                       Caption:=SMsgDlgNo;
                       ImageIndex:=integer(gtNo);
                       Default:=DefaultButton=mbNo;
                       ModalResult:=mrNo;
                       end;
                     with btClose do begin
                       Caption:=SMsgDlgCancel;
                       ImageIndex:=integer(gtCancel);
                       end;
                     end;
  else               begin
                     dt:=mtCustom;
                     btClose.Visible:=false;
                     end;
    end;
  with imgIcon do if DlgType=mdtCustom then Hide
  else begin
    ImageIndex:=integer(dt);
    Show;
    end;
  n:=pos(VertBar,AHint);
  if n>0 then begin
    sc:=copy(AHint,1,n-1); sh:=copy(AHint,n+1,length(AHint));
    end
  else begin
    sh:=AHint; sc:='';
    end;
  RunTimer:=(DlgType=mdtCustom) or (Delay>0);
  if RunTimer then begin
    if sc.IsEmpty then Caption:=LoadResString(Captions[dt])
    else Caption:=sc;
    if Delay=0 then tw:=2000+120*length(sh) else tw:=1000*Delay;
    Timer.Interval:=tw;
    end
  else begin
    if sc.IsEmpty then Caption:=LoadResString(Captions[dt])
    else Caption:=sc;
    end;
  FCaption:=sh; FDlgType:=DlgType;
  AdjustFormPosition(Screen,self,APos);
  Result:=ShowModal;
  Timer.Enabled:=false;
  end;

function MsgDialog (const APos : TPoint; const AHint : string; Delay : integer = 0;
                    DlgType : TMsgDialogType = mdtCustom; DefaultButton : TMsgDlgBtn = mbYes;
                    AMonitor : TDefaultMonitor = dmActiveForm) : TModalResult;
begin
  if not assigned(ShowMsgDialog) then ShowMsgDialog:=TShowMsgDialog.Create(Application);
  Result:=ShowMsgDialog.Execute(APos,AHint,Delay,DlgType,DefaultButton,AMonitor);
  FreeAndNil(ShowMsgDialog);
  end;

procedure ShowHintInfo (const AHint : string; DlgType : TMsgDialogType);
begin
  MsgDialog(CenterPos,AHint,0,DlgType);
  end;

{ ------------------------------------------------------------------- }
// replacement for dialogs from WinUtils for better accessibility, e.g. Screenreader
procedure ErrorDialog (const Msg : string; Delay : integer);
begin
  MsgDialog(CenterPos,Msg,Delay,mdtError);
  end;

procedure ErrorDialog (const Pos : TPoint; const Msg : string;
                       AMonitor : TDefaultMonitor = dmActiveForm); overload;
begin
  MsgDialog(Pos,Msg,0,mdtError,mbCancel,AMonitor);
  end;

procedure ErrorDialog (const Pos : TPoint; const Msg : string; Delay : integer; AMonitor : TDefaultMonitor);
begin
  MsgDialog(Pos,Msg,Delay,mdtError,mbCancel,AMonitor);
  end;

procedure InfoDialog (const Msg : string; Delay : integer);
begin
  MsgDialog(CenterPos,Msg,Delay,mdtInformation);
  end;

procedure InfoDialog (const Pos : TPoint; const Msg : string;
                      AMonitor : TDefaultMonitor = dmActiveForm); overload;
begin
  MsgDialog(Pos,Msg,0,mdtInformation,mbCancel,AMonitor);
  end;

procedure InfoDialog (const Pos : TPoint; const Msg : string; Delay : integer; AMonitor : TDefaultMonitor);
begin
  MsgDialog(Pos,Msg,Delay,mdtInformation,mbCancel,AMonitor);
  end;

function ConfirmDialog (const Msg : string) : boolean;
begin
  Result:=MsgDialog(CenterPos,Msg,0,mdtConfirm)=mrYes;
  end;

function ConfirmDialog (const Pos : TPoint; const Msg : string; DefaultButton : TMsgDlgBtn;
                        AMonitor : TDefaultMonitor) : boolean;
begin
  Result:=MsgDialog(Pos,Msg,0,mdtConfirm,DefaultButton,AMonitor)=mrYes;
  end;

function ConfirmCancelDialog (const Msg : string) : boolean; overload;
begin
  Result:=MsgDialog(CenterPos,Msg,0,mdtConfirmCancel)=mrCancel;
  end;

function ConfirmCancelDialog (const Pos : TPoint; const Msg : string; DefaultButton : TMsgDlgBtn;
                        AMonitor : TDefaultMonitor) : boolean; overload;
begin
  Result:=MsgDialog(Pos,Msg,0,mdtConfirmCancel,DefaultButton,AMonitor)=mrCancel;
  end;

function ConfirmErrorDialog (const Msg : string) : boolean; overload;
begin
  Result:=MsgDialog(CenterPos,Msg,0,mdtConfirmError)=mrOK;
  end;

function ConfirmErrorDialog (const Pos : TPoint; const Msg : string; DefaultButton : TMsgDlgBtn;
                       AMonitor : TDefaultMonitor) : boolean; overload;
begin
  Result:=MsgDialog(Pos,Msg,0,mdtConfirmError,DefaultButton,AMonitor)=mrOK;
  end;

function ConfirmRetryDialog (const Msg : string) : boolean; overload;
begin
  Result:=MsgDialog(CenterPos,Msg,0,mdtConfirmRetry)=mrRetry;
  end;

function ConfirmRetryDialog (const Pos : TPoint; const Msg : string; DefaultButton : TMsgDlgBtn;
                       AMonitor : TDefaultMonitor) : boolean; overload;
begin
  Result:=MsgDialog(Pos,Msg,0,mdtConfirmRetry,DefaultButton,AMonitor)=mrRetry;
  end;

function ConfirmYesNoDialog (const Msg : string) : TModalResult;
begin
  Result:=MsgDialog(CenterPos,Msg,0,mdtConfirmYesNo);
  end;

function ConfirmYesNoDialog (const Pos : TPoint; const Msg : string; DefaultButton : TMsgDlgBtn;
                        AMonitor : TDefaultMonitor) : TModalResult;
begin
  Result:=MsgDialog(Pos,Msg,0,mdtConfirmYesNo,DefaultButton,AMonitor);
  end;

end.
