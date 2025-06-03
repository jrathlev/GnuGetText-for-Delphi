(* Delphi Dialog
   Text- und Zahleneingabe
   =======================
   © Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.


   Dez. 1995
   Vers. 2 - July 2022: define compiler switch "ACCESSIBLE" to make dialog
                        messages accessible to screenreaders
   last modified: July 2022
   *)

unit TextDlg;

interface

uses Winapi.Windows, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Forms,
  Vcl.Controls, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Dialogs, Vcl.Buttons;

type
  TSwitchMode = (tsNotVis,tsOff,tsOn);

  TTextEingabe = class(TForm)
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    Rahmen: TBevel;
    TextFeld1: TEdit;
    Switch: TCheckBox;
    Textfeld2: TEdit;
    Descriptor1: TStaticText;
    Descriptor2: TStaticText;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
{$IFDEF HDPI}   // scale glyphs and images for High DPI
    procedure AfterConstruction; override;
{$EndIf}
    function Execute (APos : TPoint;
                      const Titel,Desc1,Desc2,SwDesc : string;
                      var AText,BText   : string;
                      var TSw           : TSwitchMode) : boolean;
  end;

(* Text bearbeiten, Ergebnis: "true" bei "ok" *)
function TextDialog (APos : TPoint;
                     const Titel,Desc : string;
                     var Text   : string) : boolean;

(* Kennwort eingeben , Ergebnis: "true" bei "ok" *)
function PwdDialog (APos : TPoint;
                    const Titel,Desc : string;
                    var Pwd    : string) : boolean;

(* Doppeltext bearbeiten, Ergebnis: "true" bei "ok" *)
function DTextDialog (APos : TPoint;
                      const Titel,Desc1,Desc2 : string;
                      var Text1,Text2   : string) : boolean;

(* Zahl bearbeiten, Ergebnis: "true" bei "ok" *)
function NumberDialog (APos : TPoint;
                       const Titel,Desc : string;
                       var n      : longint) : boolean;

(* 2 Zahlen bearbeiten, Ergebnis: "true" bei "ok" *)
function DNumberDialog (APos : TPoint;
                        const Titel,Desc1,Desc2 : string;
                        var n1,n2         : longint) : boolean;

(* 2 Double-Zahlen bearbeiten, Ergebnis: "true" bei "ok" *)
function DFNumberDialog (APos : TPoint;
                         const Titel,Desc1,Desc2 : string;
                         var n1,n2         : double) : boolean;

(* Text bearbeiten, Schalter abfragen, Ergebnis: "true" bei "ok" *)
function TextSwDialog (APos : TPoint;
                       const Titel,Desc,SwDesc : string;
                       var Text          : string;
                       var TSw           : TSwitchMode) : boolean;

(* Text bearbeiten, Ergebnis: geänderter Text bei "ok"
                              alter Text bei "Abbruch" *)
function EditText (APos : TPoint;
                   const Titel,Desc : string;
                   Text       : string) : string;

var
  TextEingabe: TTextEingabe;

implementation

{$R *.DFM}

uses GnuGetText, NumberUtils, WinUtils, {$IFDEF ACCESSIBLE} ShowMessageDlg {$ELSE} MsgDialogs {$ENDIF};

procedure TTextEingabe.FormCreate(Sender: TObject);
begin
  TranslateComponent (self,'dialogs');
  end;

procedure TTextEingabe.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
{$IFDEF ACCESSIBLE}
  if (Key=VK_F11) then begin
    with ActiveControl do if length(Hint)>0 then ShowHintInfo(Hint);
    end;
{$ENDIF}
end;

{$IFDEF HDPI}   // scale glyphs and images for High DPI
procedure TTextEingabe.AfterConstruction;
begin
  inherited;
  if Application.Tag=0 then
    ScaleButtonGlyphs(self,PixelsPerInchOnDesign,Monitor.PixelsPerInch);
  end;
{$EndIf}

{ ------------------------------------------------------------------- }
(* Text-Dialog mit 1 oder 2 Feldern anzeigen *)
function TTextEingabe.Execute(APos : TPoint;
                              const Titel,Desc1,Desc2,SwDesc : string;
                              var AText,BText   : string;
                              var TSw           : TSwitchMode) : boolean;

var
  ok : boolean;
begin
  AdjustFormPosition(Screen,self,APos);
  Caption:=Titel;
  with Switch do if TSw=tsNotVis then Visible:=false
  else begin
    Visible:=true; Checked:=Tsw=tsOn;
    Caption:=SwDesc;
    end;
  if Desc2<>'' then begin
    TextFeld2.Visible:=true; Descriptor2.Visible:=true;
    Descriptor2.Caption:=Desc2;
    with TextFeld1 do begin
      Text:=AText; Width:=166; AutoSelect:=true;
      Hint:=Desc1;
      end;
    with TextFeld2 do begin
      Text:=BText; Width:=166; AutoSelect:=true;
      Hint:=Desc2;
      end;
    end
  else begin
    TextFeld2.Visible:=false; Descriptor2.Visible:=false;
    with TextFeld1 do begin
      Text:=AText; Width:=Rahmen.Width-20; AutoSelect:=true;
      Hint:=Desc1;
      end;
    end;
  Descriptor1.Caption:=Desc1;
  ActiveControl:=TextFeld1;
  Result:=ShowModal=mrOK;
  if Result then begin
    AText:=TextFeld1.Text;
    if Desc2<>'' then BText:=TextFeld2.Text;
    if TSw<>tsNotVis then with Switch do begin
      if Checked then TSw:=tsOn else TSw:=TsOff;
      end;
    end;
  end;

{ ------------------------------------------------------------------- }
(* Text bearbeiten, Ergebnis: "true" bei "ok" *)
function TextDialog (APos : TPoint;
                     const Titel,Desc : string;
                     var Text   : string) : boolean;
var
  TSw : TSwitchMode;
  s   : string;
begin
  if not assigned(TextEingabe) then TextEingabe:=TTextEingabe.Create(Application);
  TSw:=tsNotVis; s:='';
  Result:=TextEingabe.Execute (APos,Titel,Desc,'','',Text,s,TSw);
  FreeAndNil(TextEingabe);
  end;

{ ------------------------------------------------------------------- }
(* Kennwort eingeben , Ergebnis: "true" bei "ok" *)
function PwdDialog (APos : TPoint;
                    const Titel,Desc : string;
                    var Pwd    : string) : boolean;
var
  TSw : TSwitchMode;
  s   : string;
begin
  if not assigned(TextEingabe) then TextEingabe:=TTextEingabe.Create(Application);
  with TextEingabe do begin
    TSw:=tsNotVis; s:='';
    TextFeld1.PasswordChar:='*';
    Pwd:='';
    Result:=Execute (APos,Titel,Desc,'','',Pwd,s,TSw);
    TextFeld1.PasswordChar:=#0;
    end;
  FreeAndNil(TextEingabe);
  end;

{ ------------------------------------------------------------------- }
(* Doppeltext bearbeiten, Ergebnis: "true" bei "ok" *)
function DTextDialog (APos : TPoint;
                      const Titel,Desc1,Desc2 : string;
                      var Text1,Text2   : string) : boolean;
var
  TSw : TSwitchMode;
begin
  if not assigned(TextEingabe) then TextEingabe:=TTextEingabe.Create(Application);
  TSw:=tsNotVis;
  Result:=TextEingabe.Execute (APos,Titel,Desc1,Desc2,'',Text1,Text2,TSw);
  FreeAndNil(TextEingabe);
  end;

{ ------------------------------------------------------------------- }
(* Integer-Zahl bearbeiten, Ergebnis: "true" bei "ok" *)
function NumberDialog (APos : TPoint;
                       const Titel,Desc : string;
                       var n      : longint) : boolean;
var
  TSw  : TSwitchMode;
  s,t  : string;
  nn   : longint;
  ende : boolean;
begin
  if not assigned(TextEingabe) then TextEingabe:=TTextEingabe.Create(Application);
  TSw:=tsNotVis; s:=IntToStr (n); nn:=0;
  repeat
    Result:=TextEingabe.Execute (APos,Titel,Desc,'','',s,t,TSw);
    ende:=true;
    if Result then begin
      try
        nn:=StrToInt(s);
      except
        on EConvertError do begin
          ende:=not ConfirmRetryDialog(APos,dgettext('dialogs','Invalid input'));
          Result:=false;
          end;
        end;
      end;
    until ende;
  if Result then n:=nn;
  FreeAndNil(TextEingabe);
  end;

{ ------------------------------------------------------------------- }
(* 2 Integer-Zahlen bearbeiten, Ergebnis: "true" bei "ok" *)
function DNumberDialog (APos : TPoint;
                        const Titel,Desc1,Desc2 : string;
                        var n1,n2        : longint) : boolean;
var
  TSw  : TSwitchMode;
  s,t  : string;
  nn   : longint;
  ende : boolean;
begin
  if not assigned(TextEingabe) then TextEingabe:=TTextEingabe.Create(Application);
  TSw:=tsNotVis; s:=IntToStr (n1); t:=IntToStr (n2); nn:=0;
  repeat
    Result:=TextEingabe.Execute (APos,Titel,Desc1,Desc2,'',s,t,TSw);
    ende:=true;
    if Result then begin
      try
        nn:=StrToInt(s);
      except
        on EConvertError do begin
          ende:=not ConfirmRetryDialog(APos,dgettext('dialogs','Invalid input'));
          Result:=false;
          end;
        end;
      if Result then begin
        n1:=nn;
        try
          nn:=StrToInt(t);
        except
          on EConvertError do begin
            ende:=not ConfirmRetryDialog(APos,dgettext('dialogs','Invalid input'));
            Result:=false;
            end;
          end;
        if Result then n2:=nn;
        end;
      end;
    until ende;
  FreeAndNil(TextEingabe);
  end;

{ ------------------------------------------------------------------- }
(* 2 Double-Zahlen bearbeiten, Ergebnis: "true" bei "ok" *)
function DFNumberDialog (APos : TPoint;
                         const Titel,Desc1,Desc2 : string;
                         var n1,n2         : double) : boolean;
var
  TSw : TSwitchMode;
  s,t : string;
  nn  : double;
  ende,
  ok  : boolean;
begin
  if not assigned(TextEingabe) then TextEingabe:=TTextEingabe.Create(Application);
  TSw:=tsNotVis; s:=FloatToStrX(n1,0,0); t:=FloatToStrX(n2,0,0); nn:=0;
  repeat
    ok:=TextEingabe.Execute (APos,Titel,Desc1,Desc2,'',s,t,TSw);
    ende:=true;
    if ok then begin
      if not TryStrToFloat(s,nn) then begin
        ende:=not ConfirmRetryDialog(APos,dgettext('dialogs','Invalid input'));
        ok:=false;
        end;
      if ok then begin
        n1:=nn;
        if not TryStrToFloat(t,nn) then begin
          ende:=not ConfirmRetryDialog(APos,dgettext('dialogs','Invalid input'));
          ok:=false;
          end;
        if ok then n2:=nn;
        end;
      end;
    until ende;
  Result:=ok;
  FreeAndNil(TextEingabe);
  end;

{ ------------------------------------------------------------------- }
(* Text bearbeiten, Schalter abfragen, Ergebnis: "true" bei "ok" *)
function TextSwDialog (APos : TPoint;
                       const Titel,Desc,SwDesc : string;
                       var Text          : string;
                       var TSw           : TSwitchMode) : boolean;
var
  s   : string;
begin
  if not assigned(TextEingabe) then TextEingabe:=TTextEingabe.Create(Application);
  s:='';
  Result:=TextEingabe.Execute (APos,Titel,Desc,'',SwDesc,Text,s,TSw);
  FreeAndNil(TextEingabe);
  end;

{ ------------------------------------------------------------------- }
(* Text bearbeiten, Ergebnis: geänderter Text bei "ok"
                              alter Text bei "Abbruch" *)
function EditText (APos : TPoint;
                   const Titel,Desc : string;
                   Text       : string) : string;
var
  s,t : string;
  TSw : TSwitchMode;
begin
  if not assigned(TextEingabe) then TextEingabe:=TTextEingabe.Create(Application);
  s:=Text; t:=''; TSw:=tsNotVis;
  if TextEingabe.Execute (APos,Titel,Desc,'','',s,t,TSw) then Result:=s
  else Result:=Text;
  FreeAndNil(TextEingabe);
  end;

end.
