(* Lazarus component
   Horizontal or vertical ruler with scalable tick marks
   =====================================================

   © Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.

   Created for Delphi: March 2022
   Last modified: September 2026
   *)
(* @abstract(Collection of routines for string processing)
   @author(© Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de))
   @created(March 2022)
   @lastmod(September 2026)
*)
unit DRuler;

{$mode ObjFPC}{$H+}

interface

uses
  Windows, Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs;

type
  TRulerUnit = (ruCentimeters, ruInches, ruPixels, ruUser);
  TRulerOrientation = (roHorizontal, roVertical);

  TRuler = class(TCustomControl)
  private
    FUseUnit     : TRulerUnit;
    FOrientation : TRulerOrientation;
    FPosition,
    FIncrement   : Double;
    FTickLength,
    FPixelsPerUnit,
    FLabelMarks  : integer;
    procedure SetPosition(const Value: Double);
    procedure SetOrientation(Value: TRulerOrientation);
    procedure SetUseUnit(Value: TRulerUnit);
    procedure SetPixelsPerUnit(Value : integer);
    procedure SetLabelMarks(Value : integer);
    procedure SetIncrement(const Value: Double);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Align;
    property Font;
    property Height default 25;
    property Width default 300;
    property Orientation: TRulerOrientation read FOrientation write SetOrientation  default roHorizontal;
    property Position: Double read FPosition write SetPosition;
    property UseUnit: TRulerUnit read FUseUnit write SetUseUnit default ruCentimeters;
    property PixelsPerUserUnit : integer read FPixelsPerUnit write SetPixelsPerUnit default 20;
    property LabelMarks : integer read FLabelMarks write SetLabelMarks default 1;
    property Increment : double read FIncrement write SetIncrement;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I druler_icon.lrs}
  RegisterComponents('JR-Comps',[TRuler]);
end;

function InchesToPixels(DC: HDC; Value: Single; IsHorizontal: Boolean): Integer;
const
  LogPixels: array [Boolean] of Integer = (LOGPIXELSY, LOGPIXELSX);
begin
  Result := Round(Value * GetDeviceCaps(DC, LogPixels[IsHorizontal]));// * 1.541 / 10);
  end;

function CentimetersToPixels(DC: HDC; Value: Single; IsHorizontal: Boolean): Integer;
const
  LogPixels: array [Boolean] of Integer = (LOGPIXELSY, LOGPIXELSX);
begin
  Result := Round(Value * GetDeviceCaps(DC, LogPixels[IsHorizontal])/2.54);// * 1.541 / 2.54 / 10);
  end;


constructor TRuler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOrientation := roHorizontal;
  FUseUnit := ruCentimeters;
  Height := 25;
  Width := 300;
  FTicklength:=Height div 3;
  Increment:=0.5;
  PixelsPerUserUnit:=20;
  LabelMarks:=1;
  end;

procedure TRuler.Paint;
const
  Offset: array [Boolean] of Integer = (8, 3);
var
  X, Y: Double;
  PX, PY, Pos, LPos, tl: Integer;
  ShowLabel : boolean;
  S: string;
  R: TRect;
begin
  Canvas.Font := Font;
  X := 0;
  Y := 0;
  repeat
    X := X+FIncrement;
    Y := Y+FIncrement;
    case FUseUnit of
    ruInches: begin
        PX := InchesToPixels(Canvas.Handle, X, True);
        PY := InchesToPixels(Canvas.Handle, Y, False);
        Pos := InchesToPixels(Canvas.Handle, Position, Orientation = roHorizontal);
      end;
    ruCentimeters: begin
        PX := CentimetersToPixels(Canvas.Handle, X, True);
        PY := CentimetersToPixels(Canvas.Handle, Y, False);
        Pos := CentimetersToPixels(Canvas.Handle, Position, Orientation = roHorizontal);
      end;
    ruUser: begin
      PX := Round(X * FPixelsPerUnit);
      PY := Round(Y * FPixelsPerUnit);
      Pos := Round(Position * FPixelsPerUnit);
      end;
    else // ruPixels
      PX := Round(X * 50);
      PY := Round(Y * 50);
      Pos := Round(Position);
    end;

    SetBkMode(Canvas.Handle, TRANSPARENT);
    tl:=FTickLength;
    if (PX < Width) or (PY < Height) then if Orientation = roHorizontal then begin
      if UseUnit=ruPixels then LPos:=PX else LPos:=Trunc(X);
      ShowLabel:=X=Trunc(X);
      if ShowLabel and (FLabelMarks>0) then ShowLabel:=LPos mod FLabelMarks=0;
      if ShowLabel then begin
        s:=IntToStr(LPos);
        R := Rect(PX - Canvas.TextWidth(S), 0, PX + Canvas.TextWidth(S), Height);
        Windows.DrawText(Canvas.Handle, PChar(S), Length(S), R, DT_SINGLELINE or DT_CENTER);
        end
      else tl:=tl div 2;
      Canvas.MoveTo(PX, Height - tl); //Offset[X = Trunc(X)]);
      Canvas.LineTo(PX, Height);
      end
    else begin
      if UseUnit=ruPixels then LPos:=PY else LPos:=Trunc(Y);
      ShowLabel:=Y=Trunc(Y);
      if ShowLabel and (FLabelMarks>1) then ShowLabel:=LPos mod FLabelMarks=0;
      if ShowLabel then begin
        s:=IntToStr(LPos);
        R := Rect(0, PY - Canvas.TextHeight(S), Canvas.Width, PY + Canvas.TextHeight(S));
        Windows.DrawText(Canvas.Handle, PChar(S), Length(S), R, DT_SINGLELINE or DT_VCENTER);
        end
      else tl:=tl div 2;
      Canvas.MoveTo(Width - tl, PY);
      Canvas.LineTo(Width, PY);
      end;
    until ((Orientation = roHorizontal) and (PX > Width)) or
     ((Orientation = roVertical) and (PY > Height));

  if Position > 0.0 then with Canvas do
      if Orientation = roHorizontal then begin
        MoveTo(Pos - 2, Height - tl);
        LineTo(Pos + 2, Height - tl);
        LineTo(Pos, Height);
        LineTo(Pos - 2, Height - tl);
        end
      else begin
        MoveTo(Width - tl, Pos - 2);
        LineTo(Width - tl, Pos + 2);
        LineTo(Width, Pos);
        LineTo(Width - tl, Pos - 2);
        end;
  end;

procedure TRuler.SetPosition(const Value: Double);
begin
  if FPosition <> Value then begin
    FPosition := Value;
    Invalidate;
    end;
  end;

procedure TRuler.SetOrientation(Value: TRulerOrientation);
begin
  if FOrientation <> Value then begin
    FOrientation := Value;
    if csDesigning in ComponentState then
      SetBounds(Left, Top, Height, Width);
    Invalidate;
    end;
  end;

procedure TRuler.SetUseUnit(Value: TRulerUnit);
begin
  if FUseUnit <> Value then begin
    FUseUnit := Value;
    Invalidate;
    end;
  end;

procedure TRuler.SetPixelsPerUnit (Value : integer);
begin
  if (Value>0) and (FPixelsPerUnit<>Value) then begin
    FPixelsPerUnit := Value;
    Invalidate;
    end;
  end;

procedure TRuler.SetLabelMarks(Value : integer);
begin
  if (Value>0) and (FLabelMarks<>Value) then begin
    FLabelMarks := Value;
    Invalidate;
    end;
  end;

procedure TRuler.SetIncrement(const Value : double);
begin
  if (Value>0) and (FIncrement<>Value) then begin
    FIncrement:= Value;
    Invalidate;
    end;
  end;

end.
