(* Delphi Component
   Buttons with images from ImageList
   ==================================
   designed for older Delphi versions (before 10.4) without ImageList property
   in TSpeedButtton and TBitBtn

   uses modified TSpeedButtton and TBitBtn from Delphi Visual Component Library (Vcl.Buttons.pas)
     Copyright(c) 1995-2015 Embarcadero Technologies, Inc. }

   © Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.

   Vers. 1 - Jan. 2025
   last modified: Jan. 2025
   *)

unit JrButtons;

interface

uses
  System.SysUtils, System.Classes, Winapi.Windows, Winapi.Messages, Vcl.Controls, Vcl.StdCtrls,
  Vcl.Buttons, Vcl.Graphics, System.ImageList, Vcl.ImgList;

type
  TJrSpeedButton = class (TGraphicControl)
  private
    FGroupIndex: Integer;
    FDown: Boolean;
    FDragging: Boolean;
    FAllowAllUp: Boolean;
    FLayout: TButtonLayout;
    FSpacing: Integer;
    FTransparent: Boolean;
    FMargin: Integer;
    FFlat: Boolean;
    FMouseInControl: Boolean;
    FImages : TCustomImageList;
    FImageIndex: TImageIndex;
    FImageChangeLink : TChangeLink;
    FButtonDraw : TObject;
    function IsImageIndexStored: Boolean;
    procedure UpdateExclusive;
    procedure SetDown(Value: Boolean);
    procedure SetFlat(Value: Boolean);
    procedure SetAllowAllUp(Value: Boolean);
    procedure SetGroupIndex(Value: Integer);
    procedure SetLayout(Value: TButtonLayout);
    procedure SetSpacing(Value: Integer);
    procedure SetTransparent(Value: Boolean);
    procedure SetMargin(Value: Integer);
    procedure UpdateTracking;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMButtonPressed(var Message: TMessage); message CM_BUTTONPRESSED;
    procedure ImageListChange(Sender: TObject);
    procedure SetImageIndex(const Value: TImageIndex);
    procedure SetImages(const Value: TCustomImageList);
    procedure UpdateImage;
  protected
    FState: TButtonState;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    function GetActionLinkClass: TControlActionLinkClass; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
    property MouseInControl: Boolean read FMouseInControl;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
  published
    property Action;
    property Align;
    property AllowAllUp: Boolean read FAllowAllUp write SetAllowAllUp default False;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;
    property Down: Boolean read FDown write SetDown default False;
    property Caption;
    property Enabled;
    property Flat: Boolean read FFlat write SetFlat default False;
    property Font;
    property Images : TCustomImageList read FImages write SetImages;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex stored IsImageIndexStored default -1;
    property Layout: TButtonLayout read FLayout write SetLayout default blGlyphTop;
    property Margin: Integer read FMargin write SetMargin default -1;
    property ParentFont;
    property ParentShowHint;
    property ParentBiDiMode;
    property PopupMenu;
    property ShowHint;
    property Spacing: Integer read FSpacing write SetSpacing default 4;
    property Transparent: Boolean read FTransparent write SetTransparent default True;
    property Visible;
    property StyleElements;
    property OnClick;
    property OnDblClick;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    end;

  TJrSpeedButtonActionLink = class(TControlActionLink)
  protected
    FClient: TJrSpeedButton;
    procedure AssignClient(AClient: TObject); override;
    function IsCheckedLinked: Boolean; override;
    function IsGroupIndexLinked: Boolean; override;
    function IsImageIndexLinked: Boolean; override;
    procedure SetGroupIndex(Value: Integer); override;
    procedure SetChecked(Value: Boolean); override;
    procedure SetImageIndex(Value: Integer); override;
  public
    constructor Create(AClient: TObject); override;
  end;

  TJrButton = class(TCustomButton)
  strict private
    class constructor Create;
    class destructor Destroy;
  private
    FCanvas: TCanvas;
    FButtonDraw : TObject;
    FImageChangeLink : TChangeLink;
    FLayout: TButtonLayout;
    FSpacing: Integer;
    FMargin: Integer;
    IsFocused: Boolean;
    FMouseInControl: Boolean;
    procedure SetLayout(Value: TButtonLayout);
    procedure SetSpacing(Value: Integer);
    procedure SetMargin(Value: Integer);
    procedure CNMeasureItem(var Message: TWMMeasureItem); message CN_MEASUREITEM;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
  protected
    procedure CreateHandle; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure SetButtonStyle(ADefault: Boolean); override;
    procedure ImageListChange(Sender: TObject);
    procedure UpdateImageList; override;
    procedure UpdateImages; override;
    procedure DrawItem(const DrawItemStruct: TDrawItemStruct);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Action;
    property Align;
    property Anchors;
    property BiDiMode;
    property Cancel;
    property Caption;
    property Constraints;
    property Default;
    property DoubleBuffered default True;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property Images;
    property ImageIndex;
    property Layout: TButtonLayout read FLayout write SetLayout default blGlyphTop;
    property Margin: Integer read FMargin write SetMargin default -1;
    property ModalResult;
    property ParentBiDiMode;
    property ParentDoubleBuffered default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Spacing: Integer read FSpacing write SetSpacing default 4;
    property TabOrder;
    property TabStop;
    property Visible;
    property WordWrap;
    property StyleElements;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  TJrButtonStyleHook = class(TButtonStyleHook)
  strict protected
    procedure DrawButton(ACanvas: TCanvas; AMouseInControl: Boolean); override;
  end;

implementation

uses Vcl.Forms, Vcl.Themes, Vcl.ActnList;

constructor TJrSpeedButtonActionLink.Create(AClient: TObject);
begin
  inherited Create(AClient);
end;

procedure TJrSpeedButtonActionLink.AssignClient(AClient: TObject);
begin
  inherited AssignClient(AClient);
  FClient := AClient as TJrSpeedButton;
end;

function TJrSpeedButtonActionLink.IsCheckedLinked: Boolean;
begin
  Result := inherited IsCheckedLinked and (FClient.GroupIndex <> 0) and
    FClient.AllowAllUp and (FClient.Down = TCustomAction(Action).Checked);
end;

function TJrSpeedButtonActionLink.IsGroupIndexLinked: Boolean;
begin
  Result := inherited IsGroupIndexLinked and (FClient is TJrSpeedButton) and
    (TJrSpeedButton(FClient).GroupIndex = TCustomAction(Action).GroupIndex);
end;

function TJrSpeedButtonActionLink.IsImageIndexLinked: Boolean;
begin
  Result := inherited IsImageIndexLinked and
    (TJrSpeedButton(FClient).ImageIndex = TCustomAction(Action).ImageIndex);
end;

procedure TJrSpeedButtonActionLink.SetChecked(Value: Boolean);
begin
  if IsCheckedLinked then TJrSpeedButton(FClient).Down := Value;
end;

procedure TJrSpeedButtonActionLink.SetGroupIndex(Value: Integer);
begin
  if IsGroupIndexLinked then TJrSpeedButton(FClient).GroupIndex := Value;
end;

procedure TJrSpeedButtonActionLink.SetImageIndex(Value: Integer);
begin
  inherited;
  if IsImageIndexLinked then TJrSpeedButton(FClient).ImageIndex:= Value;
end;

{ ------------------------------------------------------------------- }
type
  TButtonDraw = class
  private
    FImages : TCustomImageList;
    FImageIndex : TImageIndex;
    FImageSize : TPoint;
    FPaintOnGlass,
    FThemeTextColor,
    FThemesEnabled : boolean;
    FThemeDetails: TThemedElementDetails;
    procedure DrawButtonText(Canvas: TCanvas; const Caption: string;
      TextBounds: TRect; State: TButtonState; Flags: Longint);
    procedure CalcButtonLayout(Canvas: TCanvas; const Client: TRect;
      const Offset: TPoint; const Caption: string; Layout: TButtonLayout;
      Margin, Spacing: Integer; var GlyphPos: TPoint; var TextBounds: TRect;
      BiDiFlags: Longint);
  public
    constructor Create;
    destructor Destroy; override;
    function Draw(Canvas: TCanvas; const Client: TRect; const Offset: TPoint;
      const Caption: string; Layout: TButtonLayout; Margin, Spacing: Integer;
      State: TButtonState; Transparent: Boolean; BiDiFlags: Longint): TRect;
    end;

{ ------------------------------------------------------------------- }
constructor TButtonDraw.Create;
begin
  inherited Create;
  FPaintOnGlass := False;
  FThemesEnabled := False;
  FThemeTextColor := True;
  FImages:=nil;
  FImageIndex:=-1;
  FImageSize := Point(0,0);
  end;

destructor TButtonDraw.Destroy;
begin
  inherited Destroy;
  end;

procedure TButtonDraw.CalcButtonLayout(Canvas: TCanvas; const Client: TRect;
  const Offset: TPoint; const Caption: string; Layout: TButtonLayout; Margin,
  Spacing: Integer; var GlyphPos: TPoint; var TextBounds: TRect;
  BiDiFlags: LongInt);
var
  TextPos: TPoint;
  ClientSize, TextSize: TPoint;
  TotalSize: TPoint;
begin
  if (BiDiFlags and DT_RIGHT) = DT_RIGHT then
    if Layout = blGlyphLeft then Layout := blGlyphRight
    else
      if Layout = blGlyphRight then Layout := blGlyphLeft;
  { calculate the item sizes }
  ClientSize := Point(Client.Right - Client.Left, Client.Bottom - Client.Top);

  if Length(Caption) > 0 then
  begin
    TextBounds := Rect(0, 0, Client.Right - Client.Left, 0);
    DrawText(Canvas.Handle, Caption, Length(Caption), TextBounds,
      DT_CALCRECT or BiDiFlags);
    TextSize := Point(TextBounds.Right - TextBounds.Left, TextBounds.Bottom -
      TextBounds.Top);
  end
  else
  begin
    TextBounds := Rect(0, 0, 0, 0);
    TextSize := Point(0,0);
  end;

  { If the layout has the glyph on the right or the left, then both the
    text and the glyph are centered vertically.  If the glyph is on the top
    or the bottom, then both the text and the glyph are centered horizontally.}
  if Layout in [blGlyphLeft, blGlyphRight] then
  begin
    GlyphPos.Y := (ClientSize.Y - FImageSize.Y + 1) div 2;
    TextPos.Y := (ClientSize.Y - TextSize.Y + 1) div 2;
  end
  else
  begin
    GlyphPos.X := (ClientSize.X - FImageSize.X + 1) div 2;
    TextPos.X := (ClientSize.X - TextSize.X + 1) div 2;
  end;

  { if there is no text or no bitmap, then Spacing is irrelevant }
  if (TextSize.X = 0) or (FImageSize.X = 0) then
    Spacing := 0;

  { adjust Margin and Spacing }
  if Margin = -1 then
  begin
    if Spacing < 0 then
    begin
      TotalSize := Point(FImageSize.X + TextSize.X, FImageSize.Y + TextSize.Y);
      if Layout in [blGlyphLeft, blGlyphRight] then
        Margin := (ClientSize.X - TotalSize.X) div 3
      else
        Margin := (ClientSize.Y - TotalSize.Y) div 3;
      Spacing := Margin;
    end
    else
    begin
      TotalSize := Point(FImageSize.X + Spacing + TextSize.X, FImageSize.Y +
        Spacing + TextSize.Y);
      if Layout in [blGlyphLeft, blGlyphRight] then
        Margin := (ClientSize.X - TotalSize.X + 1) div 2
      else
        Margin := (ClientSize.Y - TotalSize.Y + 1) div 2;
    end;
  end
  else
  begin
    if Spacing < 0 then
    begin
      TotalSize := Point(ClientSize.X - (Margin + FImageSize.X), ClientSize.Y -
        (Margin + FImageSize.Y));
      if Layout in [blGlyphLeft, blGlyphRight] then
        Spacing := (TotalSize.X - TextSize.X) div 2
      else
        Spacing := (TotalSize.Y - TextSize.Y) div 2;
    end;
  end;

  case Layout of
    blGlyphLeft:
      begin
        GlyphPos.X := Margin;
        TextPos.X := GlyphPos.X + FImageSize.X + Spacing;
      end;
    blGlyphRight:
      begin
        GlyphPos.X := ClientSize.X - Margin - FImageSize.X;
        TextPos.X := GlyphPos.X - Spacing - TextSize.X;
      end;
    blGlyphTop:
      begin
        GlyphPos.Y := Margin;
        TextPos.Y := GlyphPos.Y + FImageSize.Y + Spacing;
      end;
    blGlyphBottom:
      begin
        GlyphPos.Y := ClientSize.Y - Margin - FImageSize.Y;
        TextPos.Y := GlyphPos.Y - Spacing - TextSize.Y;
      end;
  end;

  { fixup the result variables }
  Inc(GlyphPos.X, Client.Left + Offset.X);
  Inc(GlyphPos.Y, Client.Top + Offset.Y);

  OffsetRect(TextBounds, TextPos.X + Client.Left + Offset.X, TextPos.Y + Client.Top + Offset.Y);
end;

procedure TButtonDraw.DrawButtonText(Canvas: TCanvas; const Caption: string;
  TextBounds: TRect; State: TButtonState; Flags: LongInt);

  procedure DoDrawText(DC: HDC; const Text: UnicodeString;
    var TextRect: TRect; TextFlags: Cardinal);
  var
    LColor: TColor;
    LFormats: TTextFormat;
  begin
    if FThemesEnabled then
    begin
      if (State = bsDisabled) or (not StyleServices.IsSystemStyle and FThemeTextColor) then
      begin
        if not StyleServices.GetElementColor(FThemeDetails, ecTextColor, LColor) or (LColor = clNone) then
          LColor := Canvas.Font.Color;
      end
      else
        LColor := Canvas.Font.Color;

      LFormats := TTextFormatFlags(TextFlags);
      if FPaintOnGlass then
        Include(LFormats, tfComposited);
      StyleServices.DrawText(DC, FThemeDetails, Text, TextRect, LFormats, LColor);
    end
    else
      Winapi.Windows.DrawText(DC, Text, Length(Text), TextRect, TextFlags);
  end;

begin
  with Canvas do
  begin
    Brush.Style := bsClear;
    if (State = bsDisabled) and not FThemesEnabled then
    begin
      OffsetRect(TextBounds, 1, 1);
      Font.Color := clBtnHighlight;
      DoDrawText(Handle, Caption, TextBounds, DT_NOCLIP or DT_CENTER or DT_VCENTER or Flags);
      OffsetRect(TextBounds, -1, -1);
      Font.Color := clBtnShadow;
      DoDrawText(Handle, Caption, TextBounds, DT_NOCLIP or DT_CENTER or DT_VCENTER or Flags);
    end
    else
      DoDrawText(Handle, Caption, TextBounds, DT_NOCLIP or DT_CENTER or DT_VCENTER or Flags);
  end;
end;

function TButtonDraw.Draw(Canvas: TCanvas; const Client: TRect;
  const Offset: TPoint; const Caption: string; Layout: TButtonLayout;
  Margin, Spacing: Integer; State: TButtonState; Transparent: Boolean;
  BiDiFlags: LongInt): TRect;
var
  GlyphPos: TPoint;
begin
  if assigned(FImages) then FImageSize := Point(FImages.Width, FImages.Height)
  else FImageSize := Point(0,0);
  CalcButtonLayout(Canvas, Client, Offset, Caption, Layout, Margin, Spacing,
    GlyphPos, Result, BiDiFlags);
  if assigned(FImages) then FImages.Draw(Canvas,GlyphPos.X,GlyphPos.Y,FImageIndex,State<>bsDisabled);
  DrawButtonText(Canvas, Caption, Result, State, BiDiFlags);
end;

{ ------------------------------------------------------------------- }
{ TSpeedButton }

constructor TJrSpeedButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetBounds(0, 0, 23, 22);
  ControlStyle := [csCaptureMouse, csDoubleClicks];
  ParentFont := True;
  Color := clBtnFace;
  FSpacing := 4;
  FMargin := -1;
  FLayout := blGlyphTop;
  FTransparent := True;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  FButtonDraw:=TButtonDraw.Create;
  FImages:=nil;
  FImageIndex := -1;
end;

destructor TJrSpeedButton.Destroy;
begin
  FImages:= nil;
  (FButtonDraw as TButtonDraw).Free;
  FreeAndNil(FImageChangeLink);
  inherited;
  end;

const
  DownStyles: array[Boolean] of Integer = (BDR_RAISEDINNER, BDR_SUNKENOUTER);
  FillStyles: array[Boolean] of Integer = (BF_MIDDLE, 0);

procedure TJrSpeedButton.Paint;
var
  PaintRect: TRect;
  DrawFlags: Integer;
  Offset: TPoint;
  Button: TThemedButton;
  ToolButton: TThemedToolBar;
  Details: TThemedElementDetails;
  PaintOnGlass : boolean;

  function DoGlassPaint: Boolean;
  var
    LParent: TWinControl;
  begin
    Result := csGlassPaint in ControlState;
    Exit;
    if Result then
    begin
      LParent := Parent;
      while (LParent <> nil) and not LParent.DoubleBuffered do
        LParent := LParent.Parent;
      Result := (LParent = nil) or not LParent.DoubleBuffered or (LParent is TCustomForm);
    end;
  end;

begin
  if not Enabled then
  begin
    FState := bsDisabled;
  end
  else if FState = bsDisabled then
    if Down and (GroupIndex <> 0) then
      FState := bsExclusive
    else
      FState := bsUp;
  Canvas.Font := Self.Font;

  if ThemeControl(Self) then
  begin
    PaintOnGlass := DoGlassPaint;
    if not PaintOnGlass then
      if Transparent then
        StyleServices.DrawParentBackground(0, Canvas.Handle, nil, True)
      else
        PerformEraseBackground(Self, Canvas.Handle)
    else
      FillRect(Canvas.Handle, ClientRect, GetStockObject(BLACK_BRUSH));

    if not Enabled then
      Button := tbPushButtonDisabled
    else
      if FState in [bsDown, bsExclusive] then
        Button := tbPushButtonPressed
      else
        if MouseInControl then
          Button := tbPushButtonHot
        else
          Button := tbPushButtonNormal;

    ToolButton := ttbToolbarDontCare;
    if Flat or TStyleManager.IsCustomStyleActive then
    begin
      case Button of
        tbPushButtonDisabled:
          Toolbutton := ttbButtonDisabled;
        tbPushButtonPressed:
          Toolbutton := ttbButtonPressed;
        tbPushButtonHot:
          Toolbutton := ttbButtonHot;
        tbPushButtonNormal:
          Toolbutton := ttbButtonNormal;
      end;
    end;

    PaintRect := ClientRect;
    if ToolButton = ttbToolbarDontCare then
    begin
      Details := StyleServices.GetElementDetails(Button);
      StyleServices.DrawElement(Canvas.Handle, Details, PaintRect);
      StyleServices.GetElementContentRect(Canvas.Handle, Details, PaintRect, PaintRect);
    end
    else
    begin
      Details := StyleServices.GetElementDetails(ToolButton);
      if not TStyleManager.IsCustomStyleActive then
      begin
        StyleServices.DrawElement(Canvas.Handle, Details, PaintRect);
        // Windows theme services doesn't paint disabled toolbuttons
        // with grayed text (as it appears in an actual toolbar). To workaround,
        // retrieve Details for a disabled button for drawing the caption.
        if (ToolButton = ttbButtonDisabled) then
          Details := StyleServices.GetElementDetails(Button);
      end
      else
      begin
        // Special case for flat speedbuttons with custom styles. The assumptions
        // made about the look of ToolBar buttons may not apply, so only paint
        // the hot and pressed states , leaving normal/disabled to appear flat.
        if not Flat or ((Button = tbPushButtonPressed) or (Button = tbPushButtonHot)) then
          StyleServices.DrawElement(Canvas.Handle, Details, PaintRect);
      end;
      StyleServices.GetElementContentRect(Canvas.Handle, Details, PaintRect, PaintRect);
    end;

    Offset := Point(0, 0);
    if Button = tbPushButtonPressed then
    begin
      // A pressed "flat" speed button has white text in XP, but the Themes
      // API won't render it as such, so we need to hack it.
      if (ToolButton <> ttbToolbarDontCare) and not CheckWin32Version(6) then
        Canvas.Font.Color := clHighlightText
      else
        if Flat then
          Offset := Point(1, 0);
    end;
    with FButtonDraw as TButtonDraw do begin
      FPaintOnGlass := DoGlassPaint;
      FThemeDetails := Details;
      FThemesEnabled := True;
      FThemeTextColor := seFont in StyleElements;
      Draw(Canvas, PaintRect, Offset, Caption, Layout,
        Margin, Spacing, FState, Transparent, DrawTextBiDiModeFlags(0));
      end;
    end
  else begin
    PaintRect := Rect(0, 0, Width, Height);
    if not Flat then
    begin
      DrawFlags := DFCS_BUTTONPUSH or DFCS_ADJUSTRECT;
      if FState in [bsDown, bsExclusive] then
        DrawFlags := DrawFlags or DFCS_PUSHED;
      DrawFrameControl(Canvas.Handle, PaintRect, DFC_BUTTON, DrawFlags);
    end
    else
    begin
      if (FState in [bsDown, bsExclusive]) or
        (MouseInControl and (FState <> bsDisabled)) or
        (csDesigning in ComponentState) then
        DrawEdge(Canvas.Handle, PaintRect, DownStyles[FState in [bsDown, bsExclusive]],
          FillStyles[Transparent] or BF_RECT)
      else if not Transparent then
      begin
        Canvas.Brush.Color := Color;
        Canvas.FillRect(PaintRect);
      end;
      InflateRect(PaintRect, -1, -1);
    end;
    if FState in [bsDown, bsExclusive] then
    begin
      if (FState = bsExclusive) and (not Flat or not MouseInControl) then
      begin
        Canvas.Brush.Bitmap := AllocPatternBitmap(clBtnFace, clBtnHighlight);
        Canvas.FillRect(PaintRect);
      end;
      Offset.X := 1;
      Offset.Y := 1;
    end
    else
    begin
      Offset.X := 0;
      Offset.Y := 0;
    end;

    with FButtonDraw as TButtonDraw do begin
      FThemesEnabled := StyleServices.Enabled;
      Draw(Canvas, PaintRect, Offset, Caption, Layout,
        Margin, Spacing, FState, Transparent, DrawTextBiDiModeFlags(0));
      end;
    end;
  end;

procedure TJrSpeedButton.UpdateImage;
begin
  (FButtonDraw as TButtonDraw ).FImages:=FImages;
  (FButtonDraw as TButtonDraw ).FImageIndex:=FImageIndex;
  end;

procedure TJrSpeedButton.ImageListChange(Sender: TObject);
begin
  UpdateImage;
  Invalidate;
  end;

procedure TJrSpeedButton.SetImages(const Value: TCustomImageList);
begin
  if Value <> FImages then begin
    if FImages <> nil then FImages.UnRegisterChanges(FImageChangeLink);
    FImages := Value;
    if FImages <> nil then begin
      FImages.RegisterChanges(FImageChangeLink);
      FImages.FreeNotification(Self);
      end;
    UpdateImage;
    Invalidate;
    end;
  end;

function TJrSpeedButton.IsImageIndexStored: Boolean;
begin
  Result := (ActionLink = nil) or
    not TJrSpeedButtonActionLink(ActionLink).IsImageIndexLinked;
end;

procedure TJrSpeedButton.SetImageIndex (const Value: TImageIndex);
begin
  if FImageIndex <> Value then begin
    FImageIndex := Value;
    UpdateImage;
    Invalidate;
    end;
  end;

procedure TJrSpeedButton.UpdateTracking;
var
  P: TPoint;
begin
  if FFlat then
  begin
    if Enabled then
    begin
      GetCursorPos(P);
      FMouseInControl := not (FindDragTarget(P, True) = Self);
      if FMouseInControl then
        Perform(CM_MOUSELEAVE, 0, 0)
      else
        Perform(CM_MOUSEENTER, 0, 0);
    end;
  end;
end;

procedure TJrSpeedButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button = mbLeft) and Enabled then
  begin
    if not FDown then
    begin
      FState := bsDown;
      Invalidate;
    end;
    FDragging := True;
  end;
end;

procedure TJrSpeedButton.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewState: TButtonState;
begin
  inherited MouseMove(Shift, X, Y);
  if FDragging then
  begin
    if not FDown then NewState := bsUp
    else NewState := bsExclusive;
    if (X >= 0) and (X < ClientWidth) and (Y >= 0) and (Y <= ClientHeight) then
      if FDown then NewState := bsExclusive else NewState := bsDown;
    if NewState <> FState then
    begin
      FState := NewState;
      Invalidate;
    end;
  end
  else if not FMouseInControl then
    UpdateTracking;
end;

procedure TJrSpeedButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  DoClick: Boolean;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if FDragging then
  begin
    FDragging := False;
    DoClick := (X >= 0) and (X < ClientWidth) and (Y >= 0) and (Y <= ClientHeight);
    if FGroupIndex = 0 then
    begin
      { Redraw face in-case mouse is captured }
      FState := bsUp;
      FMouseInControl := False;
      if DoClick and not (FState in [bsExclusive, bsDown]) then
        Invalidate;
    end
    else
      if DoClick then
      begin
        SetDown(not FDown);
        if FDown then Repaint;
      end
      else
      begin
        if FDown then FState := bsExclusive;
        Repaint;
      end;
    if DoClick then Click;
    UpdateTracking;
  end;
end;

procedure TJrSpeedButton.Click;
begin
  inherited Click;
end;

function TJrSpeedButton.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TJrSpeedButtonActionLink;
end;

procedure TJrSpeedButton.UpdateExclusive;
var
{$IF DEFINED(CLR)}
  I: Integer;
{$ELSE}
  Msg: TMessage;
{$ENDIF}
begin
  if (FGroupIndex <> 0) and (Parent <> nil) then
  begin
{$IF DEFINED(CLR)}
    for I := 0 to Parent.ControlCount - 1 do
      if Parent.Controls[I] is TJrSpeedButton then
        TJrSpeedButton(Parent.Controls[I]).ButtonPressed(FGroupIndex, Self);
{$ELSE}
    Msg.Msg := CM_BUTTONPRESSED;
    Msg.WParam := FGroupIndex;
    Msg.LParam := LPARAM(Self);
    Msg.Result := 0;
    Parent.Broadcast(Msg);
{$ENDIF}
  end;
end;

procedure TJrSpeedButton.SetDown(Value: Boolean);
begin
  if FGroupIndex = 0 then Value := False;
  if Value <> FDown then
  begin
    if FDown and (not FAllowAllUp) then Exit;
    FDown := Value;
    if Value then
    begin
      if FState = bsUp then Invalidate;
      FState := bsExclusive
    end
    else
    begin
      FState := bsUp;
      Repaint;
    end;
    if Value then UpdateExclusive;
  end;
end;

procedure TJrSpeedButton.SetFlat(Value: Boolean);
begin
  if Value <> FFlat then
  begin
    FFlat := Value;
    Invalidate;
  end;
end;

procedure TJrSpeedButton.SetGroupIndex(Value: Integer);
begin
  if FGroupIndex <> Value then
  begin
    FGroupIndex := Value;
    UpdateExclusive;
  end;
end;

procedure TJrSpeedButton.SetLayout(Value: TButtonLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    Invalidate;
  end;
end;

procedure TJrSpeedButton.SetMargin(Value: Integer);
begin
  if (Value <> FMargin) and (Value >= -1) then
  begin
    FMargin := Value;
    Invalidate;
  end;
end;

procedure TJrSpeedButton.SetSpacing(Value: Integer);
begin
  if Value <> FSpacing then
  begin
    FSpacing := Value;
    Invalidate;
  end;
end;

procedure TJrSpeedButton.SetTransparent(Value: Boolean);
begin
  if Value <> FTransparent then
  begin
    FTransparent := Value;
    if Value then
      ControlStyle := ControlStyle - [csOpaque] else
      ControlStyle := ControlStyle + [csOpaque];
    Invalidate;
  end;
end;

procedure TJrSpeedButton.SetAllowAllUp(Value: Boolean);
begin
  if FAllowAllUp <> Value then
  begin
    FAllowAllUp := Value;
    UpdateExclusive;
  end;
end;

procedure TJrSpeedButton.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  inherited;
  if FDown then DblClick;
end;

procedure TJrSpeedButton.CMEnabledChanged(var Message: TMessage);
const
  NewState: array[Boolean] of TButtonState = (bsDisabled, bsUp);
begin
  UpdateTracking;
  Repaint;
end;

procedure TJrSpeedButton.CMButtonPressed(var Message: TMessage);
var
  Sender: TJrSpeedButton;
begin
  if Message.WParam = WPARAM(FGroupIndex) then
  begin
    Sender := TJrSpeedButton(Message.LParam);
    if Sender <> Self then
    begin
      if Sender.Down and FDown then
      begin
        FDown := False;
        FState := bsUp;
        if (Action is TCustomAction) then
          TCustomAction(Action).Checked := False;
        Invalidate;
      end;
      FAllowAllUp := Sender.AllowAllUp;
    end;
  end;
end;

procedure TJrSpeedButton.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if IsAccel(CharCode, Caption) and Enabled and Visible and
      (Parent <> nil) and Parent.Showing then
    begin
      Click;
      Result := 1;
    end else
      inherited;
end;

procedure TJrSpeedButton.CMFontChanged(var Message: TMessage);
begin
  Invalidate;
end;

procedure TJrSpeedButton.CMTextChanged(var Message: TMessage);
begin
  Invalidate;
end;

procedure TJrSpeedButton.CMMouseEnter(var Message: TMessage);
var
  NeedRepaint: Boolean;
begin
  inherited;
  { Don't draw a border if DragMode <> dmAutomatic since this button is meant to
    be used as a dock client. }
  NeedRepaint := FFlat and not FMouseInControl and Enabled and (DragMode <> dmAutomatic) and (GetCapture = 0);

  { Windows XP introduced hot states also for non-flat buttons. }
  if (NeedRepaint or StyleServices.Enabled) and not (csDesigning in ComponentState) then
  begin
    FMouseInControl := True;
    if Enabled then
      Repaint;
  end;
end;

procedure TJrSpeedButton.CMMouseLeave(var Message: TMessage);
var
  NeedRepaint: Boolean;
begin
  inherited;
  NeedRepaint := FFlat and FMouseInControl and Enabled and not FDragging;
  { Windows XP introduced hot states also for non-flat buttons. }
  if NeedRepaint or StyleServices.Enabled then
  begin
    FMouseInControl := False;
    if Enabled then
      Repaint;
  end;
end;

procedure TJrSpeedButton.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  inherited ActionChange(Sender, CheckDefaults);
  if Sender is TCustomAction then
    with TCustomAction(Sender) do
    begin
      if not CheckDefaults or (Self.ImageIndex = -1) then
        Self.ImageIndex := ImageIndex;
    end;
end;

{ ------------------------------------------------------------------- }
class constructor TJrButton.Create;
begin
  TCustomStyleEngine.RegisterStyleHook(TJrButton, TJrButtonStyleHook);
end;

class destructor TJrButton.Destroy;
begin
  TCustomStyleEngine.UnRegisterStyleHook(TJrButton, TJrButtonStyleHook);
end;

constructor TJrButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  FButtonDraw:=TButtonDraw.Create;
  FCanvas := TCanvas.Create;
  FLayout := blGlyphTop;
  FSpacing := 4;
  FMargin := -1;
  ControlStyle := ControlStyle + [csReflector, csPaintBlackOpaqueOnGlass];
  DoubleBuffered := True;
end;

destructor TJrButton.Destroy;
begin
  inherited Destroy;
  FCanvas.Free;
  (FButtonDraw as TButtonDraw).Free;
  FreeAndNil(FImageChangeLink);
end;

procedure TJrButton.CreateHandle;
begin
  inherited CreateHandle;
end;

procedure TJrButton.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do Style := Style or BS_OWNERDRAW;
end;

procedure TJrButton.ImageListChange(Sender: TObject);
begin
  UpdateImages;
  Invalidate;
  end;

procedure TJrButton.SetButtonStyle(ADefault: Boolean);
begin
  if ADefault <> IsFocused then
  begin
    IsFocused := ADefault;
    Refresh;
  end;
end;

procedure TJrButton.CNMeasureItem(var Message: TWMMeasureItem);
var
  Temp: PMeasureItemStruct;
begin
  Temp := Message.MeasureItemStruct;
  with Temp^ do
  begin
    itemWidth := Width;
    itemHeight := Height;
  end;
end;

procedure TJrButton.CNDrawItem(var Message: TWMDrawItem);
begin
  DrawItem(Message.DrawItemStruct^);
end;

procedure TJrButton.DrawItem(const DrawItemStruct: TDrawItemStruct);
const
  WordBreakFlag: array[Boolean] of Integer = (0, DT_WORDBREAK);
var
  IsDown, IsDefault: Boolean;
  State: TButtonState;
  R: TRect;
  Flags: Longint;
  Details: TThemedElementDetails;
  Button: TThemedButton;
  Offset: TPoint;
  LStyle: TCustomStyleServices;
begin
  FCanvas.Handle := DrawItemStruct.hDC;
  R := ClientRect;

  with DrawItemStruct do
  begin
    FCanvas.Handle := hDC;
    FCanvas.Font := Self.Font;
    IsDown := itemState and ODS_SELECTED <> 0;
    IsDefault := itemState and ODS_FOCUS <> 0;

    if not Enabled then State := bsDisabled
    else if IsDown then State := bsDown
    else State := bsUp;
  end;

  if ThemeControl(Self) then
  begin
    LStyle := StyleServices;
    if not Enabled then
      Button := tbPushButtonDisabled
    else
      if IsDown then
        Button := tbPushButtonPressed
      else
        if FMouseInControl then
          Button := tbPushButtonHot
        else
          if IsFocused or IsDefault then
            Button := tbPushButtonDefaulted
          else
            Button := tbPushButtonNormal;

    Details := LStyle.GetElementDetails(Button);
    // Parent background.
    if not (csGlassPaint in ControlState) then
      LStyle.DrawParentBackground(Handle, DrawItemStruct.hDC, Details, True)
    else
      FillRect(DrawItemStruct.hDC, R, GetStockObject(BLACK_BRUSH));
    // Button shape.
    LStyle.DrawElement(DrawItemStruct.hDC, Details, DrawItemStruct.rcItem);
    LStyle.GetElementContentRect(FCanvas.Handle, Details, DrawItemStruct.rcItem, R);

    Offset := Point(0, 0);
    with FButtonDraw as TButtonDraw  do begin
      FPaintOnGlass := csGlassPaint in ControlState;
      FThemeDetails := Details;
      FThemesEnabled := ThemeControl(Self);;
      FThemeTextColor := seFont in StyleElements;
      Draw(FCanvas, R, Offset, Caption, FLayout, FMargin, FSpacing, State, False,
        DrawTextBiDiModeFlags(0) or WordBreakFlag[WordWrap]);
      end;

    if IsFocused and IsDefault and LStyle.IsSystemStyle then
    begin
      FCanvas.Pen.Color := clWindowFrame;
      FCanvas.Brush.Color := clBtnFace;
      DrawFocusRect(FCanvas.Handle, R);
    end;
  end
  else
  begin
    R := ClientRect;

    Flags := DFCS_BUTTONPUSH or DFCS_ADJUSTRECT;
    if IsDown then Flags := Flags or DFCS_PUSHED;
    if DrawItemStruct.itemState and ODS_DISABLED <> 0 then
      Flags := Flags or DFCS_INACTIVE;

    { DrawFrameControl doesn't allow for drawing a button as the
        default button, so it must be done here. }
    if IsFocused or IsDefault then
    begin
      FCanvas.Pen.Color := clWindowFrame;
      FCanvas.Pen.Width := 1;
      FCanvas.Brush.Style := bsClear;
      FCanvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);

      { DrawFrameControl must draw within this border }
      InflateRect(R, -1, -1);
    end;

    { DrawFrameControl does not draw a pressed button correctly }
    if IsDown then
    begin
      FCanvas.Pen.Color := clBtnShadow;
      FCanvas.Pen.Width := 1;
      FCanvas.Brush.Color := clBtnFace;
      FCanvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
      InflateRect(R, -1, -1);
    end
    else
      DrawFrameControl(DrawItemStruct.hDC, R, DFC_BUTTON, Flags);

    if IsFocused then
    begin
      R := ClientRect;
      InflateRect(R, -1, -1);
    end;

    FCanvas.Font := Self.Font;
    if IsDown then
      OffsetRect(R, 1, 1);

    Offset := Point(0, 0);
    with FButtonDraw as TButtonDraw  do begin
      FThemesEnabled := ThemeControl(Self);;
      Draw(FCanvas, R, Offset, Caption, FLayout, FMargin, FSpacing, State, False,
        DrawTextBiDiModeFlags(0) or WordBreakFlag[WordWrap]);
      end;

    if IsFocused and IsDefault then
    begin
      R := ClientRect;
      InflateRect(R, -4, -4);
      FCanvas.Pen.Color := clWindowFrame;
      FCanvas.Brush.Color := clBtnFace;
      DrawFocusRect(FCanvas.Handle, R);
    end;
  end;

  FCanvas.Handle := 0;
end;

procedure TJrButton.UpdateImages;
begin
  (FButtonDraw as TButtonDraw).FImages:=Images;
  (FButtonDraw as TButtonDraw).FImageIndex:=ImageIndex;
  Invalidate;
  end;

procedure TJrButton.UpdateImageList;
begin
  inherited;
  UpdateImages;
  end;

procedure TJrButton.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TJrButton.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TJrButton.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  Perform(WM_LBUTTONDOWN, Message.Keys, LPARAM(Word(Message.XPos) or (Word(Message.YPos) shr 16)));
end;

procedure TJrButton.SetLayout(Value: TButtonLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    Invalidate;
  end;
end;

procedure TJrButton.SetSpacing(Value: Integer);
begin
  if FSpacing <> Value then
  begin
    FSpacing := Value;
    Invalidate;
  end;
end;

procedure TJrButton.SetMargin(Value: Integer);
begin
  if (Value <> FMargin) and (Value >= - 1) then
  begin
    FMargin := Value;
    Invalidate;
  end;
end;

procedure TJrButtonStyleHook.DrawButton(ACanvas: TCanvas; AMouseInControl: Boolean);
const
  WordBreakFlag: array[Boolean] of Integer = (0, DT_WORDBREAK);
var
  Details:  TThemedElementDetails;
  DrawRect: TRect;
  Offset: TPoint;
  State: TButtonState;
begin
  if not (Control is TJrButton) then begin
    inherited;
    Exit;
    end;
  DrawRect := Control.ClientRect;
  if FPressed then
    Details := StyleServices.GetElementDetails(tbPushButtonPressed)
  else if AMouseInControl then
    Details := StyleServices.GetElementDetails(tbPushButtonHot)
  else if Focused or TJrButton(Control).Default then
    Details := StyleServices.GetElementDetails(tbPushButtonDefaulted)
  else if Control.Enabled then
    Details := StyleServices.GetElementDetails(tbPushButtonNormal)
  else
    Details := StyleServices.GetElementDetails(tbPushButtonDisabled);
  DrawRect := Control.ClientRect;
  StyleServices.DrawElement(ACanvas.Handle, Details, DrawRect);

  Offset := Point(0, 0);
  with TJrButton(Control) do begin
    if not Enabled then State := bsDisabled
    else if FPressed then State := bsDown
    else State := bsUp;
    with FButtonDraw as TButtonDraw  do begin
      FPaintOnGlass := false;
      FThemeDetails := Details;
      FThemesEnabled := true;
      FThemeTextColor := seFont in StyleElements;
      Draw(ACanvas, DrawRect, Offset, Caption, FLayout, FMargin, FSpacing, State, False,
        DrawTextBiDiModeFlags(0) or WordBreakFlag[WordWrap]);
      end;
    end;
  end;

end.
