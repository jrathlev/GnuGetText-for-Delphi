(*  Delphi Dialog
    Zeichentabelle (Character table)
    ================================
    
   © Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.
    
   Vers. 1 - Sep. 2002 
   last modified: July 2025
   *)
    
unit CharTableDlg;

interface

uses WinApi.Windows, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Forms, Vcl.Controls, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls,
  Vcl.Grids, JrButtons, System.ImageList, Vcl.ImgList, SVGIconImageListBase,
  SVGIconImageList;

type
  TCharTableDialog = class(TForm)
    CharGrid: TDrawGrid;
    leValue: TLabeledEdit;
    imlGlyphs: TSVGIconImageList;
    OKBtn: TJrButton;
    CancelBtn: TJrButton;
    procedure FormCreate(Sender: TObject);
    procedure CharGridDblClick(Sender: TObject);
    procedure CharGridDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure leValueChange(Sender: TObject);
    procedure FormAfterMonitorDpiChanged(Sender: TObject; OldDPI,
      NewDPI: Integer);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    DefFontName,GridFontName : TFontName;
  public
    { Public declarations }
  end;

function CharFromTable (const APos : TPoint; const AFontName : TFontName) : char;

var
  CharTableDialog: TCharTableDialog;

implementation

{$R *.DFM}

uses GnuGetText, WinUtils, ImageLoader;

{------------------------------------------------------------------- }
procedure TCharTableDialog.FormCreate(Sender: TObject);
begin
  TranslateComponent (self,'dialogs-svg');
  ImageLoader.LoadImages('dialogs',[imlGlyphs.SVGIconItems]);
  DefFontName:=CharGrid.Font.Name;
  imlGlyphs.DPIChanged(self,PixelsPerInchOnDesign,Monitor.PixelsPerInch);
  end;

procedure TCharTableDialog.FormShow(Sender: TObject);
begin
  FitToScreen(Screen,self);
  end;

procedure TCharTableDialog.FormAfterMonitorDpiChanged(Sender: TObject; OldDPI,
  NewDPI: Integer);
begin
  imlGlyphs.DPIChanged(Sender,OldDPI,NewDPI);
  end;

procedure TCharTableDialog.leValueChange(Sender: TObject);
var
  n : integer;
begin
  if TryStrToInt('$'+leValue.Text,n) then with CharGrid do begin
    Row:=(n div 16)+1; Col:=(n mod 16)+1;
    end;
  end;

procedure TCharTableDialog.CharGridDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  with CharGrid.Canvas do begin
    with Font do begin
      Name:=DefFontName; Size:=8; //Style:=[fsBold];
      end;
    if (ARow=0) and (ACol>0) then TextOut(Rect.Left+3,Rect.Top+3,IntToHex(ACol-1,2));
    if (ACol=0) and (ARow>0) then TextOut(Rect.Left+3,Rect.Top+3,IntToHex(ARow-1,2));
    with Font do begin
      Name:=GridFontName; Size:=8; Style:=[];
      end;
    if (ACol>0) and (ARow>0) then begin
      TextOut(Rect.Left+3,Rect.Top+3,chr(16*pred(ARow)+pred(ACol)));
      end;
    end;
  end;

procedure TCharTableDialog.CharGridDblClick(Sender: TObject);
begin
  ModalResult:=mrOK;
  end;

{------------------------------------------------------------------- }
function CharFromTable (const APos : TPoint; const AFontName : TFontName) : char;
begin
  if not assigned(CharTableDialog) then CharTableDialog:=TCharTableDialog.Create(Application);
  AdjustFormPosition(Screen,CharTableDialog,APos);
  with CharTableDialog do begin
    GridFontName:=AFontName;
    if ShowModal=mrOK then with CharGrid do Result:=chr(16*pred(Row)+pred(Col))
    else Result:=#0;
    end;
  FreeAndNil(CharTableDialog);
  end;

end.
