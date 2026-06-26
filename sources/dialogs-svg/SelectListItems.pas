(* Delphi Dialog
   Auswahl von Listeneinträgen
   ===========================
   
   © Dr. J. Rathlev, D-24222 Schwentinental (info(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.
    
   Vers. 1 - Oct. 2010
   Vers. 2 - Feb. 2025: SVG glyphs
   last modified: July 2025
    *)

unit SelectListItems;

interface

uses Winapi.Windows, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Forms,
  Vcl.Controls, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls, Vcl.CheckLst,
  JrButtons, System.ImageList, Vcl.ImgList, SVGIconImageListBase,
  SVGIconImageList;

type
  TSelectListItemsDialog = class(TForm)
    clItems: TCheckListBox;
    laDesc: TStaticText;
    laName: TStaticText;
    imlGlyphs: TSVGIconImageList;
    OKBtn: TJrButton;
    CancelBtn: TJrButton;
    btSelNone: TJrButton;
    btSelAll: TJrButton;
    paTop: TPanel;
    paCenter: TPanel;
    paBottom: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure btSelNoneClick(Sender: TObject);
    procedure btSelAllClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormAfterMonitorDpiChanged(Sender: TObject; OldDPI,
      NewDPI: Integer);
    procedure FormShow(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  // Set SList.Objects[i] to 1 for selection
    function Execute (const APos : TPoint; const ATitle,ADesc,AName : string;
                      SList : TStrings) : boolean;
  end;

var
  SelectListItemsDialog: TSelectListItemsDialog;

function SelectItems (const APos : TPoint; const ATitle,ADesc,AName : string;
                      SList : TStrings) : boolean;

implementation

{$R *.dfm}

uses GnuGetText, WinUtils, ShowMessageDlg, ImageLoader;

procedure TSelectListItemsDialog.FormCreate(Sender: TObject);
begin
  TranslateComponent (self,'dialogs-svg');
  ImageLoader.LoadImages('dialogs',[imlGlyphs.SVGIconItems]);
  imlGlyphs.DPIChanged(self,PixelsPerInchOnDesign,Monitor.PixelsPerInch);
  end;

procedure TSelectListItemsDialog.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
{$IFDEF ACCESSIBLE}
  if (Key=VK_F11) then begin
    with ActiveControl do if length(Hint)>0 then ShowHintInfo(Hint);
    end;
{$ENDIF}
end;

procedure TSelectListItemsDialog.FormShow(Sender: TObject);
begin
  FitToScreen(Screen,self);
  end;

procedure TSelectListItemsDialog.FormAfterMonitorDpiChanged(Sender: TObject;
  OldDPI, NewDPI: Integer);
begin
  imlGlyphs.DPIChanged(Sender,OldDPI,NewDPI);
  end;

procedure TSelectListItemsDialog.btSelNoneClick(Sender: TObject);
var
  i : integer;
begin
  with clItems do for i:=0 to Count-1 do Checked[i]:=false;
  end;

procedure TSelectListItemsDialog.btSelAllClick(Sender: TObject);
var
  i : integer;
begin
  with clItems do for i:=0 to Count-1 do Checked[i]:=true;
  end;

function TSelectListItemsDialog.Execute (const APos : TPoint; const ATitle,ADesc,AName : string;
                    SList : TStrings) : boolean;
var
  i,n : integer;
begin
  AdjustFormPosition(Screen,self,APos);
  Caption:=ATitle;
  paTop.Visible:=not ADesc.IsEmpty;
  laDesc.Caption:=ADesc;
  laName.Caption:=AName;
  clItems.Clear;
  for i:=0 to SList.Count-1 do begin
    with clItems do begin
      n:=Items.AddObject(SList.Strings[i],pointer(i));
      Checked[n]:=integer(SList.Objects[i]) and 1 <>0;
      Hint:=AName;
      end;
    end;
  if ShowModal=mrOK then with clItems do begin
    for i:=0 to Items.Count-1 do begin
      n:=integer(Items.Objects[i]);
      with SList do if Checked[i] then Objects[n]:=Pointer(integer(Objects[n]) or 1)
      else Objects[n]:=Pointer(integer(Objects[n]) and $FFFE);
      end;
    Result:=true;
    end
  else Result:=false;
  end;

{------------------------------------------------------------------- }
function SelectItems (const APos : TPoint; const ATitle,ADesc,AName : string;
                      SList : TStrings) : boolean;
begin
  if not assigned(SelectListItemsDialog) then
    SelectListItemsDialog:=TSelectListItemsDialog.Create(Application);
  Result:=SelectListItemsDialog.Execute(APos,ATitle,ADesc,AName,SList);
  FreeAndNil(SelectListItemsDialog)
  end;


end.
