(* Delphi Dialog
   Löschen und Bearbeiten von Einträgen in Listen (TStrings)
   =========================================================
   z.B. geeignet zur Wartung von History-Listen optional auch mit
   zugeordneten Objekten

   © Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.
    
   Vers. 1 - Apr. 2005
   Vers. 2 - Feb. 2020: Mehrfach-Auswahl für Verschieben und Löschen
   Vers. 3 - Dez. 2021: zugeordnete Objekte werden verarbeitet
   Vers. 3.1 - July 2022: define compiler switch "ACCESSIBLE" to make dialog
                        messages accessible to screenreaders
   last modified: July 2025
    *)

unit EditHistListDlg;

interface

uses Winapi.Windows, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Forms,
  Vcl.Controls, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls;

type
  TEditHistListDialog = class(TForm)
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    lbxStringList: TListBox;
    btnDelete: TBitBtn;
    gbxMove: TGroupBox;
    UpBtn: TBitBtn;
    DownBtn: TBitBtn;
    btnEdit: TBitBtn;
    lbDesc: TStaticText;
    procedure FormCreate(Sender: TObject);
    procedure UpBtnClick(Sender: TObject);
    procedure DownBtnClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    DelList : TStringList;
  public
    { Public declarations }
{$IFDEF HDPI}   // scale glyphs and images for High DPI
    procedure AfterConstruction; override;
{$EndIf}
    function Execute (APos : TPoint; Titel,Desc : string;
                      HList : TStrings; ADelete,AEdit : boolean;
                      var AIndex : integer) : boolean;
  end;

function EditHistList(APos : TPoint; Titel,Desc : string;
                      HList : TStrings; ADelete,AEdit : boolean;
                      var AIndex : integer) : boolean; overload;
function EditHistList(APos : TPoint; Titel,Desc : string;
                      HList : TStrings) : boolean; overload;
function EditHistList(APos : TPoint; Titel,Desc : string;
                      Combo : TComboBox; ADelete,AEdit : boolean;
                      var AIndex : integer) : boolean; overload;

var
  EditHistListDialog: TEditHistListDialog;

implementation

{$R *.DFM}

uses GnuGetText, ExtSysUtils, InpText, WinUtils,
  {$IFDEF ACCESSIBLE} ShowMessageDlg {$ELSE} MsgDialogs {$ENDIF};

{------------------------------------------------------------------- }
procedure TEditHistListDialog.FormCreate(Sender: TObject);
begin
  TranslateComponent (self,'dialogs');
  DelList:=TStringList.Create;
  end;

procedure TEditHistListDialog.FormDestroy(Sender: TObject);
begin
  DelList.Free;
  end;

procedure TEditHistListDialog.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
{$IFDEF ACCESSIBLE}
  if (Key=VK_F11) then begin
    with ActiveControl do if length(Hint)>0 then ShowHintInfo(Hint);
    end;
{$ENDIF}
  end;

procedure TEditHistListDialog.FormShow(Sender: TObject);
begin
  FitToScreen(Screen,self);
  end;

{$IFDEF HDPI}   // scale glyphs and images for High DPI
procedure TEditHistListDialog.AfterConstruction;
begin
  inherited;
  ScaleButtonGlyphs(self,PixelsPerInchOnDesign,Monitor.PixelsPerInch);
  end;
{$EndIf}

{------------------------------------------------------------------- }
procedure TEditHistListDialog.btnDeleteClick(Sender: TObject);
var
  i,n : integer;
begin
  with lbxStringList do if (SelCount>0) then begin
    n:=ItemIndex;
    for i:=Count-1 downto 0 do if Selected[i] then with Items do begin
      DelList.AddObject(Strings[i],Objects[i]);  // retain objects
      Delete(i);
      end;
    if n>=Items.Count then n:=Items.Count-1;
    ItemIndex:=n; Selected[n]:=true;
    end;
  end;

procedure TEditHistListDialog.btnEditClick(Sender: TObject);
var
  s  : string;
  n  : integer;
begin
  with lbxStringList do if ItemIndex>=0 then begin
    s:=Items[ItemIndex];
    if InputText(BottomRightPos(btnEdit),dgettext('dialogs','Edit item'),lbDesc.Caption,false,'',nil,false,s) then begin
      n:=Items.IndexOf(s);
      if (n>=0) and (n<>ItemIndex) then
        ErrorDialog(TopLeftPos(btnEdit),TryFormat(dgettext('dialogs','Item "%s" already exists!'),[s]))
      else begin
        Items[ItemIndex]:=s;
        Selected[ItemIndex]:=true;
        end;
      end;
    end;
  end;

procedure TEditHistListDialog.UpBtnClick(Sender: TObject);
var
  i,n,k : integer;
  IndexList : array of integer;
begin
  with lbxStringList do if (Count>0) then begin
    SetLength(IndexList,Count);
    n:=0;
    for i:=0 to Count-1 do if Selected[i] then begin
      IndexList[n]:=i; inc(n);
      end;
    SetLength(IndexList,n);  // list of selected items
    ClearSelection; k:=0;
    for i:=0 to High(IndexList) do begin // move selected items
      if IndexList[i]>k then begin
        n:=IndexList[i]-1;
        Items.Exchange(IndexList[i],n);
        IndexList[i]:=n;
        end
      else n:=IndexList[i];
      if k=n then inc(k);
      end;
    for i:=0 to High(IndexList) do Selected[IndexList[i]]:=true; //update selection
    ItemIndex:=IndexList[0];
    end;
  end;

procedure TEditHistListDialog.DownBtnClick(Sender: TObject);
var
  i,n,k : integer;
  IndexList : array of integer;
begin
  with lbxStringList do if (Count>0) then begin
    SetLength(IndexList,Count);
    n:=0;
    for i:=Count-1 downto 0 do if Selected[i] then begin
      IndexList[n]:=i; inc(n);
      end;
    SetLength(IndexList,n);  // list of selected items
    ClearSelection; k:=Count-1;
    for i:=0 to High(IndexList) do begin // move selected items
      if IndexList[i]<k then begin
        n:=IndexList[i]+1;
        Items.Exchange(IndexList[i],n);
        IndexList[i]:=n;
        end
      else n:=IndexList[i];
      if k=n then dec(k);
      end;
    for i:=0 to High(IndexList) do Selected[IndexList[i]]:=true; //update selection
    ItemIndex:=IndexList[0];
    end;
  end;

{------------------------------------------------------------------- }
function TEditHistListDialog.Execute (APos : TPoint; Titel,Desc : string;
                    HList : TStrings; ADelete,AEdit : boolean;
                    var AIndex : integer) : boolean;
var
  i : integer;
  s : string;
begin
  with APos do begin
    if (Y < 0) or (X < 0) then Position:=poScreenCenter
    else begin
      Position:=poDesigned;
      if X<0 then X:=Left;
      if Y<0 then Y:=Top;
      CheckScreenBounds(Screen,x,y,Width,Height);
      Left:=x; Top:=y;
      end;
    end;
  if length(Titel)>0 then Caption:=Titel;
  lbDesc.Caption:=Desc;
  lbxStringList.Clear; DelList.Clear;
  btnDelete.Visible:=ADelete;
  btnEdit.Visible:=AEDit;
  with HList do for i:=0 to Count-1 do lbxStringList.AddItem(Strings[i],Objects[i]);
  if (AIndex>=0) and (AIndex<HList.Count) then begin
    s:=HList.Strings[AIndex];
    lbxStringList.Selected[AIndex]:=true;;
    end
  else s:='';
  if ShowModal=mrOK then begin
    with DelList do for i:=0 to Count-1 do  // delete existing associated objects
      if assigned(Objects[i]) then (Objects[i] as TObject).Free;
    HList.Clear;
    with lbxStringList,Items do begin
      for i:=0 to Count-1 do HList.AddObject(Strings[i],Objects[i]);
      if AEdit then begin
        AIndex:=ItemIndex;
        end
      else if length(s)>0 then begin
        i:=IndexOf(s);
        if i>=0 then AIndex:=i else AIndex:=-1;
        end;
      end;
    Result:=true;
    end
  else Result:=false;
  end;

function EditHistList(APos : TPoint; Titel,Desc : string;
                      HList : TStrings; ADelete,AEdit : boolean;
                      var AIndex : integer) : boolean;
begin
  if not assigned(EditHistListDialog) then begin
    EditHistListDialog:=TEditHistListDialog.Create(Application);
    end;
  Result:=EditHistListDialog.Execute(APos,Titel,Desc,HList,ADelete,AEdit,AIndex);
  FreeAndNil(EditHistListDialog);
  end;

function EditHistList(APos : TPoint; Titel,Desc : string;
                      HList : TStrings) : boolean;
var
  n : integer;
begin
  Result:=EditHistList(APos,Titel,Desc,Hlist,true,false,n);
  end;

function EditHistList(APos : TPoint; Titel,Desc : string;
                      Combo : TComboBox; ADelete,AEdit : boolean;
                      var AIndex : integer) : boolean; overload;
var
  n : integer;
begin
  Result:=EditHistList(APos,Titel,Desc,Combo.Items,true,false,n);
  with Combo do if Items.Count<=1 then Style:=csSimple else Style:=csDropDown;
  end;

end.
