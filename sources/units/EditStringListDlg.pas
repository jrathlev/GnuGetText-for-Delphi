(* Delphi Dialog
   Auswahl und Bearbeiten von Listeneinträgen
   ==========================================
   
   © Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.
    
   Vers. 1 - Apr. 2005
   letzte Änderung - Feb. 2022
    *)

unit EditStringListDlg;

interface

uses Winapi.Windows, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Forms,
  Vcl.Controls, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls;

type
  TEditOption = (eoAdd,eoDelete,eoEdit,eoOrder);
  TEditOptions = set of TEditOption;

type
  TEditStringListDialog = class(TForm)
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    lbxStringList: TListBox;
    gbxMove: TGroupBox;
    UpBtn: TSpeedButton;
    DownBtn: TSpeedButton;
    lbDesc: TLabel;
    btnDelete: TBitBtn;
    btnEdit: TBitBtn;
    btnAdd: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure UpBtnClick(Sender: TObject);
    procedure DownBtnClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    function Execute (APos : TPoint; Titel,Desc : string;
                      SList : TStrings; Options : TEditOptions;
                      var AIndex : integer) : boolean;
    function EditList (APos : TPoint; Titel,Desc : string;
                       SList : TStrings; Options : TEditOptions) : boolean;
    end;

function EditStringList(APos : TPoint; Titel,Desc : string;
                      SList : TStrings; Options : TEditOptions;
                      var AIndex : integer) : boolean;

var
  EditStringListDialog: TEditStringListDialog;

implementation

{$R *.DFM}

uses InpText, WinUtils, GnuGetText;

{------------------------------------------------------------------- }
procedure TEditStringListDialog.FormCreate(Sender: TObject);
begin
  TranslateComponent (self,'dialogs');
  end;

{------------------------------------------------------------------- }
procedure TEditStringListDialog.UpBtnClick(Sender: TObject);
var
  n : integer;
begin
  with lbxStringList do if (Count>0) and (ItemIndex>0) then begin
    n:=ItemIndex;
    Items.Exchange(n,n-1);
    ItemIndex:=n-1;
    end;
  end;

procedure TEditStringListDialog.btnAddClick(Sender: TObject);
var
  s  : string;
begin
  with lbxStringList do begin
    s:='';
    if InputText(BottomRightPos(btnEdit),dgettext('dialogs','Add item'),lbDesc.Caption,false,'',nil,false,s) then begin
      Items.Add(s);
      end;
    end;
  end;

procedure TEditStringListDialog.btnDeleteClick(Sender: TObject);
var
  n : integer;
begin
  with lbxStringList do if ItemIndex>=0 then begin
    n:=ItemIndex;
    if assigned(Items.Objects[ItemIndex]) then Items.Objects[ItemIndex].Free;
    Items.Delete(ItemIndex);
    if n>=Items.Count then ItemIndex:=Items.Count-1 else ItemIndex:=n;
    end;
  end;

procedure TEditStringListDialog.btnEditClick(Sender: TObject);
var
  s  : string;
begin
  with lbxStringList do if ItemIndex>=0 then begin
    s:=Items[ItemIndex];
    if InputText(BottomRightPos(btnEdit),dgettext('dialogs','Edit item'),lbDesc.Caption,false,'',nil,false,s) then begin
      Items[ItemIndex]:=s
      end;
    end;
  end;

procedure TEditStringListDialog.DownBtnClick(Sender: TObject);
var
  n : integer;
begin
  with lbxStringList do if (Count>0) and (ItemIndex<Count-1) then begin
    n:=ItemIndex;
    Items.Exchange(n,n+1);
    ItemIndex:=n+1;
    end;
  end;

{------------------------------------------------------------------- }
function TEditStringListDialog.Execute (APos : TPoint; Titel,Desc : string;
                    SList : TStrings; Options : TEditOptions;
                    var AIndex : integer) : boolean;
var
  i : integer;
  s : string;
begin
  with APos do begin
    if (Y < 0) or (X < 0) then Position:=poScreenCenter
    else begin
      if X<0 then X:=Left;
      if Y<0 then Y:=Top;
      CheckScreenBounds(Screen,x,y,Width,Height);
      Left:=x; Top:=y;
      end;
    end;
  if length(Titel)>0 then Caption:=Titel;
  lbDesc.Caption:=Desc;
  lbxStringList.Clear;
  btnAdd.Visible:=eoAdd in Options;
  btnEdit.Visible:=eoEdit in Options;
  btnDelete.Visible:=eoDelete in Options;
  gbxMove.Visible:=eoOrder in Options;
  with SList do for i:=0 to Count-1 do lbxStringList.AddItem(Strings[i],Objects[i]);
  if (AIndex>=0) and (AIndex<SList.Count) then begin
    s:=SList.Strings[AIndex];
    lbxStringList.ItemIndex:=AIndex;
    end
  else s:='';
  if ShowModal=mrOK then begin
    SList.Clear;
    with lbxStringList.Items do begin
      for i:=0 to Count-1 do SList.AddObject(Strings[i],Objects[i]);
      if length(s)>0 then begin
        i:=IndexOf(s);
        if i>=0 then AIndex:=i else AIndex:=0;
        end;
      end;
    Result:=true;
    end
  else Result:=false;
  end;

function TEditStringListDialog.EditList (APos : TPoint; Titel,Desc : string;
                   SList : TStrings; Options : TEditOptions) : boolean;
var
  n : integer;
begin
  n:=-1;
  Result:=EditStringListDialog.Execute (APos,Titel,Desc,SList,Options,n);
  end;

{------------------------------------------------------------------- }
function EditStringList(APos : TPoint; Titel,Desc : string;
                        SList : TStrings; Options : TEditOptions;
                        var AIndex : integer) : boolean;
begin
  if not assigned(EditStringListDialog) then begin
    EditStringListDialog:=TEditStringListDialog.Create(Application);
    end;
  Result:=EditStringListDialog.Execute(APos,Titel,Desc,SList,Options,AIndex);
  FreeAndNil(EditStringListDialog);
  end;

end.
