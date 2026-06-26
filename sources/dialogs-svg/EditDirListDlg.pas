(* Delphi Dialog
   Auswahl und Bearbeiten von DateiListen
   ======================================
   
   © Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.
    
   Vers. 1 - March 2026
    *)

unit EditDirListDlg;

interface

uses Winapi.Windows, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Forms,
  Vcl.Controls, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls, Vcl.Dialogs, JrButtons,
  System.ImageList, Vcl.ImgList, SVGIconImageListBase, SVGIconImageList;

type
  TDirEditOption = (doAdd,doDelete,doEdit,doOrder);
  TDirEditOptions = set of TDirEditOption;

const
  doAll = [doAdd,doDelete,doEdit,doOrder];

type
  TEditDirListDialog = class(TForm)
    lbxStringList: TListBox;
    gbxMove: TGroupBox;
    lbDesc: TLabel;
    imlGlyphs: TSVGIconImageList;
    OKBtn: TJrButton;
    CancelBtn: TJrButton;
    btnDelete: TJrButton;
    btnEdit: TJrButton;
    btnAdd: TJrButton;
    BitBtn1: TJrButton;
    UpBtn: TJrButton;
    DownBtn: TJrButton;
    OpenDialog: TFileOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure UpBtnClick(Sender: TObject);
    procedure DownBtnClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
  private
    { Private declarations }
    FPrompt,FPath : string;
  public
    { Public declarations }
    function Execute (APos : TPoint; const Titel,Desc,Prompt,Dir : string;
                      SList : TStrings; Options : TDirEditOptions = doAll) : boolean;
    end;

function EditFileList(APos : TPoint; const Titel,Desc,Prompt,Dir : string;
                      SList : TStrings; Options : TDirEditOptions = doAll) : boolean;

var
  EditDirListDialog: TEditDirListDialog;

implementation

{$R *.DFM}

uses ExtSysUtils, WinUtils, StringUtils, PathUtils, GnuGetText, ImageLoader;

{------------------------------------------------------------------- }
procedure TEditDirListDialog.FormCreate(Sender: TObject);
begin
  TranslateComponent (self,'dialogs-svg');
  ImageLoader.LoadImages('dialogs',[imlGlyphs.SVGIconItems]);
  imlGlyphs.DPIChanged(self,PixelsPerInchOnDesign,PixelsPerInch);
  end;

{------------------------------------------------------------------- }
procedure TEditDirListDialog.UpBtnClick(Sender: TObject);
var
  n : integer;
begin
  with lbxStringList do if (Count>0) and (ItemIndex>0) then begin
    n:=ItemIndex;
    Items.Exchange(n,n-1);
    ItemIndex:=n-1;
    end;
  end;

procedure TEditDirListDialog.btnAddClick(Sender: TObject);
var
  s  : string;
  i  : integer;
begin
  with lbxStringList do if ItemIndex>=0 then s:=Items[ItemIndex] else s:='';
  with OpenDialog do begin
    if ContainsFullPath(s) then DefaultFolder:=ExtractFilePath(s)
    else DefaultFolder:=FPath;
    Filename:='';
    Title:=TryFormat(_('Add %s'),[FPrompt]);
    Options:=Options+[fdoAllowMultiSelect];
    if Execute then for i:=0 to Files.Count-1 do begin
      lbxStringList.Items.Add(MakeQuotedStr(MakeRelativePath(FPath,Files[i]),[Space,Comma]));
      end;
    end;
  end;

procedure TEditDirListDialog.btnDeleteClick(Sender: TObject);
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

procedure TEditDirListDialog.btnEditClick(Sender: TObject);
var
  s  : string;
begin
  if lbxStringList.ItemIndex>=0 then begin
    with lbxStringList do s:=Items[ItemIndex];
    with OpenDialog do begin
      if ContainsFullPath(s) then DefaultFolder:=ExtractFilePath(s)
      else DefaultFolder:=FPath;
      Filename:='';
      Title:=TryFormat(_('Replace %s'),[FPrompt]);
      Options:=Options-[fdoAllowMultiSelect];
      if Execute then begin
        with lbxStringList do Items[ItemIndex]:=MakeQuotedStr(MakeRelativePath(FPath,FileName),[Space,Comma]);
        end;
      end;
    end;
  end;

procedure TEditDirListDialog.DownBtnClick(Sender: TObject);
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
function TEditDirListDialog.Execute (APos : TPoint; const Titel,Desc,Prompt,Dir : string;
                      SList : TStrings; Options : TDirEditOptions) : boolean;
var
  i : integer;
  s : string;
begin
  AdjustFormPosition(Screen,self,APos);
  if length(Titel)>0 then Caption:=Titel;
  FPath:=Dir; FPrompt:=Prompt;
  lbDesc.Caption:=Desc;
  lbxStringList.Clear;
  btnAdd.Visible:=doAdd in Options;
  btnEdit.Visible:=doEdit in Options;
  btnDelete.Visible:=doDelete in Options;
  gbxMove.Visible:=doOrder in Options;
  with SList do for i:=0 to Count-1 do lbxStringList.AddItem(Strings[i],Objects[i]);
  lbxStringList.ItemIndex:=0;
  if ShowModal=mrOK then begin
    SList.Clear;
    with lbxStringList.Items do begin
      for i:=0 to Count-1 do SList.AddObject(Strings[i],Objects[i]);
      end;
    Result:=true;
    end
  else Result:=false;
  end;

{------------------------------------------------------------------- }
function EditFileList(APos : TPoint; const Titel,Desc,Prompt,Dir : string;
                      SList : TStrings; Options : TDirEditOptions) : boolean;
begin
  if not assigned(EditDirListDialog) then begin
    EditDirListDialog:=TEditDirListDialog.Create(Application);
    end;
  Result:=EditDirListDialog.Execute(APos,Titel,Desc,Prompt,Dir,SList,Options);
  FreeAndNil(EditDirListDialog);
  end;

end.
