(* Delphi Dialog
   Dialog für Datei-Filter
   =======================

   © Dr. J. Rathlev, D-24222 Schwentinental (info(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.
    
   Okt. 1999
   last modified: Nov. 2021
   *)

unit FileFilterDlg;

{$MODE Delphi}

interface

uses LCLIntf, LCLType, SysUtils, Classes, Graphics, Forms,
  Controls, StdCtrls, Buttons, Grids;

type
  TFileFilterDialog = class(TForm)
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    FilterListe: TStringGrid;
    FilterNameEdit: TEdit;
    FilterEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    UpBtn: TSpeedButton;
    DownBtn: TSpeedButton;
    DeleteBtn: TSpeedButton;
    InsertBtn: TSpeedButton;
    ReplBtn: TSpeedButton;
    procedure UpBtnClick(Sender: TObject);
    procedure DownBtnClick(Sender: TObject);
    procedure FilterListeSelectCell(Sender: TObject; Col, Row: Longint;
      var CanSelect: Boolean);
    procedure DeleteBtnClick(Sender: TObject);
    procedure InsertBtnClick(Sender: TObject);
    procedure FilterEditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure EditKeyPress(Sender: TObject; var Key: Char);
    procedure ReplBtnClick(Sender: TObject);
    procedure FilterNameEditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    LineCount : integer;
{$IFDEF HDPI}   // scale glyphs and images for High DPI
    procedure AfterConstruction; override;
{$EndIf}
  public
    { Public declarations }
    function Execute (const Title : string;
                      var Filter  : string) : boolean;
  end;

var
  FileFilterDialog: TFileFilterDialog;

implementation

{$R *.lfm}

uses lazgettext;

{------------------------------------------------------------------- }
procedure TFileFilterDialog.FormCreate(Sender: TObject);
begin
  TranslateComponent (self,'dialogs');
  end;

{$IFDEF HDPI}   // scale glyphs and images for High DPI
procedure TFileFilterDialog.AfterConstruction;
begin
  inherited;
  if Application.Tag=0 then
    ScaleButtonGlyphs(self,PixelsPerInchOnDesign,Monitor.PixelsPerInch);
  end;
{$EndIf}

procedure TFileFilterDialog.UpBtnClick(Sender: TObject);
begin
  with FilterListe do if (Row>1) and (Row<=LineCount) then begin
    Cols[0].Exchange(Row,Row-1);
    Cols[1].Exchange(Row,Row-1);
    Row:=Row-1;
    FilterNameEdit.Text:=cells[0,Row];
    FilterEdit.Text:=cells[1,Row];
    end;
  end;

procedure TFileFilterDialog.DownBtnClick(Sender: TObject);
begin
  with FilterListe do if Row<LineCount then begin
    Cols[0].Exchange(Row,Row+1);
    Cols[1].Exchange(Row,Row+1);
    Row:=Row+1;
    FilterNameEdit.Text:=cells[0,Row];
    FilterEdit.Text:=cells[1,Row];
    end;
  end;

procedure TFileFilterDialog.DeleteBtnClick(Sender: TObject);
var
  i : integer;
begin
  if LineCount>0 then begin
    dec(LineCount);
    with FilterListe do begin
      for i:=Row to Linecount do begin
        cells[0,i]:=cells[0,succ(i)];
        cells[1,i]:=cells[1,succ(i)];
        end;
      cells[0,succ(LineCount)]:='';
      cells[1,succ(LineCount)]:='';
      if (Row=succ(LineCount)) and (Row>1) then Row:=Row-1;
      end;
    end;
  end;

procedure TFileFilterDialog.InsertBtnClick(Sender: TObject);
var
  i : integer;
begin
  if LineCount<pred(FilterListe.RowCount) then begin
    with FilterListe do begin
      for i:=Linecount downto Row do begin
        cells[0,succ(i)]:=cells[0,i];
        cells[1,succ(i)]:=cells[1,i];
        end;
      cells[0,Row]:=FilterNameEdit.Text;
      cells[1,Row]:=FilterEdit.Text;
      end;
    inc(LineCount);
    end;
  end;

procedure TFileFilterDialog.ReplBtnClick(Sender: TObject);
begin
  with FilterListe do begin
    cells[0,Row]:=FilterNameEdit.Text;
    cells[1,Row]:=FilterEdit.Text;
   end;
 end;

procedure TFileFilterDialog.FilterListeSelectCell(Sender: TObject; Col,
  Row: Longint; var CanSelect: Boolean);
begin
  CanSelect:=Row<=LineCount;
  if CanSelect then with FilterListe do begin
    FilterNameEdit.Text:=cells[0,Row];
    FilterEdit.Text:=cells[1,Row];
    end;
  end;

procedure TFileFilterDialog.FilterEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key=VK_RETURN) then begin
    with FilterListe do if (LineCount<=Row) and (LineCount<pred(RowCount)) then begin
      inc(LineCount);
      cells[0,LineCount]:=FilterNameEdit.Text;
      cells[1,LineCount]:=FilterEdit.Text;
      Row:=LIneCount;
      end
    else ReplBtnClick(Sender);
    FilternameEdit.SetFocus;
    end;
  end;

procedure TFileFilterDialog.EditKeyPress(Sender: TObject; var Key: Char);
begin
  if Key=#$0D then Key:=#0;
end;

procedure TFileFilterDialog.FilterNameEditKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if (Key=VK_RETURN) then FilterEdit.SetFocus;
  if (Key=VK_DOWN) then with FilterListe do if (Row<LineCount) then Row:=Row+1;
  if (Key=VK_Up) then with FilterListe do if (Row>1) then Row:=Row-1;
  end;

{------------------------------------------------------------------- }
function TFileFilterDialog.Execute (const Title : string;
                                    var Filter  : string) : boolean;
var
  ok : boolean;
  i,j : integer;
  s   : string;
begin
  if Title='' then Caption:=dgettext('dialogs','File filter dialog')
  else Caption:=Title;
  s:=Filter;
  with FilterListe do begin
    cells[0,0]:=dgettext('dialogs','Description');
    cells[1,0]:=dgettext('dialogs','Filter');
    FilterNameEdit.Text:='';
    FilterEdit.Text:='';
    j:=1;
    if length(s)>0 then repeat
      i:=pos('|',s);
      cells[0,j]:=copy(s,1,pred(i));
      delete (s,1,i);
      if length(s)>0 then begin
        i:=pos('|',s);
        if i=0 then i:=succ(length(s));
        cells[1,j]:=copy(s,1,pred(i));
        delete (s,1,i);
        end
      else cells[1,j]:='*.*';
      inc(j);
      until (length(s)=0) or (j=RowCount);
    Row:=1;
    FilterNameEdit.Text:=cells[0,Row];
    FilterEdit.Text:=cells[1,Row];
    end;
  LineCount:=pred(j);
  ok:=ShowModal=mrOK;
  if ok then begin
    s:='';
    with FilterListe do for i:=1 to LineCount do begin
      s:=s+cells[0,i];
      if pos('(',cells[0,i])=0 then s:=s+' ('+cells[1,i]+')';
      s:=s+'|'+cells[1,i]+'|';
      end;
    delete (s,length(s),1);
    Filter:=s;
    end;
  Result:=ok;  
  end;

end.
