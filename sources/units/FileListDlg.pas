(* Delphi Dialog
   Stringliste aus Dateinamen erstellen und sortieren
   ==================================================

   © Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.

   Okt. 2005
   last modified: July 2023
   *)

unit FileListDlg;

interface

uses Winapi.Windows, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Forms,
  Vcl.Controls, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls, Vcl.Dialogs;

type
  TFileListDialog = class(TForm)
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    lbxFiles: TListBox;
    lblPath: TLabel;
    UpBtn: TSpeedButton;
    DownBtn: TSpeedButton;
    DeleteBtn: TSpeedButton;
    InsertBtn: TSpeedButton;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    btSaveAs: TSpeedButton;
    laName: TLabel;
    lblListfile: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    paBottom: TPanel;
    paTop: TPanel;
    paCenter: TPanel;
    procedure InsertBtnClick(Sender: TObject);
    procedure DeleteBtnClick(Sender: TObject);
    procedure UpBtnClick(Sender: TObject);
    procedure DownBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btSaveAsClick(Sender: TObject);
  private
    { Private-Deklarationen }
    FFilename,FPath : string;
{$IFDEF HDPI}   // scale glyphs and images for High DPI
    procedure AfterConstruction; override;
{$EndIf}
    procedure SaveList;
  public
    { Public-Deklarationen }
// FromFile = false : FName is a comma separated file list
//          = true  : FName is a file name from whre to load the file list
    function Execute (const Title,FileFilter,Path,FName,
            ListFilter,DefExt : string; FromFile : boolean = false) : string;
  end;

var
  FileListDialog: TFileListDialog;

implementation

{$R *.dfm}

uses GnuGetText, Vcl.FileCtrl {$IFDEF HDPI}, WinUtils{$EndIf};

{------------------------------------------------------------------- }
procedure TFileListDialog.FormCreate(Sender: TObject);
begin
  TranslateComponent (self,'dialogs');
  end;

{$IFDEF HDPI}   // scale glyphs and images for High DPI
procedure TFileListDialog.AfterConstruction;
begin
  inherited;
  if Application.Tag=0 then
    ScaleButtonGlyphs(self,PixelsPerInchOnDesign,Monitor.PixelsPerInch);
  end;
{$EndIf}

procedure TFileListDialog.InsertBtnClick(Sender: TObject);
var
  i : integer;
begin
  with OpenDialog do if Execute then with Files do for i:=0 to Count-1 do
      lbxFiles.Items.Add(ExtractRelativePath(FPath,Strings[i]));
  end;

procedure TFileListDialog.SaveList;
var
  bf : textFile;
  i  : integer;
begin
  AssignFile(bf,FFilename); rewrite(bf);
  with lbxFiles.Items do for i:=0 to Count-1 do writeln(bf,Strings[i]);
  CloseFile(bf);
  end;

procedure TFileListDialog.btSaveAsClick(Sender: TObject);
begin
  with SaveDialog do begin
    if length(FFilename)>0 then InitialDir:=ExtractFilePath(FFilename);
    if Execute then begin
      FFilename:=Filename;
      SaveList;
      end;
    end;
  end;

procedure TFileListDialog.DeleteBtnClick(Sender: TObject);
var
  i : integer;
begin
  with lbxFiles do if (Items.Count>0) and (ItemIndex>=0) then begin
    i:=ItemIndex;
    Items.Delete(Itemindex);
    if i<Items.Count then Itemindex:=i else Itemindex:=Items.Count-1;
    end;
  end;

procedure TFileListDialog.UpBtnClick(Sender: TObject);
var
  i : integer;
begin
  with lbxFiles do if (Items.Count>0) and (ItemIndex>0) then begin
    i:=ItemIndex-1;
    Items.Exchange (i,ItemIndex);
    SetFocus;
    ItemIndex:=i;
    end;
  end;

procedure TFileListDialog.DownBtnClick(Sender: TObject);
var
  i : integer;
begin
  with lbxFiles do if (Items.Count>0) and (ItemIndex<Items.Count-1) then begin
    i:=ItemIndex+1;
    Items.Exchange (i,ItemIndex);
    SetFocus;
    ItemIndex:=i;
    end;
  end;

{------------------------------------------------------------------- }
function TFileListDialog.Execute (const Title,FileFilter,Path,FName,
                                  ListFilter,DefExt : string; FromFile : boolean) : string;
var
  bf : TextFile;
  s  : string;
begin
  Caption:=Title;
  FPath:=IncludeTrailingPathDelimiter(Path);
  lblPath.Caption:=MinimizeName(Path,Canvas,ClientWidth-lblPath.Left-10);
  FFilename:=FName;
  btSaveAs.Visible:=FromFile;
  laName.Visible:=FromFile;
  with lblListfile do begin
    Visible:=FromFile;
    if Visible then Caption:=ExtractFilename(FName);
    end;
  lbxFiles.Clear;
  with OpenDialog do begin
    InitialDir:=Path;
    Filter:=FileFilter;
    end;
  with SaveDialog do begin
    InitialDir:=Path;
    Filter:=ListFilter;
    DefaultExt:=DefExt;
    end;
  paTop.Visible:=FromFile;
  if length(FName)>0 then begin
    if FromFile then begin
      AssignFile(bf,FName); reset(bf);
      while not Eof(bf) do begin
        readln (bf,s);
        lbxFiles.Items.Add(s);
        end;
      CloseFile(bf);
      end
    else lbxFiles.Items.CommaText:=FName;
    lbxFiles.Itemindex:=0;
    end;
  if (ShowModal=mrOK) and (lbxFiles.Items.Count>0) then begin
    if FromFile then begin
      if length(FFilename)=0 then with SaveDialog do begin
        if Execute then FFilename:=Filename;
        end;
      if length(FName)>0 then begin
        SaveList;
        Result:=FFilename;
        end
      else Result:='';
      end
    else Result:=lbxFiles.Items.CommaText;
    end
  else Result:='';
  end;

end.
