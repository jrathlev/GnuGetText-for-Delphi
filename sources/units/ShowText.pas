(* Delphi Dialog
   Text oder Textdatei anzeigen (Basis: TMemo)
   ===========================================
   mit Suchfunktion und Drucken

   © Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.

   Vers. 1 - Sep. 2001
   last modified: Nov. 2021
   *)

unit ShowText;

interface

uses Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Buttons,
  Vcl.Printers, Vcl.ComCtrls;

const  // for printout
  texthoehe  = 40;   // 4 mm
  obererRand = 150;  // 150 mm
  linkerRand = 200;  // 20 mm
  untererRand = 150; // 15 mm
  rechterRand = 100; // 10 mm

type
  TShowDlgBtn = (sbErase,sbPrint,sbSearch);
  TShowDlgButtons = set of TShowDlgBtn;

  TShowDlgType = (stShow,stShowModal);

  TShowtextDialog = class(TForm)
    Panel2: TPanel;
    EndeBtn: TBitBtn;
    PrintBtn: TBitBtn;
    PrintDialog: TPrintDialog;
    DeleteBtn: TBitBtn;
    SearchBtn: TBitBtn;
    FindDialog: TFindDialog;
    Memo: TMemo;
    StatusBar: TStatusBar;
    CopyBtn: TBitBtn;
    UpdateBtn: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure MemoKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure PrintBtnClick(Sender: TObject);
    procedure DeleteBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure EndeBtnClick(Sender: TObject);
    procedure FindDialogFind(Sender: TObject);
    procedure SearchBtnClick(Sender: TObject);
    procedure MemoChange(Sender: TObject);
    procedure CopyBtnClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MemoMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MemoKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure UpdateBtnClick(Sender: TObject);
  private
    { Private-Deklarationen }
    LPos            : integer;
    FIniName,FIniSection,
    FName           : string;
{$IFDEF HDPI}   // scale glyphs and images for High DPI
    procedure AfterConstruction; override;
{$EndIf}
  public
    { Public-Deklarationen }
    procedure LoadFromIni(IniName, Section : string);
    procedure Execute (APos            : TPoint;
                       const ATitle,AName : string;
                       TextList        : TStringList;
                       Line            : integer;
                       DlgType         : TShowDlgType;
                       Buttons         : TShowDlgButtons); overload;

    procedure Execute (APos            : TPoint;
                       const TextFile  : string;
                       Line            : integer;
                       DlgType         : TShowDlgType;
                       Buttons         : TShowDlgButtons); overload;
  end;

procedure ShowTextFile (APos            : TPoint;
                        const TextFile  : string;
                        Line            : integer;
                        DlgType         : TShowDlgType;
                        Buttons         : TShowDlgButtons);

var
  ShowtextDialog: TShowtextDialog;

implementation

{$R *.DFM}

uses GnuGetText, System.StrUtils, System.IniFiles, System.Math, WinUtils, MsgDialogs,
  PathUtils;

{------------------------------------------------------------------- }
// Ersatz für Bibliotheksfunktion, da dort nur 16-bit-Werte verarbeitet werden
function GetCaretPos (Memo : TMemo) : TPoint;
var
  CPos : integer;
begin
  with Memo do begin
    Result.X := SendMessage(Handle, EM_GETSEL, WParam(@CPos), 0);
    Result.Y := SendMessage(Handle, EM_LINEFROMCHAR, CPos, 0);
    Result.X := CPos - SendMessage(Handle, EM_LINEINDEX, -1, 0);
    end;
  end;

{ ------------------------------------------------------------------- }
function FindOptionsToSearchOptions (FOptions : TFindOptions) : TStringSearchOptions;
begin
  Result:=[];
  if frDown in FOptions then Include(Result,soDown);
  if frMatchCase in FOptions then Include(Result,soMatchCase);
  if frWholeWord in FOptions then Include(Result,soWholeWord);
  end;

function SearchMemo(Memo: TMemo; SearchDown : boolean;
                    const SearchString: String;
                    Options: TFindOptions): Boolean;
var
  Buffer, P: PChar;
  Size: cardinal;
begin
  Result := False;
  if (Length(SearchString) = 0) then Exit;
  Size := Memo.GetTextLen;
  if (Size = 0) then Exit;
  Buffer := StrAlloc(Size + 1);
  try
    Memo.GetTextBuf(Buffer, Size + 1);
    P := SearchBuf(Buffer, Size, Memo.SelStart, Memo.SelLength, SearchString, FindOptionsToSearchOptions(Options));
    if P <> nil then with memo do begin
      SelStart := P - Buffer;     // Number of line has to be subtracted to get right value for SelStart ???
      SelLength := Length(SearchString);
      Perform(EM_SCROLLCARET,0,0);
      if SearchDown and (Perform(EM_GETFIRSTVISIBLELINE,0,0)<Perform(EM_LINEFROMCHAR,SelStart,0)) then begin
        Perform(EM_SCROLL,SB_LINEDOWN,0); Perform(EM_SCROLL,SB_LINEDOWN,0);
        end;
      Result := True;
    end;
  finally
    StrDispose(Buffer);
  end;
end;

{ ------------------------------------------------------------------- }
procedure TShowtextDialog.FormCreate(Sender: TObject);
begin
  TranslateComponent (self,'dialogs');
  Memo.Clear;
  end;

{$IFDEF HDPI}   // scale glyphs and images for High DPI
procedure TShowtextDialog.AfterConstruction;
begin
  inherited;
  if Application.Tag=0 then begin
    ScaleButtonGlyphs(self,PixelsPerInchOnDesign,Monitor.PixelsPerInch);
    end;
  end;
{$EndIf}

{ ------------------------------------------------------------------- }
const
  iniTop = 'Top';
  iniLeft = 'Left';
  iniHeight = 'Height';
  iniWidth = 'Width';

procedure TShowtextDialog.LoadFromIni(IniName, Section : string);
begin
  FIniName:=IniName; FIniSection:=Section;
  if (length(FIniName)>0) and (length(FIniSection)>0) then begin
    with TIniFile.Create(IniName) do begin
      Top:=ReadInteger(FIniSection,iniTop,Top);
      Left:=ReadInteger(FIniSection,iniLeft,Left);
      Height:=ReadInteger(FIniSection,iniHeight,Height);
      Width:=ReadInteger(FIniSection,iniWidth,Width);
      Free;
      end;
    end;
  end;

procedure TShowtextDialog.FormDestroy(Sender: TObject);
begin
  if (length(FIniName)>0) and (length(FIniSection)>0) then
   with TIniFile.Create(FIniName) do begin
    WriteInteger(FIniSection,iniTop,Top);
    WriteInteger(FIniSection,iniLeft,Left);
    WriteInteger(FIniSection,iniHeight,Height);
    WriteInteger(FIniSection,iniWidth,Width);
    Free;
    end;
  end;

{ ---------------------------------------------------------------- }
(* Cursorposition anzeigen *)
procedure TShowtextDialog.MemoChange(Sender: TObject);
var
  y : integer;
begin
  y:=Memo.CaretPos.y;
  StatusBar.SimpleText:=Format(dgettext('dialogs',' Line: %u of %u'),[y+1,Memo.Lines.Count]);
  end;

procedure TShowtextDialog.FormShow(Sender: TObject);
var
  LineNr : integer;
begin
  with Memo do begin
    if (LPos<=0) or (LPos>=Lines.Count) then begin
      LineNr:=Lines.Count;
      Perform(WM_VSCROLL,SB_BOTTOM,0);
      end
    else begin
      LineNr:=LPos-1;
      Perform(EM_SCROLLCARET,0,0);
      end;
    SelStart:=Perform(EM_LINEINDEX,LineNr,0);
    Perform(EM_SCROLLCARET,0,0);
    SelLength:=0;
    StatusBar.SimpleText:=Format(dgettext('dialogs',' Line: %u of %u'),[LineNr+1,Memo.Lines.Count]);
    end;
  end;

procedure TShowtextDialog.FormPaint(Sender: TObject);
begin
//  MemoChange(Sender);
  end;

procedure TShowtextDialog.MemoKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=VK_ESCAPE then Close;
  end;

procedure TShowtextDialog.MemoKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  MemoChange(Sender);
  end;

procedure TShowtextDialog.MemoMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  MemoChange(Sender);
  end;

procedure TShowtextDialog.FormActivate(Sender: TObject);
begin
  Memo.SetFocus;
  end;

{ ------------------------------------------------------------------- }
procedure TShowtextDialog.PrintBtnClick(Sender: TObject);
var
  w,v      : TSize;
  i,k,n1,n2,
  y,lp,hp  : integer;
  sel      : boolean;
  txt,s,t  : string;

  procedure CheckForNewPage;
  begin
    with Printer,Canvas do begin
      if y>=lp then begin
        y:=obererRand;
        newpage;
        end;
      if y=obererRand then begin   // Kopfzeile erzeugen
        font.style := [fsbold];
        txt:=dgettext('dialogs','Page: ')+IntToStr(pagenumber);
        textout(linkerRand,y,StripPath(Caption,72));
        textout(hp-TextWidth(txt),y,txt);
        inc(y,texthoehe+3);
        moveto (linkerRand,y); lineto (hp,y);
        font.style := [];
        inc(y,20);
        end;
      end;
    end;

begin
  if not Printer.Printing then begin
    if Memo.SelLength>0 then sel:=ConfirmDialog('',dgettext('dialogs','Print selected lines?'))
    else sel:=false;
    if PrintDialog.Execute then begin
      Screen.Cursor:=crHourglass;
      with Printer do begin
        lp:=GetDeviceCaps (Handle,VERTSIZE);
        if lp<=0 then lp:=2900 else lp:=10*lp;  // Seitenhöhe in 1/10 mm
        hp:=GetDeviceCaps (Handle,HORZSIZE);
        if hp<=0 then hp:=2000 else hp:=10*hp;  // Seitenbreite in 1/10 mm
        Title:=Caption;
        begindoc;
        with Canvas do begin
          setmapmode(handle,mm_lometric);     // 1/10 mm aber Y geht nach oben
          GetWindowExtEx(handle,w);           // Auflösung ermitteln
          GetViewportExtEx(handle,v);
          setmapmode(handle,MM_ANISOTROPIC);         // neuer Abb.-Modus
          SetWindowExtEX(handle,w.cx,w.cy,nil);      // Skalierung ...
          SetViewPortExtEx(handle,v.cx,-v.cy,nil);
          font.name := 'Courier New';
          Brush.Color := clwhite;   // Hintergrundfarbe
          font.height:= texthoehe;
          lp:=lp-untererRand;
          hp:=hp-rechterRand;
          y:=obererRand;
          with Memo do begin
            if sel then begin
              (* Zeilennummern des selektierten Bereichs ermitteln *)
              n1:=Perform(EM_LINEFROMCHAR,SelStart,0);
              n2:=Perform(EM_LINEFROMCHAR,SelStart+SelLength,0);
              end
            else begin
              n1:=0; n2:=Lines.Count-1;
              end;
            end;
          for i:=n1 to n2 do begin
            CheckForNewPage;
            txt:=Memo.Lines[i];
            while TextWidth(txt)>=hp-linkerRand do begin
              s:='';
              repeat
                k:=Min(Pos('\',txt),Pos(' ',txt));
                if k>0 then begin
                  t:=copy(txt,1,k);
                  if TextWidth(s+t)<hp-linkerRand then begin
                    s:=s+t; delete(txt,1,k); t:='';
                    end;
                  end
                else t:='x';
                until length(t)>0;
              textout(linkerRand,y,s);
              txt:='* '+txt;
              inc(y,texthoehe+3);
              CheckForNewPage;
              end;
            textout(linkerRand,y,txt);
            inc(y,texthoehe+3);
            end;
          end;
        EndDoc;
        end;
      Screen.Cursor:=crDefault;
      end;
    Memo.SetFocus;
    end;
  end;

{ ------------------------------------------------------------------- }
procedure TShowtextDialog.DeleteBtnClick(Sender: TObject);
begin
  if ConfirmDialog (Point(Left+200,Top+100),Format(dgettext('dialogs','Delete file "%s"?'),[FName])) then begin
    DeleteFile(Caption);
    Memo.Clear;
    end;
  Memo.SetFocus;
  end;

procedure TShowtextDialog.EndeBtnClick(Sender: TObject);
begin
  Close;
  end;

procedure TShowtextDialog.FindDialogFind(Sender: TObject);
begin
  with FindDialog do
    if not SearchMemo(Memo,false,FindText,Options) then
      ErrorDialog(Format(dgettext('dialogs','"%s" not found!'),[FindText]))
    else MemoChange(Sender);
  end;

procedure TShowtextDialog.SearchBtnClick(Sender: TObject);
begin
  FindDialog.Execute;
  end;

procedure TShowtextDialog.UpdateBtnClick(Sender: TObject);
var
  cp : TPoint;
begin
  if FileExists(FName) then with Memo do begin
    cp:=CaretPos;
    Lines.LoadFromFile(FName);
    if cp.Y>=Lines.Count then cp.Y:=Lines.Count-1;
    CaretPos:=cp;
    Perform(EM_SCROLLCARET,0,0);
    SetFocus;
    end
  else ErrorDialog ('',Format(dgettext('dialogs','File: "%s" not found!'),[FName]));
  end;

procedure TShowtextDialog.CopyBtnClick(Sender: TObject);
begin
  Memo.CopyToClipboard;
  end;

{ ------------------------------------------------------------------- }
(* Text anzeigen *)
procedure TShowTextDialog.Execute (APos            : TPoint;
                                   const ATitle,AName : string;
                                   TextList        : TStringList;
                                   Line            : integer;
                                   DlgType         : TShowDlgType;
                                   Buttons         : TShowDlgButtons);
var
  x : integer;
begin
  if Visible then BringToFront
  else begin
    AdjustFormPosition(Screen,self,APos);
    Caption:=ATitle; FName:=AName;
    LPos:=Line;
    x:=2;
    with PrintBtn do if sbPrint in Buttons then begin
      Visible:=true; Left:=x; x:=x+Width-1;
      end
    else Visible:=false;
    with DeleteBtn do if sbErase in Buttons then begin
      Visible:=true; Left:=x; x:=x+Width-1;
      end
    else Visible:=false;
    with SearchBtn do if sbSearch in Buttons then begin
      Visible:=true; Left:=x; x:=x+Width-1;
      end
    else Visible:=false;
    UpdateBtn.Visible:=length(FName)>0;
    with Memo do begin
      Lines:=TextList;
      ReadOnly:=not DeleteBtn.Visible;
      end;
    CopyBtn.Left:=x;
//    with EndeBtn do if DlgType=stShow then Caption:=dgettext('dialogs',' &Back')
//    else Caption:=dgettext('dialogs',' &Close');
    if DlgType=stShowModal then ShowModal else Show;
    end;
  end;

procedure TShowTextDialog.Execute (APos            : TPoint;
                                   const TextFile  : string;
                                   Line            : integer;
                                   DlgType         : TShowDlgType;
                                   Buttons         : TShowDlgButtons);
var
  sl : TStringList;
begin
  if not assigned(ShowTextDialog) then ShowtextDialog:=TShowtextDialog.Create(Application);
  if FileExists (TextFile) then begin
    sl:=TStringList.Create;
    sl.LoadFromFile(TextFile);
    Execute(APos,TextFile,TextFile,sl,Line,DlgType,Buttons);
    sl.Free;
    end
  else ErrorDialog ('',Format(dgettext('dialogs','File: "%s" not found!'),[TextFile]));
  end;

procedure ShowTextFile (APos            : TPoint;
                        const TextFile  : string;
                        Line            : integer;
                        DlgType         : TShowDlgType;
                        Buttons         : TShowDlgButtons);
begin
  if not assigned(ShowTextDialog) then ShowtextDialog:=TShowtextDialog.Create(Application);
  ShowTextDialog.Execute(APos,TextFile,Line,DlgType,Buttons);
  if DlgType=stShowModal then FreeAndNil(ShowTextDialog);
  end;

end.
