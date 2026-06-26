(* Delphi Component
   Register TJrSpeedButton
   =======================
   designed for older Delphi versions (before 10.4) without ImageList property
   in TSpeedButtton

   uses modified TSpeedButtton and TBitBtn from unit Vcl.Buttons.pas

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

unit JrButtonsRegister;

interface

uses Vcl.ImgList, SVGIconImageRegister;

type
// Property editor for ImageIndex of TJrSpeedButton
  TJrImageIndexPropertyEditor = class(TSVGImageIndexPropertyEditor)
  public
    function GetImageListAt(Index: Integer): TCustomImageList; override;
  end;

procedure Register;

implementation

uses System.Classes, DesignIntf, JrButtons;

// Definiere CompPalPage (siehe Register)
{$Include UserComps.pas }

function TJrImageIndexPropertyEditor.GetImageListAt(Index: Integer): TCustomImageList;
var
  LComponent: TPersistent;
begin
  Result := nil;
  LComponent := GetComponent(Index);
  if LComponent is TJrSpeedButton then
    Result := TJrSpeedButton(LComponent).Images;
end;


{ ------------------------------------------------------------------- }
procedure Register;
begin
  RegisterComponents(CompPalPage, [TJrSpeedButton,TJrButton]);
  RegisterPropertyEditor(TypeInfo(TImageIndex), TJrSpeedButton, 'ImageIndex', TJrImageIndexPropertyEditor);
  end;

end.
