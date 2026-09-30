(* Delphi unit
   Menu helper functions
   =====================

   © Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.

   Vers. 1.0 - January 2026
   last modified: January 2026
   *)

unit MenuUtils;

interface

uses Vcl.Menus;

procedure SetOwnerDrawMenuItems (AItems : TMenuItem; DrawItem : TMenuDrawItemEvent;
  MeasureItem : TMenuMeasureItemEvent);
procedure SetOwnerDrawMenu (AMenu : TMenu; DrawItem : TMenuDrawItemEvent;
  MeasureItem : TMenuMeasureItemEvent);

implementation

{ ------------------------------------------------------------------- }
// add ownerdraw events to all menu items
// useful to scale the menu entries on HighDPI screens
procedure SetOwnerDrawMenuItems (AItems : TMenuItem; DrawItem : TMenuDrawItemEvent;
  MeasureItem : TMenuMeasureItemEvent);
var
  i : integer;
begin
  for i:=0 to AItems.Count-1 do begin
    with AItems[i] do begin
      OnDrawItem:=DrawItem;
      OnMeasureItem:=MeasureItem;
      end;
    SetOwnerDrawMenuItems(AItems[i],DrawItem,MeasureItem);
    end;
  end;

// prepare menu to use ownerdraw
procedure SetOwnerDrawMenu (AMenu : TMenu; DrawItem : TMenuDrawItemEvent;
  MeasureItem : TMenuMeasureItemEvent);
begin
  with AMenu do begin
    AutoHotKeys:=maManual;
    OwnerDraw:=true;
    SetOwnerDrawMenuItems(Items,DrawItem,MeasureItem);
    end;
  end;

end.
