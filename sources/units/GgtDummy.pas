(* Delphi-Unit
   Dummy replacement for GnuGetText
   ================================

   Replaces the following GnuGetText functions by dummy statements
   - TranslateComponent
   - dxgetext and _

   Call this function in a monolingual unit created from a multilingual version

   © Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.

   October 2021
   *)

unit GgtDummy;

interface

uses System.Classes;

procedure TranslateComponent(AObject : TComponent; const TextDomain : String='');

function _(const szMsgId: UniCodeString) : UniCodeString;
function dgettext(const szDomain: String; const szMsgId: UniCodeString) : UniCodeString;

implementation

procedure TranslateComponent(AObject : TComponent; const TextDomain : String='');
begin
  // do nothing
  end;

function _(const szMsgId: UniCodeString) : UniCodeString;
begin
  Result:=szMsgId;
  end;

function dgettext(const szDomain: String; const szMsgId: UniCodeString) : UniCodeString;
begin
  Result:=szMsgId;
  end;

end.
