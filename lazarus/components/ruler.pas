{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit Ruler;

{$warn 5023 off : no warning about unused units}
interface

uses
  DRuler, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('DRuler', @DRuler.Register);
end;

initialization
  RegisterPackage('Ruler', @Register);
end.
