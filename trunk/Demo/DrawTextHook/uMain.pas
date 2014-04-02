unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm2 = class(TForm)
    Button1: TButton;
    Button2: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses DDetours;
{$R *.dfm}

var
  TrampolineDrawTextEx: function(DC: HDC; lpchText: LPCWSTR; cchText: Integer; var p4: TRect; dwDTFormat: UINT; DTParams: PDrawTextParams): Integer; stdcall;

function InterceptDrawTextEx(DC: HDC; lpchText: LPCWSTR; cchText: Integer; var p4: TRect; dwDTFormat: UINT; DTParams: PDrawTextParams): Integer; stdcall;
var
  s: string;
  OldColor:TColor;
begin
  s := String(lpchText);
  if Assigned(Form2) then
    if s = Form2.Button1.Caption then
    begin
      OldColor := SetTextColor(DC, clRed);
      Result := TrampolineDrawTextEx(DC, lpchText, cchText, p4, dwDTFormat, DTParams);
      SetTextColor(DC, OldColor);
      Exit;
    end;
  Result := TrampolineDrawTextEx(DC, lpchText, cchText, p4, dwDTFormat, DTParams);
end;

initialization

@TrampolineDrawTextEx := InterceptCreate(@DrawTextEx, @InterceptDrawTextEx);

finalization

InterceptRemove(@TrampolineDrawTextEx);

end.
