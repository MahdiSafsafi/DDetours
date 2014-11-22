unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, DDetours;

type
  TForm2 = class(TForm)
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

var
  RedBrush: HBRUSH;
  TrampolineFillRect: function(hDC: hDC; const lprc: TRect; hbr: HBRUSH): Integer;
stdcall = nil;

function InterceptFillRect(hDC: hDC; const lprc: TRect; hbr: HBRUSH): Integer; stdcall;
begin
  Result := TrampolineFillRect(hDC, lprc, RedBrush);
end;

initialization

RedBrush := CreateSolidBrush(clRed);
@TrampolineFillRect := InterceptCreate(@FillRect, @InterceptFillRect);

finalization

InterceptRemove(@TrampolineFillRect);
DeleteObject(RedBrush);

end.
