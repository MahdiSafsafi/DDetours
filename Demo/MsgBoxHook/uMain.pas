unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Buttons;

type
  TForm2 = class(TForm)
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
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
  TrampolineMessageBoxA: function(hWnd: hWnd; lpText, lpCaption: LPCSTR; uType: UINT): Integer;
stdcall = nil;
MsgHooked: Boolean = False;

function InterceptMessageBoxA(hWnd: hWnd; lpText, lpCaption: LPCSTR; uType: UINT): Integer; stdcall;
begin
  Result := TrampolineMessageBoxA(hWnd, 'Hooked !!', lpCaption, uType);
end;

procedure TForm2.SpeedButton1Click(Sender: TObject);
begin
  MessageBoxA(Handle, 'Text', 'Hi..', MB_OK);
end;

procedure TForm2.SpeedButton2Click(Sender: TObject);
begin
  if not MsgHooked then
  begin
    @TrampolineMessageBoxA := InterceptCreate(@MessageBoxA, @InterceptMessageBoxA);
    MsgHooked := True;
    SpeedButton2.Enabled := False;
    SpeedButton3.Enabled := True;
  end;
end;

procedure TForm2.SpeedButton3Click(Sender: TObject);
begin
  if (@TrampolineMessageBoxA <> nil) and MsgHooked then
  begin
    MsgHooked := False;
    SpeedButton2.Enabled := True;
    SpeedButton3.Enabled := False;
    InterceptRemove(@TrampolineMessageBoxA);
    TrampolineMessageBoxA := nil;
  end;
end;

end.
