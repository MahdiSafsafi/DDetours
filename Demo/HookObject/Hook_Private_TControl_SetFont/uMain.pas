unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Label1: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses DDetours;
{$R *.dfm}

type
  TControlHack = type TControl;

var
  TrampoSetFont: procedure(Self: TControl; Value: TFont);

procedure SetFontHooked(Self: TControl; Value: TFont);
begin
  Value.Style := [fsBold];
  TrampoSetFont(Self, Value);
end;

initialization

@TrampoSetFont := InterceptCreate(@TControlHack.SetFont, @SetFontHooked);

finalization

InterceptRemove(@TrampoSetFont);

end.
