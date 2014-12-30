unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
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

{ TControlHlp }
var
  TrampolineClick: procedure(Self: TControl);

procedure ClickHooked(Self: TControl);
begin
  ShowMessage('You clicked ' + Self.Name);
  TrampolineClick(Self); // Call the original function .
end;

type
  TControlHack = type TControl; // Access protected method .

initialization

@TrampolineClick := InterceptCreate(@TControlHack.Click, @ClickHooked);

finalization

InterceptRemove(@TrampolineClick);

end.
