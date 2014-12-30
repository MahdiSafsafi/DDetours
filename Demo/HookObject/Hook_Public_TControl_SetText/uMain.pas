unit uMain;

interface

(* **********************************
  * This demo show you how you can
  * insert a hook on object (class)
  * The example show you how tho hook
  * the SetTextBuf function in order
  * to change controls text (caption)
  ************************************ *)
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TControlHlp = class helper for TControl
    procedure SetTextBufHooked(Buffer: PChar);
  end;

  TForm1 = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Button2: TButton;
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
  TrampolineSetTextBuf: procedure(Buffer: PChar) of Object;

procedure TControlHlp.SetTextBufHooked(Buffer: PChar);
var
  S: String;
begin
  S := String(Buffer);
  S := S + ' :)';
  MakeProcObj(TrampolineSetTextBuf, Self); // init the trampoline function .
  TrampolineSetTextBuf(PChar(S)); // Call the original function .
end;

initialization

@TrampolineSetTextBuf := InterceptCreate(@TControl.SetTextBuf,
  @TControl.SetTextBufHooked);

finalization

InterceptRemove(@TrampolineSetTextBuf);

end.
