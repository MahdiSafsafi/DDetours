program MsgBoxHook;

uses
  Vcl.Forms,
  uMain in 'uMain.pas' {Form2},
  DDetours in '..\DDetours.pas',
  InstDecode in '..\InstDecode.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
