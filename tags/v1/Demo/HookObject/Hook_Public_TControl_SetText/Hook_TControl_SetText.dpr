program Hook_TControl_SetText;

uses
  Vcl.Forms,
  uMain in 'uMain.pas' {Form1},
  DDetours in '..\..\..\DDetours.pas',
  InstDecode in '..\..\..\InstDecode.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
