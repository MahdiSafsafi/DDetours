program EditSelColor;

uses
  Vcl.Forms,
  uMain in 'uMain.pas' {Form2},
  DDetours in '..\..\..\delphi-detours-library\delphi-detours-library\DDetours.pas',
  InstDecode in '..\..\..\delphi-detours-library\delphi-detours-library\InstDecode.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
