program HookGetMem;

uses
  Vcl.Forms,
  uMain in 'uMain.pas' {Main},
  DDetours in '..\..\..\DDetours.pas',
  InstDecode in '..\..\..\InstDecode.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMain, Main);
  Application.Run;
end.
