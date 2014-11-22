program Demo2;

uses
  Vcl.Forms,
  uMain in 'uMain.pas' {Main},
  CPUID in '..\..\..\src\CPUID.pas',
  DDetours in '..\..\..\src\DDetours.pas',
  InstDecode in '..\..\..\src\InstDecode.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMain, Main);
  Application.Run;
end.
