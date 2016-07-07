program FastTest;

uses
  Forms,
  FastTestFormUnit1 in 'FastTestFormUnit1.pas' {Form1},
  stm32_SFU in 'stm32_SFU.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
