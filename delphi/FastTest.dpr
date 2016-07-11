program FastTest;

uses
  Forms,
  FastTestFormUnit1 in 'FastTestFormUnit1.pas' {Form1},
  SFU_boot in 'SFU_boot.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
