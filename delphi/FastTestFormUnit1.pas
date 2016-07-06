unit FastTestFormUnit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  CRCunit;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormShow(Sender: TObject);
const
 test_str_a:array [0 .. 11] of byte = ($12, $ED, $08, $00, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
 test_str_b:array [0 .. 11] of byte = ($12, $ED, $08, $00, $FF, $FF, $FF, $FF, $FF, $FF, $00, $FF);
 test_str_c:array [0 .. 11] of byte = ($12, $ED, $08, $00, $FF, $FF, $FF, $FF, $FF, $FF, $01, $FF);
 test_str_d:array [0 .. 11] of byte = ($12, $ED, $08, $00, $FF, $FF, $FF, $FF, $FF, $FF, $02, $FF);
 test_str_e:array [0 .. 11] of byte = ($12, $ED, $08, $00, $FF, $FF, $FF, $FF, $FF, $FF, $04, $FF);
begin
 memo1.Lines.Add(IntToHex(crc_stm32(@test_str_a, 12 div 4), 8));
 memo1.Lines.Add(IntToHex(crc_stm32(@test_str_b, 12 div 4), 8));
 memo1.Lines.Add(IntToHex(crc_stm32(@test_str_c, 12 div 4), 8));
 memo1.Lines.Add(IntToHex(crc_stm32(@test_str_d, 12 div 4), 8));
 memo1.Lines.Add(IntToHex(crc_stm32(@test_str_e, 12 div 4), 8));
end;

end.
