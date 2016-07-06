unit FastTestFormUnit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  CRCunit,
  comclient, linkclient, ExtCtrls;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Timer1mS: TTimer;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Timer1mSTimer(Sender: TObject);
  private
    { Private declarations }

    procedure onLog(sender:tobject; msg:string);
    procedure onLogBegin(sender:tLinkClient);
    procedure onLogEnd(sender:tLinkClient);

    procedure onRX(sender:tLinkClient; data:pbyte; size:integer);
  public
    { Public declarations }
   device : tCOMClient;
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
{ memo1.Lines.Add(IntToHex(crc_stm32(@test_str_a, 12 div 4), 8));
 memo1.Lines.Add(IntToHex(crc_stm32(@test_str_b, 12 div 4), 8));
 memo1.Lines.Add(IntToHex(crc_stm32(@test_str_c, 12 div 4), 8));
 memo1.Lines.Add(IntToHex(crc_stm32(@test_str_d, 12 div 4), 8));
 memo1.Lines.Add(IntToHex(crc_stm32(@test_str_e, 12 div 4), 8));}

 device.Open;
 randomize;
 memo1.Lines.Add('randseed = ' + IntToStr(RandSeed));
 memo1.Lines.Add('');
end;

procedure TForm1.onLog(sender:tobject; msg:string);
begin
 memo1.Lines.Add(msg);
end;

procedure TForm1.onLogBegin(sender:tLinkClient);
begin
 memo1.Lines.BeginUpdate;
end;

procedure TForm1.onLogEnd(sender:tLinkClient);
begin
 memo1.Lines.EndUpdate;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
 device := tCOMClient.Create($4000, $4000);

 device.onLog := self.onLog;
 device.onLogBegin := self.onLogBegin;
 device.onLogEnd := self.onLogEnd;
 device.onRX := self.onRX;

 device.port_name_serial := true;
 device.port_name := 'GM18_E_0010';
 device.port_speed := 500000;
 device.port_parity := NOPARITY;
 device.no_activate := true;
end;

procedure TForm1.onRX(sender:tLinkClient; data:pbyte; size:integer);
var
 str : string;
begin
 SetLength(str, size);
 move(data^, str[1], size);
 memo1.Lines.Add(str);
end;

procedure stm32_FUU_packet_make(code:byte; size:word; cmd_body:pointer; raw_buf:pointer; raw_cnt:pcardinal);
var
 crc : cardinal;
 pos : integer;
 buf : array of byte absolute raw_buf;
 body : array of byte absolute cmd_body;
begin
 buf[0] := $81;
 buf[1] := $7E;
 buf[2] := $A3;
 buf[3] := $45;

 buf[4] := code;
 buf[5] := not buf[4];

 buf[6] := (size shr (8*0)) and 255;
 buf[7] := (size shr (8*1)) and 255;

 for pos := 0 to size - 1 do
  buf[8 + pos] := body[pos];

 crc := crc_stm32(@buf[4], (size + 4) div 4); //+4: code + ncode + size

 buf[8 + size + 0] := (crc shr (8*0)) and 255;
 buf[8 + size + 1] := (crc shr (8*1)) and 255;
 buf[8 + size + 2] := (crc shr (8*2)) and 255;
 buf[8 + size + 3] := (crc shr (8*3)) and 255;

 raw_cnt^ := 8 + size + 4;
end;

procedure rand_array(buf:pbyte; cnt:cardinal);
begin
 while cnt > 0 do
  begin
   buf^ := random(256);
   inc(buf);
   dec(cnt);
  end;
end;

procedure TForm1.Timer1mSTimer(Sender: TObject);
var
 buf : array[0..$1000] of byte;
 body : array[0..$1000-12] of byte;
 size : integer;
 code : byte;
 cnt : cardinal;
begin
 if device.State <> link_establish then exit;

 while device.tx_free_bytes > sizeof(buf) do
  begin
   size := random(sizeof(body) div 4) * 4;
   code := random(256);
   rand_array(@body[0], size);

   cnt := 0;
   stm32_FUU_packet_make(code, size, @body[0], @buf[0], @cnt);

   device.Write(@buf[0], cnt);
  end;
end;

end.
