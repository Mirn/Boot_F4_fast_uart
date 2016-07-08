unit FastTestFormUnit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  CRCunit,
  comclient, linkclient, ExtCtrls,
  SFU_cmd;

type
  TForm1 = class(TForm)
    MemoDevice: TMemo;
    Timer1mS: TTimer;
    MemoCMD: TMemo;
    StatLabel: TLabel;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Timer1mSTimer(Sender: TObject);
  private
    device : tCOMClient;
    sfu : tSFUcmd;

    procedure onLog(sender:tobject; msg:string);
    procedure onInfoString(sender:tobject; msg:string);
    procedure onLogBegin(sender:tLinkClient);
    procedure onLogEnd(sender:tLinkClient);

    procedure onCommand(code:byte; body:pbyte; count:integer);

  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
 device := tCOMClient.Create($4000, $4000);//, $2000, $2000);

 device.onLog := self.onLog;
 device.onLogBegin := self.onLogBegin;
 device.onLogEnd := self.onLogEnd;

 device.port_name_serial := true;
 device.port_name := 'GM18_E_0010';
 device.port_speed := 500000;
 device.port_parity := NOPARITY;
 device.no_activate := true;

 sfu := tSFUcmd.create;
 sfu.onWrite := device.Write;
 sfu.onLog := self.onLog;
 sfu.onInfoString := self.onInfoString;
 sfu.onCommand := self.onCommand;

 device.onRX := sfu.process_recive;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
 device.Open;
 randomize;
 MemoDevice.Lines.Add('randseed = ' + IntToStr(RandSeed));
 MemoDevice.Lines.Add('');
end;

procedure TForm1.onInfoString(sender:tobject; msg:string);
begin
 MemoDevice.Lines.Add('SFU:'#9 + msg);
end;

procedure TForm1.onLog(sender:tobject; msg:string);
begin
 MemoDevice.Lines.Add(msg);
end;

procedure TForm1.onLogBegin(sender:tLinkClient);
begin
 MemoDevice.Lines.BeginUpdate;
end;

procedure TForm1.onLogEnd(sender:tLinkClient);
begin
 MemoDevice.Lines.EndUpdate;
end;

procedure TForm1.onCommand(code:byte; body:pbyte; count:integer);
var
 str : string;
 cnt : integer;
begin
 str := '';
 cnt := count;
 while cnt > 0 do
  begin
//   if body^ > 32 then
//    str := str + ansichar(body^)
//   else
    str := str + inttohex(body^, 2) + ' ';
   inc(body);
   dec(cnt);
  end;

 MemoCMD.Lines.Add('CMD('+inttohex(code, 2)+'#'+inttohex(count, 2)+'): ' + str);
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
 body : array[0..100-12] of byte;
 size : integer;
 code : byte;
begin
 if device.State <> link_establish then exit;

 sfu.process_recive(device, nil, 0);

 while device.tx_free_bytes > sizeof(body)*2 do
  begin
   size := random(sizeof(body) div 4) * 4;
   code := random(256);
   rand_array(@body[0], size);

   sfu.send_command(code, @body[0], size);
  end;

 StatLabel.Caption :=
    'TXok: ' + inttostr(sfu.stat_send) +
    '  RXok: ' + inttostr(sfu.stat_normals) +
    '  ' +
    '  start: ' + inttostr(sfu.stat_error_start) +
    '  Err: ' + inttostr(sfu.stat_errors) +
    '  ' + 
    '  Timeout: ' + inttostr(sfu.stat_error_timeout) +
    '  code: ' + inttostr(sfu.stat_error_code) +
    '  size: ' + inttostr(sfu.stat_error_size) +
    '  crc: ' + inttostr(sfu.stat_error_crc);
end;

end.
