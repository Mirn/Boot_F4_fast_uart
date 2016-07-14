unit FastTestFormUnit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls,
  CRCunit,
  comclient,
  linkclient,
  SFU_cmd,
  SFU_boot,
  u_millesecond_timer,
  fifo;

type
  TForm1 = class(TForm)
    MemoDevice: TMemo;
    Timer1mS: TTimer;
    MemoCMD: TMemo;
    StatLabel: TLabel;
    ProgressBar1: TProgressBar;
    Timer100ms: TTimer;
    StopCheckBox: TCheckBox;
    Button1: TButton;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Timer1mSTimer(Sender: TObject);
    procedure Timer100msTimer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    device : tCOMClient;
    sfu : tSFUcmd;
    boot : tSFUboot;
    ms_timer : tmillisecond_timer;

    log_cmd : tfifo_blocks;
    log_dev : tfifo_blocks;

    procedure onLog(sender:tobject; msg:string);
    procedure onLogBoot(sender:tobject; msg:string);
    procedure onInfoString(sender:tobject; msg:string);
    procedure onLogBegin(sender:tLinkClient);
    procedure onLogEnd(sender:tLinkClient);

    procedure onCommand(code:byte; body:pbyte; count:word);

  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
 log_dev := tfifo_blocks.create($10000, $4000);
 log_cmd := tfifo_blocks.create($10000, $4000);
 self.DoubleBuffered := true;
 MemoDevice.DoubleBuffered := true;
 MemoCMD.DoubleBuffered := true;

 device := tCOMClient.Create($10000, $4000);//, $2000, $2000);

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
 //sfu.onCommand := self.onCommand;

 boot := tSFUboot.create(sfu.send_command);
 boot.onLog := self.onLogBoot;
 //boot.firmware_fname := 'E:\gsm\lab\Firmware\SPGateM_pcb16-4lay_ver 1.21 (MR) codec fix.bin';
 boot.firmware_fname := 'E:\Temp\flash_images_fsu_test\added.bin';
 boot.func_free := device.tx_free_bytes;

 sfu.onCommand := boot.recive_command;

 device.onRX := sfu.process_recive;
 milliseconds_start(ms_timer);
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
 log_dev.write_str(msg);
 //MemoDevice.Lines.Add('SFU device:'#9 + msg);
end;

procedure TForm1.onLogBoot(sender:tobject; msg:string);
begin
 msg := inttostr(round(milliseconds_get(ms_timer))) + #9 + msg;
 milliseconds_start(ms_timer);
 msg := inttostr(device.TX_fifo_blocks.data_count)+#9 + msg;
 log_cmd.write_str(msg);
 //MemoCMD.Lines.Add(msg);
end;

procedure TForm1.onLog(sender:tobject; msg:string);
begin
 log_dev.write_str(msg);
 //MemoDevice.Lines.Add(msg);
end;

procedure TForm1.onLogBegin(sender:tLinkClient);
begin
 MemoDevice.Lines.BeginUpdate;
end;

procedure TForm1.onLogEnd(sender:tLinkClient);
begin
 MemoDevice.Lines.EndUpdate;
end;

procedure TForm1.onCommand(code:byte; body:pbyte; count:word);
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
//var
// body : array[0..$100-12] of byte;
// size : integer;
// code : byte;
begin
 if device.State <> link_establish then exit;
 if StopCheckBox.Checked then exit;

 sfu.process_recive(device, nil, 0);
 boot.next_send;

{ while device.tx_free_bytes > sizeof(body)*2 do
  begin
   size := random(sizeof(body) div 4) * 4;
   code := random(256);
   rand_array(@body[0], size);

   sfu.send_command(code, @body[0], size);
  end;}          
end;

procedure TForm1.Timer100msTimer(Sender: TObject);
begin
 if log_dev.blocks_count > 0 then
  begin
   MemoDevice.Lines.BeginUpdate;
   while log_dev.blocks_count > 0 do
    MemoDevice.Lines.Add(log_dev.read_str());
   MemoDevice.Lines.EndUpdate;
   SendMessage(MemoDevice.Handle, EM_LINESCROLL, 0, MemoDevice.Lines.Count);
  end;

 if log_cmd.blocks_count > 0 then
  begin
   MemoCMD.Lines.BeginUpdate;
   while log_cmd.blocks_count > 0 do
    MemoCMD.Lines.Add(log_cmd.read_str());
   MemoCMD.Lines.EndUpdate;
   SendMessage(MemoCMD.Handle, EM_LINESCROLL, 0, MemoCMD.Lines.Count);
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

 ProgressBar1.Max := boot.progress_max;
 ProgressBar1.Position := boot.progress_pos;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
 milliseconds_start(ms_timer);
 boot.start;
end;

end.
