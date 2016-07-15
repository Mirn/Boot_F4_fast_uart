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
  fifo,
  Unit_Win7Taskbar;

type
  TForm1 = class(TForm)
    MemoDevice: TMemo;
    Timer1mS: TTimer;
    MemoCMD: TMemo;
    StatLabel: TLabel;
    ProgressBar: TProgressBar;
    Timer100ms: TTimer;
    StopCheckBox: TCheckBox;
    GoButton: TButton;
    SFUboot_StatusLabel: TLabel;
    FastEraseCheckBox: TCheckBox;
    DeviceEdit: TEdit;
    ResetCheckBox: TCheckBox;
    LabelDev: TLabel;
    LabelBin: TLabel;
    FirmwareEdit: TEdit;
    StopButton: TButton;
    ExitCheckBox: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Timer1mSTimer(Sender: TObject);
    procedure Timer100msTimer(Sender: TObject);
    procedure GoButtonClick(Sender: TObject);
    procedure StopButtonClick(Sender: TObject);
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

    procedure OnDone_OnError;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}
{$R windowsxp.RES}

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
 device.task_open_with_reset := true;

 device.stm32_task_read   := false;
 device.stm32_task_RDlock := false;
 device.stm32_task_UNlock := false;
 device.stm32_task_erase  := false;
 device.stm32_task_write  := false;
 device.stm32_task_verify := false;

 device.stm32_find_disable_atm    := true;
 device.stm32_find_disable_armka  := false;
 device.stm32_find_disable_spgate := true;

 sfu := tSFUcmd.create;
 sfu.onWrite := device.Write;
 sfu.onLog := self.onLog;
 sfu.onInfoString := self.onInfoString;
 //sfu.onCommand := self.onCommand;

 boot := tSFUboot.create(sfu.send_command);
 boot.onLog := self.onLogBoot;
 boot.onERROR := self.OnDone_OnError;
 boot.onDone  := self.OnDone_OnError;
// boot.firmware_fname := 'E:\gsm\lab_flash\Gate_Tester_50 pcb_ver18_norm.bin';
// boot.firmware_fname := 'E:\Temp\flash_images_fsu_test\SpGate_MR.bin';
// boot.firmware_fname := 'E:\gsm\lab\Firmware\SPGateM_pcb16-4lay_ver 1.21 (MR) codec fix.bin';
// boot.firmware_fname := 'E:\Temp\flash_images_fsu_test\added.bin';
 boot.tx_free_func := device.tx_free_bytes;
 boot.tx_reset_func := device.TX_fifo_blocks.reset;

 sfu.onCommand := boot.recive_command;

 device.onRX := sfu.process_recive;

 InitializeTaskbarAPI;
 SetTaskbarProgressState(tbpsNone);

 milliseconds_start(ms_timer);
end;

procedure TForm1.FormShow(Sender: TObject);
begin
// device.Open;

 if DeviceEdit.Text = '' then
  DeviceEdit.Text := 'GM18_E_0010';

 if FirmwareEdit.Text = '' then
  FirmwareEdit.Text := 'E:\Temp\flash_images_fsu_test\SpGate_MR.bin';
//  FirmwareEdit.Text := 'E:\gsm\lab_flash\Gate_Tester_50 pcb_ver18_norm.bin';
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
end;

procedure TForm1.onLog(sender:tobject; msg:string);
begin
 log_dev.write_str(msg);
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
begin
 if device.State <> link_establish then exit;
 if StopCheckBox.Checked then exit;

 sfu.process_recive(device, nil, 0);
 boot.next_send;
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

 SFUboot_StatusLabel.Caption := boot.task_info;
 if (boot.task_done = false) and (boot.task_error = false) then SFUboot_StatusLabel.Font.Color := rgb(  0,   0,   0);
 if (boot.task_done =  true) and (boot.task_error = false) then SFUboot_StatusLabel.Font.Color := rgb(  0, 100,   0);
 if (boot.task_done =  true) and (boot.task_error =  true) then SFUboot_StatusLabel.Font.Color := rgb(200, 100, 100);
 if (boot.task_done = false) and (boot.task_error =  true) then SFUboot_StatusLabel.Font.Color := rgb(200,   0,   0);

 ProgressBar.Max := boot.progress_max;
 ProgressBar.Position := boot.progress_pos;

 if (device.State = link_establish) then
  begin
   if (boot.progress_max = 0) and (boot.progress_pos = 0) then
    SetTaskbarProgressState(tbpsIndeterminate)
   else
    begin
     SetTaskbarProgressState(tbpsNormal);
     SetTaskbarProgressValue(boot.progress_pos, boot.progress_max);
    end;
  end
 else
  begin
   SetTaskbarProgressValue(1, 1);
   if (boot.task_done =  true) and (boot.task_error =  true) then SetTaskbarProgressState(tbpsError);
   if (boot.task_done =  true) and (boot.task_error =  false) then SetTaskbarProgressState(tbpsNormal);
  end;
end;

procedure TForm1.OnDone_OnError;
begin
 device.close;
 if ExitCheckBox.Checked then
  begin
   sleep(100); Application.ProcessMessages;
   sleep(100); Application.ProcessMessages;
   sleep(100); Application.ProcessMessages;
   sleep(100); Application.ProcessMessages;
   self.Close;
   exit;
  end;

 GoButton.Enabled := true;
 FirmwareEdit.Enabled := true;
 DeviceEdit.Enabled := true;
 FastEraseCheckBox.Enabled := true;
 ResetCheckBox.Enabled := true;
 LabelDev.Enabled := true;
 LabelBin.Enabled := true;

 StopButton.Visible := false;
 GoButton.Visible := true;
end;

procedure TForm1.GoButtonClick(Sender: TObject);
var
 start_time : cardinal;
begin
 StopButton.Left := GoButton.left;
 StopButton.Top := GoButton.Top;
 StopButton.Visible := True;
 GoButton.Visible := False;

 GoButton.Enabled := false;
 FirmwareEdit.Enabled := false;
 DeviceEdit.Enabled := false;
 FastEraseCheckBox.Enabled := false;
 ResetCheckBox.Enabled := false;
 LabelDev.Enabled := false;
 LabelBin.Enabled := false;

 if ResetCheckBox.Checked then
  begin
   device.stm32_task_enable := true;
   device.stm32_task_stop := false;
   device.no_activate := false;
  end
 else
  begin
   device.stm32_task_enable := true;
   device.stm32_task_stop := false;
   device.no_activate := true;
  end;

 device.Open;
 start_time := GetTickCount;
 while (GetTickCount - start_time) < 5000 do
  begin
   if device.State = link_establish then
    break;
   Application.ProcessMessages;
   sleep(1);
  end;

 if device.State <> link_establish then
  begin
   device.Close;
   OnDone_OnError;
   exit;
  end;

 boot.firmware_fname := FirmwareEdit.Text;

 device.port_name := DeviceEdit.Text;
 device.port_name_serial := (device.port_name <> '');

 milliseconds_start(ms_timer);
 boot.start(true, FastEraseCheckBox.Checked);
end;

procedure TForm1.StopButtonClick(Sender: TObject);
begin
 OnDone_OnError;
 boot.RESET;
end;

end.
