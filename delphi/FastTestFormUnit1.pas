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
    FastCheckBox: TCheckBox;
    DeviceEdit: TEdit;
    ResetCheckBox: TCheckBox;
    LabelDev: TLabel;
    LabelBin: TLabel;
    FirmwareEdit: TEdit;
    StopButton: TButton;
    ExitCheckBox: TCheckBox;
    OpenFWButton: TButton;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Timer1mSTimer(Sender: TObject);
    procedure Timer100msTimer(Sender: TObject);
    procedure GoButtonClick(Sender: TObject);
    procedure StopButtonClick(Sender: TObject);
    procedure OpenFWButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

  private
    device : tCOMClient;
    sfu : tSFUcmd;
    boot : tSFUboot;

    cmd_ms_timer : tmillisecond_timer;
    dev_ms_timer : tmillisecond_timer;

    log_cmd : tfifo_blocks;
    log_dev : tfifo_blocks;

    current_dir : string;

    dev_filelog : file;
    cmd_filelog : file;

    auto_run : boolean;
    start_count : integer;
    form_closed : boolean; //TO DO: сделать нормально!

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

procedure logfile_create(var f:file; name:string);
begin
{$I-}
 AssignFile(f, name);
 rewrite(f, 1);
{$I+}
end;

procedure logfile_write_str(var f:file; str:string);
begin
 if str = '' then exit;
 str := str + #13;
{$I-}
 BlockWrite(f, str[1], length(str));
{$I+}
end;

////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////

procedure TForm1.FormCreate(Sender: TObject);
begin
 log_dev := tfifo_blocks.create($10000, $4000);
 log_cmd := tfifo_blocks.create($10000, $4000);

 self.DoubleBuffered := true;
 MemoDevice.DoubleBuffered := true;
 MemoCMD.DoubleBuffered := true;

 current_dir := GetCurrentDir;

 logfile_create(dev_filelog, 'dev.log');
 logfile_create(cmd_filelog, 'cmd.log');

 device := tCOMClient.Create($10000, $4000);//, $2000, $2000);

 device.onLog := self.onLog;
 device.onLogBegin := self.onLogBegin;
 device.onLogEnd := self.onLogEnd;

 device.port_name_serial := true;
 device.port_name := 'GM18_E_0010';
 device.port_speed := 115200;//921600;//500000;//
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
 boot.tx_free_func := device.tx_free_bytes;
 boot.tx_reset_func := device.TX_fifo_blocks.reset;

 sfu.onCommand := boot.recive_command;

 device.onRX := sfu.process_recive;

 InitializeTaskbarAPI;
 SetTaskbarProgressState(tbpsNone);

 milliseconds_start(cmd_ms_timer);
 milliseconds_start(dev_ms_timer);
end;


procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 FreeAndNil(device);
 FreeAndNil(sfu);
 FreeAndNil(boot);
 FreeAndNil(log_cmd);
 FreeAndNil(log_dev);

 Timer100ms.Enabled := false;
 Timer100ms.onTimer := nil;
 Timer1ms.Enabled := false;
 Timer1ms.onTimer := nil;

 form_closed := true;

{$I-}
 CloseFile(dev_filelog);
 CloseFile(cmd_filelog);
{$I+}
end;

////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////

procedure TForm1.FormShow(Sender: TObject);
var
 index : integer;
 str : string;
const
 DEV_KEY : string = '-CP210X:';
begin
 auto_run := false;
 if ParamCount > 0 then
  begin
   ResetCheckBox.Checked := false;
   FastCheckBox.Checked := false;
   ExitCheckBox.Checked := false;
   DeviceEdit.Text := '';
   FirmwareEdit.Text := '';

   index := 1;
   while index <= ParamCount do
    begin
     str := ParamStr(index);
     if UpperCase(str) = UpperCase('-reset') then ResetCheckBox.Checked := true else
     if UpperCase(str) = UpperCase('-RST')   then ResetCheckBox.Checked := true else
     if UpperCase(str) = UpperCase('-fast')  then FastCheckBox.Checked := true else
     if UpperCase(str) = UpperCase('-exit')  then ExitCheckBox.Checked := true else
     if UpperCase(str) = UpperCase('-go')    then auto_run := true else
     if UpperCase(str) = UpperCase('-run')   then auto_run := true else
     if UpperCase(str) = UpperCase('-start') then auto_run := true else

     if UpperCase(copy(str, 1, length(DEV_KEY))) = DEV_KEY then
      DeviceEdit.Text := copy(str, length(DEV_KEY)+1, length(str))
     else
      FirmwareEdit.Text := str;
     inc(index);
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////

procedure TForm1.onInfoString(sender:tobject; msg:string);
begin
 onLog(sender, msg);
end;

procedure TForm1.onLogBoot(sender:tobject; msg:string);
begin
 msg := inttostr(round(milliseconds_get(cmd_ms_timer))) + #9 + msg;
 milliseconds_start(cmd_ms_timer);

 msg := inttostr(device.TX_fifo_blocks.data_count)+#9 + msg;

 log_cmd.write_str(msg);
 //log_dev.write_str(':');
 logfile_write_str(cmd_filelog, msg);
end;

procedure TForm1.onLog(sender:tobject; msg:string);
begin
 msg := inttostr(round(milliseconds_get(dev_ms_timer))) + #9 + msg;
 milliseconds_start(dev_ms_timer);

 log_dev.write_str(msg);
 //log_cmd.write_str(':');
 logfile_write_str(dev_filelog, msg);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////

procedure TForm1.onLogBegin(sender:tLinkClient);
begin
 MemoDevice.Lines.BeginUpdate;
end;

procedure TForm1.onLogEnd(sender:tLinkClient);
begin
 MemoDevice.Lines.EndUpdate;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////

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

procedure TForm1.Timer1mSTimer(Sender: TObject);
begin
 if device = nil then exit;
 if sfu = nil then exit;
 if boot = nil then exit;

 if device.State <> link_establish then exit;
 if StopCheckBox.Checked then exit;

 sfu.process_recive(device, nil, 0);
 boot.next_send;
end;

procedure TForm1.Timer100msTimer(Sender: TObject);
begin
 if auto_run then
  begin
   auto_run := false;
   GoButton.Click;
   if form_closed then exit;
  end;

 if device = nil then exit;
 if sfu = nil then exit;
 if boot = nil then exit;

 if log_dev <> nil then
 if log_dev.blocks_count > 0 then
  begin
   MemoDevice.Lines.BeginUpdate;
   while log_dev.blocks_count > 0 do
    MemoDevice.Lines.Add(log_dev.read_str());
   MemoDevice.Lines.EndUpdate;
   SendMessage(MemoDevice.Handle, EM_LINESCROLL, 0, MemoDevice.Lines.Count);
  end;

 if log_cmd <> nil then
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
 if (boot.task_done =  true) and (boot.task_error =  true) then SFUboot_StatusLabel.Font.Color := rgb(200,   0,   0);
 if (boot.task_done = false) and (boot.task_error =  true) then SFUboot_StatusLabel.Font.Color := rgb(200,  50,  50);

 ProgressBar.Max := boot.progress_max;
 ProgressBar.Position := boot.progress_pos;

 if (start_count > 0) then
  if (device.State = link_establish) or (device.State = link_open) then
   begin
    if ((boot.progress_max = 0) and (boot.progress_pos = 0)) or (device.State = link_open) then
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
 sfu.recive_reset;

 if ExitCheckBox.Checked then
  begin
   sleep(100); Application.ProcessMessages;
   sleep(100); Application.ProcessMessages;
   sleep(100); Application.ProcessMessages;
   sleep(100); Application.ProcessMessages;

   if boot.task_error then
    ExitCode := 1
   else
    ExitCode := 0;

   self.Close;
   exit;
  end;

 GoButton.Enabled := true;
 DeviceEdit.Enabled := true;
 FirmwareEdit.Enabled := true;
 FastCheckBox.Enabled := true;
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
 inc(start_count);
 StopButton.Left := GoButton.left;
 StopButton.Top := GoButton.Top;
 StopButton.Visible := True;
 GoButton.Visible := False;

 GoButton.Enabled := false;
 DeviceEdit.Enabled := false;
 FirmwareEdit.Enabled := false;
 FastCheckBox.Enabled := false;
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
   if device.State in [link_error, link_idle, link_establish, link_close] then
    break;
   Application.ProcessMessages;
   if form_closed then
    exit;
   sleep(1);
  end;

 if device.State <> link_establish then
  begin
   boot.task_error := true;
   boot.task_info := 'Device open timeout ERROR';
   onLog(self, 'Device open timeout ERROR');
   device.Close;
   OnDone_OnError;
   exit;
  end;

 boot.firmware_fname := FirmwareEdit.Text;

 device.port_name := DeviceEdit.Text;
 device.port_name_serial := (device.port_name <> '');

 milliseconds_start(cmd_ms_timer);
 milliseconds_start(dev_ms_timer);
 boot.start(true, FastCheckBox.Checked);
end;

procedure TForm1.StopButtonClick(Sender: TObject);
begin
 OnDone_OnError;
 boot.RESET;
end;

procedure TForm1.OpenFWButtonClick(Sender: TObject);
var
 OpenDialog : TOpenDialog;
begin
 OpenDialog := TOpenDialog.Create(self);
 OpenDialog.DefaultExt := 'bin';
 OpenDialog.FileName := '';
 OpenDialog.Filter   := '(*.bin)|*.bin';
// OpenDialog.Filter   := '(*.bin; *.hex)|*.bin;*.hex';
 opendialog.Options  := [ofHideReadOnly,ofEnableSizing];

 if not OpenDialog.Execute then
  begin
   {$I-}
   ChDir(current_dir);
   if IOResult<>0 then
    MemoDevice.Lines.Add('ERROR: Can''t change dir to '+current_dir);
   {$I+}
   OpenDialog.Free;
   exit;
  end;

 {$I-}
 ChDir(current_dir);
 if IOResult<>0 then
  MemoDevice.Lines.Add('ERROR: Can''t change dir to '+current_dir);
 {$I+}

 FirmwareEdit.Text := openDialog.FileName;
 FirmwareEdit.SelStart  := length(FirmwareEdit.Text);
 FirmwareEdit.SelLength := 0;

 FreeAndNil(OpenDialog);
end;

end.
