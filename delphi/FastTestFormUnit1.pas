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
    Timer32ms: TTimer;
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
    Cap1Label: TLabel;
    Cap2Label: TLabel;
    PreWriteCheckBox: TCheckBox;
    ErrorsKeepCheckBox: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Timer1mSTimer(Sender: TObject);
    procedure Timer32msTimer(Sender: TObject);
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

    log_fifo_cmd : tfifo_blocks;
    log_fifo_dev : tfifo_blocks;

    current_dir : string;

    dev_filelog : file;
    cmd_filelog : file;

    auto_run : boolean;
    start_count : integer;
    form_closed : boolean; //TO DO: сделать нормально!

    procedure LogAdd(log_fifo:tfifo_blocks; msg:string);
    procedure LogMemoUpdate(log_fifo:tfifo_blocks; memo:tmemo);

    procedure onLogDev(sender:tobject; msg:string);
    procedure onLogBoot(sender:tobject; msg:string);

    procedure OnDone_OnError;

    procedure settings_save;
    function settings_load:boolean;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}
//{$R windowsxp.RES}

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
 log_fifo_dev := tfifo_blocks.create($10000, $4000);
 log_fifo_cmd := tfifo_blocks.create($10000, $4000);

 self.DoubleBuffered := true;
 MemoDevice.DoubleBuffered := true;
 MemoCMD.DoubleBuffered := true;

 current_dir := GetCurrentDir;

 logfile_create(dev_filelog, 'dev.log');
 logfile_create(cmd_filelog, 'cmd.log');

 device := tCOMClient.Create($10000, $4000);//, $2000, $2000);

 device.onLog := self.onLogDev;

 device.port_speed := 921600;//500000;//115200;//
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
 sfu.onLog := self.onLogDev;
 sfu.onInfoString := self.onLogDev;
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
 if device.State <> link_idle then ExitCode := 2 else
 if boot.task_error then           ExitCode := 1 else
  ExitCode := 0;

 if paramcount = 0 then
  settings_save;

 FreeAndNil(device);
 FreeAndNil(sfu);
 FreeAndNil(boot);
 FreeAndNil(log_fifo_cmd);
 FreeAndNil(log_fifo_dev);

 Timer32ms.Enabled := false;
 Timer32ms.onTimer := nil;
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
 DEV_KEY : string = '-DEV:';
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
     if UpperCase(str) = UpperCase('-no-Errors-Keep')  then ErrorsKeepCheckBox.Checked := false else
     if UpperCase(str) = UpperCase('-no-Prewrite')  then PreWriteCheckBox.Checked := false else
     if UpperCase(str) = UpperCase('-go')    then auto_run := true else
     if UpperCase(str) = UpperCase('-run')   then auto_run := true else
     if UpperCase(str) = UpperCase('-start') then auto_run := true else


     if UpperCase(copy(str, 1, length(DEV_KEY))) = DEV_KEY then
      DeviceEdit.Text := copy(str, length(DEV_KEY)+1, length(str))
     else
      FirmwareEdit.Text := str;
     inc(index);
    end;
  end
 else
  settings_load;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////

procedure TForm1.LogAdd(log_fifo:tfifo_blocks; msg:string);
begin
 log_fifo.write_str(msg);
 //log_fifo_dev.write_str(':');
 if log_fifo = log_fifo_dev then logfile_write_str(dev_filelog, msg);
 if log_fifo = log_fifo_cmd then logfile_write_str(cmd_filelog, msg);
end;

procedure TForm1.LogMemoUpdate(log_fifo:tfifo_blocks; memo:tmemo);
begin
 if log_fifo <> nil then
 if log_fifo.blocks_count > 0 then
  begin
   memo.Lines.BeginUpdate;
   while log_fifo.blocks_count > 0 do
    memo.Lines.Add(log_fifo.read_str());
   memo.Lines.EndUpdate;
   SendMessage(memo.Handle, EM_LINESCROLL, 0, memo.Lines.Count);
  end;
end;

procedure TForm1.onLogBoot(sender:tobject; msg:string);
begin
 msg := inttostr(device.TX_fifo_blocks.data_count) + #9 + msg;

 msg := inttostr(round(milliseconds_get(cmd_ms_timer))) + #9 + msg;
 milliseconds_start(cmd_ms_timer);

 LogAdd(log_fifo_cmd, msg);
end;

procedure TForm1.onLogDev(sender:tobject; msg:string);
begin
 msg := inttostr(round(milliseconds_get(dev_ms_timer))) + #9 + msg;
 milliseconds_start(dev_ms_timer);

 LogAdd(log_fifo_dev, msg);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////

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

procedure TForm1.Timer32msTimer(Sender: TObject);
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

 LogMemoUpdate(log_fifo_dev, MemoDevice);
 LogMemoUpdate(log_fifo_cmd, MemoCMD);

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
    '  crc: ' + inttostr(sfu.stat_error_crc) +
    '     Host:' +
    ' over: ' + inttostr(device.TX_fifo_blocks.stat_blocks_overfulled + device.TX_fifo_blocks.stat_bytes_overfulled) +
    ' lost: ' + inttostr(device.TX_fifo_blocks.stat_blocks_lost + device.TX_fifo_blocks.stat_bytes_lost) +
    '';

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
  if (ErrorsKeepCheckBox.Checked = false) or (boot.task_error = false) then
   begin
    sleep(100); Application.ProcessMessages;
    sleep(100); Application.ProcessMessages;
    sleep(100); Application.ProcessMessages;
    sleep(100); Application.ProcessMessages;

    self.Close;
    exit;
   end;

 GoButton.Enabled := true;
 DeviceEdit.Enabled := true;
 FirmwareEdit.Enabled := true;
 FastCheckBox.Enabled := true;
 ResetCheckBox.Enabled := true;
 PreWriteCheckBox.Enabled := true;
 LabelDev.Enabled := true;
 LabelBin.Enabled := true;

 StopButton.Visible := false;
 GoButton.Visible := true;
end;

procedure TForm1.GoButtonClick(Sender: TObject);
var
 start_time : cardinal;
begin
 if (MemoCMD.Lines.Count <> 0) or
    (MemoDevice.Lines.Count <> 0) then
  begin
   LogAdd(log_fifo_cmd, ' ');
   LogAdd(log_fifo_dev, ' ');
   LogAdd(log_fifo_cmd, '====================================================================');
   LogAdd(log_fifo_dev, '====================================================================');
   LogAdd(log_fifo_cmd, '====================================================================');
   LogAdd(log_fifo_dev, '====================================================================');
   LogAdd(log_fifo_cmd, ' ');
   LogAdd(log_fifo_dev, ' ');
  end;

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
 PreWriteCheckBox.Enabled := false;
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

 device.port_name := DeviceEdit.Text;
 device.port_name_serial := (device.port_name <> '') and
                            (copy(device.port_name, 1, 3) <> 'COM') and
                            (system.pos('USB#VID', device.port_name) = 0);

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
   onLogDev(self, 'Device open timeout ERROR');
   device.Close;
   OnDone_OnError;
   exit;
  end;

 boot.firmware_fname := FirmwareEdit.Text;
 boot.opt_prewrite := PreWriteCheckBox.Checked;
 boot.opt_fast_erase := FastCheckBox.Checked;

 milliseconds_start(cmd_ms_timer);
 milliseconds_start(dev_ms_timer);
 boot.start;
end;

procedure TForm1.StopButtonClick(Sender: TObject);
begin
 boot.abort;
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

procedure TForm1.settings_save;
var
 settings : tstringlist;
begin
 settings := TStringList.Create;

 settings.Add(FirmwareEdit.Text);
 settings.Add(DeviceEdit.Text);
 settings.Add(BoolToStr(FastCheckBox.Checked,  false));
 settings.Add(BoolToStr(ResetCheckBox.Checked, false));
 settings.Add(BoolToStr(ExitCheckBox.Checked,  false));
 settings.Add(inttostr(self.left));
 settings.Add(inttostr(self.Top));
 settings.Add(BoolToStr(PreWriteCheckBox.Checked,   false));
 settings.Add(BoolToStr(ErrorsKeepCheckBox.Checked, false));

 settings.SaveToFile(ParamStr(0)+'.config');
end;

function TForm1.settings_load:boolean;
var
 settings : tstringlist;
 fname : string;
begin
 result := false;
 fname := ParamStr(0)+'.config';
 if not FileExists(fname) then exit;
 settings := TStringList.Create;

 try
  settings.LoadFromFile(fname);

  FirmwareEdit.Text := settings.Strings[0];
  DeviceEdit.Text   := settings.Strings[1];
  FastCheckBox.Checked  := settings.Strings[2] <> '0';
  ResetCheckBox.Checked := settings.Strings[3] <> '0';
  ExitCheckBox.Checked  := settings.Strings[4] <> '0';
  self.left := StrToInt(settings.Strings[5]);
  self.top  := StrToInt(settings.Strings[6]);
  PreWriteCheckBox.Checked    := settings.Strings[7] <> '0';
  ErrorsKeepCheckBox.Checked  := settings.Strings[8] <> '0';

  result := true;
 except
  FreeAndNil(settings);
  exit;
 end;
end;

end.
