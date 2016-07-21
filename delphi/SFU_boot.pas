unit SFU_boot;

interface

type
 tSFUboot_CommandSend = function(code:byte; cmd_body:pointer = nil; size:word = 0):cardinal of object;
 tSFUboot_EventLog = procedure(sender:tobject; msg:string) of object;
 tSFUboot_tx_free_func = function:integer of object;
 tSFUboot_tx_reset_func = procedure of object;
 tSFUboot_Event = procedure of object;

 tSFUboot = class
 private
  CommandSend : tSFUboot_CommandSend;

  firmware_buf : array[0.. $400000 - 1] of byte;
  firmware_size : cardinal;
  firmware_crc  : cardinal;
  firmware_addr : cardinal;
  firmware_start : cardinal;
  firmware_end_addr : cardinal;
  firmware_last_block : cardinal;

  info_done : boolean;
  load_done : boolean;
  erase_done : boolean;
  write_done : boolean;
  start_done : boolean;

  write_restart : boolean;

  send_timeout : cardinal;

  info_chip_id : array[0..11] of byte;
  info_dev_type : word;
  info_dev_rev  : word;
  info_flash_size : word;
  info_boot_ver : word;
  info_addr_from : cardinal;
  info_addr_run : cardinal;
  info_rx_size : cardinal;

  start_time : cardinal;

  last_sended_addr : cardinal;
  last_writed_addr : cardinal;
  last_corrected_addr : cardinal;

  write_block_packet_size : cardinal;
  prewrite_bytes_free : cardinal;
  prewrite_bytes_writed : cardinal;

  procedure log(msg:string);
  procedure log_block_hex(msg:string; block:pbyte; count:integer);
  procedure log_block_ascii(msg:string; block:pbyte; count:integer);

  procedure recive_info(body:pbyte; count:word);
  procedure recive_erase_done(body:pbyte; count:word);
  procedure recive_erase_page(body:pbyte; count:word);
  procedure recive_write(body:pbyte; count:word);
  procedure recive_start(body:pbyte; count:word);
  procedure recive_hw_reset(msg:string);

  procedure error_stop(msg:string);

  procedure send_info;
  procedure send_erase;
  procedure send_start;

  function  send_write:cardinal;

  procedure send_write_restart;
  procedure send_write_multi(count:integer);

  procedure firmware_load;

 public
  onLog : tSFUboot_EventLog;
  onERROR: tSFUboot_Event;
  onDone: tSFUboot_Event;

  tx_free_func : tSFUboot_tx_free_func;
  tx_reset_func : tSFUboot_tx_reset_func;

  firmware_fname : string;

  progress_max : cardinal;
  progress_pos : cardinal;

  task_done : boolean;
  task_error : boolean;
  task_info : ansistring;

  opt_fast_erase : boolean;
  opt_prewrite : boolean;

  constructor create(send: tSFUboot_CommandSend);

  procedure recive_command(code:byte; body:pbyte; count:word);
  procedure next_send;

  procedure RESET;
  procedure start;
 end;

implementation

uses
  SysUtils, windows, math,
  crcunit,
  SFU_other;

const
 SFU_CMD_INFO  = $97;
 SFU_CMD_ERASE = $C5;
 SFU_CMD_PAGE  = $B3;
 SFU_CMD_WRITE = $38;
 SFU_CMD_START = $26;

 SFU_CMD_TIMEOUT = $AA;
 SFU_CMD_WRERROR = $55;
 SFU_CMD_HWRESET = $11;

const
 WRITE_BLOCK_SIZE = 2048;

const
 TIMEOUT_ERASE = 2000;
 TIMEOUT_WRITE = 300;
 TIMEOUT_RESTART = 500;
 TIMEOUT_DEFAULT = 200;

////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////

constructor tSFUboot.create(send: tSFUboot_CommandSend);
begin
 CommandSend := send;
 RESET();
end;

////////////////////////////////////////////////////////////////////////////////////////

procedure tSFUboot.firmware_load;
var
 readed : cardinal;
begin
 firmware_size := 0;

 FillChar(firmware_buf[0], sizeof(firmware_buf), $FF);

 readed := 0;
 if not stm32_load_file(firmware_fname, @firmware_buf[0], sizeof(firmware_buf), @readed) then
  begin
   error_stop('ERROR: firmware_load(' + firmware_fname + ')');
   exit;
  end;
 if readed = 0 then exit;

 readed := (readed + 3) and $FFFFFFFC;
 firmware_size := readed;
 firmware_crc := crc_stm32(pcardinal(@firmware_buf[0]), firmware_size div 4);

 firmware_end_addr := info_addr_from + firmware_size;
 firmware_last_block := ((firmware_end_addr - 1) div WRITE_BLOCK_SIZE) * WRITE_BLOCK_SIZE;

 log('firmware_load(''' + firmware_fname + ''') OK');
 log('firmware_size: ' + IntToStr(firmware_size));
 log('firmware_from: 0x' + IntToHex(firmware_start, 8));
 log('firmware_to  : 0x' + IntToHex(firmware_end_addr, 8));
 log('firmware_last: 0x' + IntToHex(firmware_last_block, 8));
 log('firmware_crc : 0x' + IntToHex(firmware_crc, 8));
 log(' ');

 load_done := true;
 send_timeout := GetTickCount;
end;

////////////////////////////////////////////////////////////////////////////////////////

procedure tSFUboot.log(msg:string);
begin
 if @onlog<>nil then
  onLog(self, msg);
 task_info := msg;
end;

procedure tSFUboot.log_block_hex(msg:string; block:pbyte; count:integer);
var
 str : string;
begin
 if @onlog = nil then exit;
 str := msg;
 while count > 0 do
  begin
   str := str + inttohex(block^, 2) + ' ';
   inc(block);
   dec(count);
  end;
 onLog(self, str);
end;

procedure tSFUboot.log_block_ascii(msg:string; block:pbyte; count:integer);
var
 str : string;
begin
 if @onlog = nil then exit;
 str := msg;
 while count > 0 do
  begin
   if block^ in [9, 32..127] then
    str := str + ansichar(block^)
   else
    str := str + '<' + inttohex(block^, 2) + '>';
   inc(block);
   dec(count);
  end;
 onLog(self, str);
end;

//////////////////////////////////////////////////////////////////////////////////////////

procedure tSFUboot.recive_start(body:pbyte; count:word);
var
 crc_from  : cardinal;
 crc_size  : cardinal;
 crc_value : cardinal;
begin
 if count <> 12 then
  begin
   error_stop('ERROR: recive_start: count <> 12');
   exit;
  end;

 crc_from  := body_get_cardinal(body, count);
 crc_size  := body_get_cardinal(body, count);
 crc_value := body_get_cardinal(body, count);

 log('Command: Check and START');
 log('crc_from  : 0x' + inttohex(crc_from, 8));
 log('crc_to    : 0x' + inttohex(crc_from + crc_size, 8));
 log('crc_value : 0x' + inttohex(crc_value, 8));

 if crc_value = firmware_crc then
  log('CRC OK')
 else
  begin
   error_stop('ERROR: Firmware CRC wrong');
   exit;
  end;

 log(' ');

 start_done := true;
 send_timeout := GetTickCount;
end;

procedure tSFUboot.recive_info(body:pbyte; count:word);
begin
 if count <> 32 then
  begin
   error_stop('ERROR: recive_info: count <> 32');
   exit;
  end;

 body_get_block(body, count, @info_chip_id[0], sizeof(info_chip_id));

 info_dev_type   := body_get_word(body, count);
 info_dev_rev    := body_get_word(body, count);
 info_flash_size := body_get_word(body, count);
 info_boot_ver   := body_get_word(body, count);

 info_rx_size   := body_get_cardinal(body, count);
 info_addr_from := body_get_cardinal(body, count);
 info_addr_run  := body_get_cardinal(body, count);

 log('Command: INFO');
 log_block_hex('ChipID: ', @info_chip_id[0], sizeof(info_chip_id));
 log_block_ascii('ChipID: ', @info_chip_id[0], sizeof(info_chip_id));
 log('Dev type  : 0x' + inttohex(info_dev_type, 4));
 log('Dev rev   : 0x' + inttohex(info_dev_rev, 4));
 log('FlashSize : '   + inttostr(info_flash_size * 1024));
 log('Boot ver  : 0x' + inttohex(info_boot_ver, 4));
 log('RX fifo   : 0x' + inttohex(info_rx_size, 8));
 log('Addr from : 0x' + inttohex(info_addr_from, 8));
 log('Addr run  : 0x' + inttohex(info_addr_run, 8));
 log(' ');

 firmware_start := info_addr_from;
 firmware_addr  := info_addr_from;

 prewrite_bytes_free := info_rx_size - WRITE_BLOCK_SIZE*2;
 prewrite_bytes_writed := 0;

 info_done := true;
 send_timeout := GetTickCount;
end;

procedure tSFUboot.recive_erase_page(body:pbyte; count:word);
var
 log_str : string;
 sended_count : integer;
 writed : cardinal;
begin
 log_str := 'Erase sector #' + inttostr(body_get_cardinal(body, count) + 1);

 if opt_prewrite then
  if @tx_free_func <> nil then
   begin
    sended_count := 0;
    while (tx_free_func() >= write_block_packet_size) and
          (sended_count < 64) and
          (prewrite_bytes_free >= write_block_packet_size) and
          (prewrite_bytes_writed < firmware_size) do
     begin
      writed := send_write();

      inc(prewrite_bytes_writed, writed);
      inc(sended_count);
      dec(prewrite_bytes_free, write_block_packet_size);
     end;
   if sended_count > 0 then
    log_str := log_str + '. Prewrite packets: ' + inttostr(sended_count) + ', free : ' + inttostr(prewrite_bytes_free);
  end;

 log(log_str);
 send_timeout := GetTickCount + TIMEOUT_ERASE;
end;

procedure tSFUboot.recive_erase_done(body:pbyte; count:word);
begin
 if (count <> 4) then
  begin
   error_stop('ERROR: Erase command');
   exit;
  end
 else
  log('Erase DONE');
 log(' ');

 if opt_prewrite then
  begin
   if prewrite_bytes_writed < firmware_size then
    send_write_multi((firmware_size - prewrite_bytes_writed) div WRITE_BLOCK_SIZE);
  end
 else
  send_write_multi(64);

 erase_done := true;
 send_timeout := GetTickCount + TIMEOUT_WRITE;
end;

procedure tSFUboot.recive_write(body:pbyte; count:word);
var
 addr : cardinal;
 rxed : cardinal;
begin
 if count <> 8 then
  begin
   log('ERROR: write command recive count <> 12');
   exit;
  end;

 addr := body_get_cardinal(body, count);
 rxed := body_get_cardinal(body, count);

 log('WR: 0x'+inttohex(addr, 8) + #9 + inttostr(rxed));

 if not write_done then
  begin
   if addr >= firmware_end_addr then
    begin
     if @tx_reset_func <> nil then
      tx_reset_func();

     write_done := true;
     progress_max := firmware_size;
     progress_pos := progress_max;
     log('Write Done');
     log(' ');
     send_timeout := GetTickCount;
     exit;
    end;

   if write_restart then
    begin
     write_restart := false;
     firmware_addr := addr;

     send_write_multi(64);
    end
   else
    begin
     if (last_writed_addr = addr) and (last_corrected_addr < addr)  then
      begin
       firmware_addr := addr;
       last_corrected_addr := addr;

       log('WR address corrected to 0x' + inttohex(last_corrected_addr, 8));

       if @tx_reset_func <> nil then
        begin
         tx_reset_func();
         send_write_multi(4);
        end;
      end;

     if last_sended_addr < firmware_last_block then
      send_write;
    end;
  end;

 progress_max := firmware_size;
 progress_pos := addr - firmware_start;

 last_writed_addr := addr;
 send_timeout := GetTickCount + TIMEOUT_WRITE;
end;

procedure tSFUboot.error_stop(msg:string);
begin
 RESET();
 task_error := true;
 log(msg);

 if @onError <> nil then
  onError();
end;

procedure tSFUboot.recive_hw_reset(msg:string);
begin
 if erase_done then
  error_stop(msg)
 else
  log(msg);
end;

////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////

procedure tSFUboot.recive_command(code:byte; body:pbyte; count:word);
begin
 if code = SFU_CMD_INFO  then recive_info(body, count) else
 if code = SFU_CMD_ERASE then recive_erase_done(body, count) else
 if code = SFU_CMD_PAGE  then recive_erase_page(body, count) else
 if code = SFU_CMD_WRITE then recive_write(body, count) else
 if code = SFU_CMD_START then recive_start(body, count) else

 if code = SFU_CMD_TIMEOUT then error_stop('ERROR command: TIMEOUT') else
 if code = SFU_CMD_HWRESET then recive_hw_reset('ERROR command: H/W RESET') else
 if code = SFU_CMD_WRERROR then error_stop('ERROR command: Flash write') else
  log_block_hex('Error unknow command (' + inttohex(code, 2) + ')', body, count);
end;

////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////

procedure tSFUboot.send_info;
begin
 log('Send command: INFO');
 CommandSend(SFU_CMD_INFO);
end;

procedure tSFUboot.send_erase;
var
 info : array[0..3] of byte;
begin
 if opt_fast_erase then
  begin
   info[0] := (firmware_size shr  0) and $FF;
   info[1] := (firmware_size shr  8) and $FF;
   info[2] := (firmware_size shr 16) and $FF;
   info[3] := (firmware_size shr 24) and $FF;
  end
 else
  begin
   info[0] := $FF;
   info[1] := $FF;
   info[2] := $FF;
   info[3] := $FF;
  end;
 log('Send command: Erase');
 CommandSend(SFU_CMD_ERASE, @info, sizeof(info));
 send_timeout := GetTickCount + TIMEOUT_ERASE;
end;

function tSFUboot.send_write;
var
 body : array[0 .. WRITE_BLOCK_SIZE + 4 - 1] of byte;
 count : integer;
 pos : integer;
begin
 result := 0;
 if @tx_free_func <> nil then
  if tx_free_func() < write_block_packet_size then
   exit;

 body[0] := (firmware_addr shr  0) and $FF;
 body[1] := (firmware_addr shr  8) and $FF;
 body[2] := (firmware_addr shr 16) and $FF;
 body[3] := (firmware_addr shr 24) and $FF;

 pos := firmware_addr - firmware_start;
 if (pos < 0) or (pos > (Length(firmware_buf) - WRITE_BLOCK_SIZE)) then
  begin
   error_stop('ERROR: tSFUboot.send_write:  pos = ' + inttostr(pos));
   exit;
  end;

 if WRITE_BLOCK_SIZE > (firmware_size - pos) then
  count := firmware_size - pos
 else
  count := WRITE_BLOCK_SIZE;

 //log('Send: 0x' + inttohex(firmware_addr, 8) + ' ' + inttostr(count)); //log('');

 if count <= 0 then
  exit;

 move(firmware_buf[pos], body[4], count);
 result := CommandSend(SFU_CMD_WRITE, @body[0], count + 4);
 write_block_packet_size := result;
 
 last_sended_addr := firmware_addr;

 if (firmware_addr - firmware_start) + count < firmware_size then
  firmware_addr := firmware_addr + count;
end;

procedure tSFUboot.send_write_multi(count:integer);
var
 pos : integer;
begin
 for pos := 0 to count-1 do
  begin
   if @tx_free_func <> nil then
    if tx_free_func() < write_block_packet_size then
     break;
   send_write;
  end;
end;

procedure tSFUboot.send_write_restart;
begin
 log('Send command: Write(RESART)');
 write_restart := true;
 CommandSend(SFU_CMD_WRITE);
 send_timeout := GetTickCount + TIMEOUT_RESTART;
end;

procedure tSFUboot.send_start;
begin
 log('Send command: Check and START');
 CommandSend(SFU_CMD_START, @firmware_crc, sizeof(firmware_crc));
end;

////////////////////////////////////////////////////////////////////////////////////////////

procedure tSFUboot.RESET();
begin
 if start_time <> 0 then
  begin
   if @onLog <> nil then
    onLog(self, 'Total time : ' + inttostr(GetTickCount - start_time));
   start_time := 0;
  end;

 task_done := true;
 task_error := false;

 info_done := false;
 load_done := false;
 erase_done := false;
 write_done := false;
 start_done := false;

 write_restart := false;

 progress_pos := 0;
 progress_max := 0;

 firmware_size  := 0;
 firmware_addr  := 0;
 firmware_start := 0;

 write_restart := false;

 send_timeout := 0;

 last_writed_addr := 0;
 last_sended_addr := 0;
 last_corrected_addr := 0;

 FillChar(firmware_buf[0], sizeof(firmware_buf), $FF);
 FillChar(info_chip_id, sizeof(info_chip_id), 0);

 info_dev_type := 0;
 info_dev_rev  := 0;
 info_flash_size := 0;
 info_boot_ver   := 0;
 info_addr_from  := 0;
 info_addr_run   := 0;

 write_block_packet_size := WRITE_BLOCK_SIZE * 2;
 prewrite_bytes_free := 0;
 prewrite_bytes_writed := 0;
end;

procedure tSFUboot.start;
begin
 if firmware_fname = '' then
  begin
   error_stop('ERROR: firmware_fname =''''');
   exit;
  end;

 if not FileExists(firmware_fname) then
  begin
   error_stop('ERROR: firmware not exist: ' + firmware_fname);
   exit;
  end;

 log(' ');
 log('New TASK');
 log('Firmware: ' + firmware_fname);
 log('opt_fast_erase: ' + BoolToStr(opt_fast_erase, true));
 log('opt_prewrite:   ' + BoolToStr(opt_prewrite, true));
 log('WRITE_BLOCK_SIZE: '+IntToStr(WRITE_BLOCK_SIZE));
 log('tx_free_func:  0x' + inttohex(cardinal(TMethod(tx_free_func).code), 8));
 log('tx_reset_func: 0x' + inttohex(cardinal(TMethod(tx_reset_func).code), 8));
 log(' ');

 self.RESET();

 start_time := GetTickCount;

 task_info := '';
 task_done := false;
 task_error := false;
end;

procedure tSFUboot.next_send;
begin
 if task_done then exit;
 if GetTickCount < send_timeout then exit;
 send_timeout := GetTickCount + TIMEOUT_DEFAULT;

 if info_done  = false then send_info() else
 if load_done  = false then firmware_load() else
 if erase_done = false then send_erase() else
 if write_done = false then send_write_restart() else
 if start_done = false then send_start() else
  begin
   RESET;

   progress_max := 1;
   progress_pos := 1;

   log('Task DONE ['+datetostr(date)+' '+TimeToStr(time)+']');
   task_done := true;

   if @onDone <> nil then
    onDone();
  end;
end;

end.
