unit SFU_boot;

interface
uses
 linkclient;

type
 tSFUboot_CommandSend = procedure (code:byte; cmd_body:pointer = nil; size:word = 0) of object;
 tSFUboot_EventLog=procedure(sender:tobject; msg:string) of object;
 tSFUboot_func_Free=function:integer of object;

 tSFUboot = class
 private
  CommandSend : tSFUboot_CommandSend;

  firmware_buf : array[0.. $400000 - 1] of byte;
  firmware_size : cardinal;
  firmware_addr : cardinal;
  firmware_start : cardinal;

  last_block_addr : cardinal;

  info_done : boolean;
  erase_done : boolean;
  write_done : boolean;
  write_restart : boolean;

  send_timeout : cardinal;

  info_chip_id : array[0..11] of byte;
  info_dev_type : word;
  info_dev_rev  : word;
  info_flash_size : word;
  info_boot_ver : word;
  info_addr_from : cardinal;
  info_addr_run : cardinal;

  procedure log(msg:string);
  procedure log_block_hex(msg:string; block:pbyte; count:integer);
  procedure log_block_ascii(msg:string; block:pbyte; count:integer);

  procedure recive_info(body:pbyte; count:word);
  procedure recive_erase(body:pbyte; count:word);
  procedure recive_erase_part(body:pbyte; count:word);
  procedure recive_write(body:pbyte; count:word);
  procedure recive_error(body:pbyte; count:word; msg:string);

  procedure send_info;
  procedure send_erase(all:boolean = true; size:cardinal=$FFFFFFFF);
  procedure send_write;
  procedure send_write_restart;
  procedure send_write_multi;

  function firmware_load:boolean;

 public
  onLog : tSFUboot_EventLog;
  func_free : tSFUboot_func_Free;

  firmware_fname : string;

  progress_max : cardinal;
  progress_pos : cardinal;

  task_done : boolean;
  task_error : boolean;

  constructor create(send: tSFUboot_CommandSend);
  procedure recive_command(code:byte; body:pbyte; count:word);
  procedure next_send;

  procedure RESET;
  procedure start;
 end;

implementation

uses
  SysUtils, windows, math;

const
 SFU_CMD_INFO  = $97;
 SFU_CMD_ERASE = $C5;
 SFU_CMD_WRITE = $38;
 SFU_CMD_TIMEOUT = $AA;
 SFU_CMD_ERROR   = $55;
 SFU_CMD_HWRESET = $11;
 SFU_CMD_ERASE_PART = $B3;

const
 WRITE_BLOCK_SIZE = 2048;

function stm32_load_file(file_name:string; data:pointer; max_size:cardinal; readed:pcardinal) : boolean;
var
 f:file;
 v_readed:cardinal;
 old_mode : integer;
begin
 result := false;
 fillchar(data^, max_size, $FF);
 if readed <> nil then
  readed^:=0;

 {$I-}
 old_mode := filemode;
 filemode := fmOpenReadWrite;
 AssignFile(f, file_name);
 reset(f,1);
 filemode := old_mode;
 if IOResult<>0 then exit;

 if filesize(f)>100000000 then
  begin
   CloseFile(f); ioresult;
   exit;
  end;

 v_readed := 0;
 BlockRead(f, data^, max_size, v_readed);
 if IOResult<>0 then
  begin
   CloseFile(f); ioresult;
   exit;
  end;

 CloseFile(f); ioresult;
 {$I+}

 if readed <> nil then
  readed^ :=v_readed;
 result:=true;
end;

////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////

constructor tSFUboot.create(send: tSFUboot_CommandSend);
begin
 CommandSend := send;
 RESET();
end;

////////////////////////////////////////////////////////////////////////////////////////

function tSFUboot.firmware_load:boolean;
var
 readed : cardinal;
begin
 result := false;
 firmware_size := 0;
 if firmware_fname = '' then exit;

 FillChar(firmware_buf[0], sizeof(firmware_buf), $FF);

 readed := 0;
 if not stm32_load_file(firmware_fname, @firmware_buf[0], sizeof(firmware_buf), @readed) then exit;
 if readed = 0 then exit;

 readed := (readed + 3) and $FFFFFFFC;
 firmware_size := readed;
 result := true;
end;

////////////////////////////////////////////////////////////////////////////////////////

procedure tSFUboot.log(msg:string);
begin
 if @onlog<>nil then
  onLog(self, msg);
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

function body_get_byte(var body:pbyte; var count:word):byte;
begin
 result := 0;
 if count < 1 then exit;
 if body = nil then exit;
 result := body^;
 inc(body);
 dec(count);
end;

procedure body_get_block(var body:pbyte; var count:word; block_ptr:pbyte; block_size:integer);
begin
 while (block_size > 0) do
  begin
   block_ptr^ := body_get_byte(body, count);
   inc(block_ptr);
   dec(block_size);
  end;
end;

function body_get_word(var body:pbyte; var count:word):word;
begin
 result := 0;
 result := result or word(body_get_byte(body, count)) shl 0;
 result := result or word(body_get_byte(body, count)) shl 8;
end;

function body_get_cardinal(var body:pbyte; var count:word):cardinal;
begin
 result := 0;
 result := result or cardinal(body_get_word(body, count)) shl  0;
 result := result or cardinal(body_get_word(body, count)) shl 16;
end;

//////////////////////////////////////////////////////////////////////////////////////////

procedure tSFUboot.recive_info(body:pbyte; count:word);
begin
 if count <> 28 then exit;

 body_get_block(body, count, @info_chip_id[0], sizeof(info_chip_id));

 info_dev_type   := body_get_word(body, count);
 info_dev_rev    := body_get_word(body, count);
 info_flash_size := body_get_word(body, count);
 info_boot_ver   := body_get_word(body, count);

 info_addr_from := body_get_cardinal(body, count);
 info_addr_run  := body_get_cardinal(body, count);

 info_done := true;

 log('Command: INFO');
 log_block_hex('ChipID: ', @info_chip_id[0], sizeof(info_chip_id));
 log_block_ascii('ChipID: ', @info_chip_id[0], sizeof(info_chip_id));
 log('Dev type  : 0x' + inttohex(info_dev_type, 4));
 log('Dev rev   : 0x' + inttohex(info_dev_rev, 4));
 log('FlashSize : ' + inttostr(info_flash_size * 1024));
 log('Boot ver  : 0x' + inttohex(info_boot_ver, 4));
 log('Addr from : 0x' + inttohex(info_addr_from, 8));
 log('Addr run  : 0x' + inttohex(info_addr_run, 8));
 log(' ');
end;

procedure tSFUboot.recive_erase_part(body:pbyte; count:word);
begin
 log('Erase part #' + inttostr(body_get_cardinal(body, count)));
 send_timeout := GetTickCount + 2000;
end;

procedure tSFUboot.recive_erase(body:pbyte; count:word);
begin
 if (count <> 4) then
  begin
   log('Erase ERROR');
   RESET();
   task_error := true;
  end
 else
  log('Erase DONE');
 log(' ');

 if firmware_load then
  log('firmware_load(''' + firmware_fname + ''') OK')
 else
  log('ERROR: firmware_load(''' + firmware_fname + '''');
 log(' ');

 progress_max := firmware_size;
 progress_pos := 0;

 firmware_start := info_addr_from;

 erase_done := true;
 send_timeout := GetTickCount;
end;

procedure tSFUboot.recive_write(body:pbyte; count:word);
var
 addr : cardinal;
 free : cardinal;
 rxed : cardinal;
begin
 if count <> 12 then
  begin
   log('ERROR: write command recive count <> 12');
   exit;
  end;

 addr := body_get_cardinal(body, count);
 free := body_get_cardinal(body, count);
 rxed := body_get_cardinal(body, count);

 log('WR: 0x'+inttohex(addr, 8) + #9 + inttostr(free) + #9 + inttostr(rxed) + #9 + inttohex(last_block_addr, 8));

 if last_block_addr <> 0 then
  if addr = last_block_addr then
   begin
    write_done := true;
    progress_pos := progress_max;
    log('Upload Done');
   end;

 if not write_done then
  begin
   progress_pos := addr - firmware_start;

   if write_restart then
    begin
     write_restart := false;
     firmware_addr := addr;
     last_block_addr := 0;

     send_write_multi;
    end
   else
    send_write;
  end;

 send_timeout := GetTickCount + 150;
end;

procedure tSFUboot.recive_error(body:pbyte; count:word; msg:string);
begin
 RESET();
 task_error := true;
 log(msg);
end;

procedure tSFUboot.recive_command(code:byte; body:pbyte; count:word);
begin
 if code = SFU_CMD_INFO then recive_info(body, count) else
 if code = SFU_CMD_ERASE then recive_erase(body, count) else
 if code = SFU_CMD_WRITE then recive_write(body, count) else
 if code = SFU_CMD_TIMEOUT then recive_error(body, count, 'ERROR: command TIMEOUT') else
 if code = SFU_CMD_HWRESET then recive_error(body, count, 'ERROR: command H/W RESET') else
 if code = SFU_CMD_ERROR then recive_error(body, count, 'Flash ERROR command') else
 if code = SFU_CMD_ERASE_PART then recive_erase_part(body, count) else
  log_block_hex('Error unknow command (' + inttohex(code, 2) + ')', body, count);
end;

////////////////////////////////////////////////////////////////////////////////////////////

procedure tSFUboot.send_info;
begin
 log('Send command: INFO');
 CommandSend(SFU_CMD_INFO);
end;

procedure tSFUboot.send_erase(all:boolean = true; size:cardinal=$FFFFFFFF);
var
 info : array[0..3] of byte;
begin
 if all then
  begin
   info[0] := $FF;
   info[1] := $FF;
   info[2] := $FF;
   info[3] := $FF;
  end
 else
  begin
   info[0] := (size shr  0) and $FF;
   info[1] := (size shr  8) and $FF;
   info[2] := (size shr 16) and $FF;
   info[3] := (size shr 24) and $FF;
  end;
 log('Send command: Erase');
 CommandSend(SFU_CMD_ERASE, @info, sizeof(info));
 send_timeout := GetTickCount + 2000;
end;

procedure tSFUboot.send_write;
var
 body : array[0 .. WRITE_BLOCK_SIZE + 4 - 1] of byte;
 count : integer;
 pos : integer;
begin
 body[0] := (firmware_addr shr  0) and $FF;
 body[1] := (firmware_addr shr  8) and $FF;
 body[2] := (firmware_addr shr 16) and $FF;
 body[3] := (firmware_addr shr 24) and $FF;

 pos := firmware_addr - firmware_start;

 if WRITE_BLOCK_SIZE > (firmware_size - pos) then
  count := firmware_size - pos
 else
  count := WRITE_BLOCK_SIZE;

 if count < WRITE_BLOCK_SIZE then
  if last_block_addr = 0 then
   begin
    if count = 0 then
     last_block_addr := firmware_addr
    else
     last_block_addr := firmware_addr + count;
   end;

 if count <= 0 then
  exit;

 //log('Send: 0x' + inttohex(firmware_addr, 8) + ' ' + inttostr(count));
 move(firmware_buf[pos], body[4], count);
 CommandSend(SFU_CMD_WRITE, @body[0], count + 4);
 firmware_addr := firmware_addr + count;
end;

procedure tSFUboot.send_write_multi;
var
 pos : integer;
begin
 for pos := 0 to 15 do
  begin
   if @func_free <> nil then
    if func_free < (WRITE_BLOCK_SIZE*2) then
     break;
   send_write;
  end;
end;

procedure tSFUboot.send_write_restart;
begin
 log('Send command: Write(RESART)');
 write_restart := true;
 CommandSend(SFU_CMD_WRITE);
end;

////////////////////////////////////////////////////////////////////////////////////////////

procedure tSFUboot.RESET();
begin
 task_done := true;
 task_error := false;

 info_done := false;
 erase_done := false;
 write_done := false;

 write_restart := false;

 progress_pos := 0;
 progress_max := 0;

 firmware_size  := 0;
 firmware_addr  := 0;
 firmware_start := 0;

 last_block_addr := 0;

 write_restart := false;

 send_timeout := 0;

 FillChar(firmware_buf[0], sizeof(firmware_buf), $FF);
 fillchar(info_chip_id, sizeof(info_chip_id), 0);

 info_dev_type := 0;
 info_dev_rev  := 0;
 info_flash_size := 0;
 info_boot_ver   := 0;
 info_addr_from  := 0;
 info_addr_run   := 0;
end;

procedure tSFUboot.start;
begin
 send_timeout := GetTickCount;

 RESET();

 task_done := false;
 task_error := false;
end;

procedure tSFUboot.next_send;
begin
 if task_done then exit;
 if GetTickCount < send_timeout then exit;
 send_timeout := GetTickCount + 100;

 if info_done = false then
  send_info
 else
  if erase_done = false then
   send_erase(true)
  else
   if write_done = false then
    send_write_restart;
end;

end.
