unit SFU_boot;

interface
uses
 linkclient;

type
 tSFUboot_CommandSend = procedure (code:byte; cmd_body:pointer = nil; size:word = 0) of object;
 tSFUboot_EventLog=procedure(sender:tobject; msg:string) of object;

 tSFUboot = class
 private
  CommandSend : tSFUboot_CommandSend;

  info_done : boolean;
  erase_done : boolean;
  write_done : boolean;

  error_erase : boolean;

  send_timeout : cardinal;

  info_chip_id : array[0..11] of byte;
  info_dev_type : word;
  info_dev_rev  : word;
  info_flash_size : word;
  info_boot_ver : word;
  info_addr_from : cardinal;
  info_addr_run : cardinal;

  procedure state_reset;

  procedure log(msg:string);
  procedure log_block_hex(msg:string; block:pbyte; count:integer);
  procedure log_block_ascii(msg:string; block:pbyte; count:integer);

  procedure recive_info(body:pbyte; count:word);
  procedure recive_erase(body:pbyte; count:word);
  procedure recive_write(body:pbyte; count:word);

  procedure send_erase(all:boolean = true; size:cardinal=$FFFFFFFF);

 public
  onLog : tSFUboot_EventLog;

  constructor create(send: tSFUboot_CommandSend);
  procedure recive_command(code:byte; body:pbyte; count:word);
  procedure next_send;
 end;

implementation

uses
  SysUtils, windows;

const
 SFU_CMD_INFO  = $97;
 SFU_CMD_ERASE = $C5;
 SFU_CMD_WRITE = $38;


constructor tSFUboot.create(send: tSFUboot_CommandSend);
begin
 CommandSend := send;
 state_reset;
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

procedure tSFUboot.recive_erase(body:pbyte; count:word);
begin
 error_erase := (count <> 4);
 if error_erase then
  log('Erase ERROR')
 else
  log('Erase DONE');
 log(' ');
 erase_done := true;
end;

procedure tSFUboot.recive_write(body:pbyte; count:word);
begin
end;

procedure tSFUboot.recive_command(code:byte; body:pbyte; count:word);
begin
 if code = SFU_CMD_INFO then recive_info(body, count) else
 if code = SFU_CMD_ERASE then recive_erase(body, count) else
 if code = SFU_CMD_WRITE then recive_write(body, count) else
  log_block_hex('Error unknow command (' + inttohex(code, 2) + ')', body, count);
end;

////////////////////////////////////////////////////////////////////////////////////////////

procedure tSFUboot.send_erase(all:boolean = true; size:cardinal=$FFFFFFFF);
var
 info:array[0..3] of byte;
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
 log('Send command Erase');
 CommandSend(SFU_CMD_ERASE, @info, sizeof(info));
 send_timeout := GetTickCount + 30000;
end;

procedure tSFUboot.state_reset;
begin
 info_done := false;
 erase_done := false;
 write_done := false;

 send_timeout := GetTickCount;
end;

procedure tSFUboot.next_send;
begin
 if GetTickCount < send_timeout then exit;
 send_timeout := GetTickCount + 100;

 if info_done = false then
  CommandSend(SFU_CMD_INFO)
 else
  if erase_done = false then
   send_erase(true)
  else
   CommandSend(SFU_CMD_WRITE);
end;

end.
