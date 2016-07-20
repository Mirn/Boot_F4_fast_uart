{$I-}
unit SFU_cmd;

interface
uses
 linkclient;

type
 tSFUcmd_EventWrite=procedure(data:pbyte; size:integer) of object;
 tSFUcmd_EventCommand=procedure(code:byte; body:pbyte; count:word) of object;
 tSFUcmd_EventInfoString=procedure(sender:tobject; msg:string) of object;
 tSFUcmd_EventLog=procedure(sender:tobject; msg:string) of object;
 tSFUcmd_internalParser = procedure(data:byte) of object;

 tSFUcmd = class
 private
  info_string : string;

  recive_seq : cardinal;

  recive_buf : array[0..4095] of byte;
  recive_cnt : cardinal;
  recive_code : byte;
  recive_code_n : byte;
  recive_size : word;
  recive_body : pbyte;
  recive_crc : cardinal;

  recive_timelimit : cardinal;
  recive_block_cnt : cardinal;

  onParse : tSFUcmd_internalParser;

  raw_log : file;

  function  error_check(error_flag:boolean; msg:string; var stat:cardinal):boolean;
  function  recive_block_add(data:byte; need:cardinal):boolean;

  procedure parse_start(data:byte);
  procedure parse_info(data:byte);
  procedure parse_body(data:byte);
  procedure parse_crc(data:byte);

 protected
  procedure log(msg:string);

 public
  stat_send : cardinal;
  stat_normals : cardinal;
  stat_errors : cardinal;
  stat_error_timeout : cardinal;
  stat_error_overfull : cardinal;
  stat_error_start : cardinal;
  stat_error_code : cardinal;
  stat_error_size : cardinal;
  stat_error_crc : cardinal;

  send_buf : array[0..4095] of byte;
  send_cnt : integer;

  onWrite : tSFUcmd_EventWrite;
  onCommand : tSFUcmd_EventCommand;
  onInfoString : tSFUcmd_EventInfoString;
  onLog : tSFUcmd_EventLog;

  constructor create(write:tSFUcmd_EventWrite = nil; command:tSFUcmd_EventCommand = nil; infostring:tSFUcmd_EventInfoString = nil; log:tSFUcmd_EventLog = nil);

  function  send_command(code:byte; cmd_body:pointer = nil; size:word = 0):cardinal;
  procedure process_recive(sender:tLinkClient; data:pbyte; size:integer);

  procedure recive_reset;
 end;

implementation
uses
  SysUtils, Windows, CRCunit;

Const
 PACKET_SIGN_TX:cardinal = $817EA345;
 PACKET_SIGN_RX:cardinal = $45A37E81;
 RECIVE_TIMEOUT_mS = 200;


constructor tSFUcmd.create(write:tSFUcmd_EventWrite = nil; command:tSFUcmd_EventCommand = nil; infostring:tSFUcmd_EventInfoString = nil; log:tSFUcmd_EventLog = nil);
begin
 onParse := self.parse_start;

 onWrite := write;
 onCommand := command;
 onInfoString := infostring;
 onLog := log;

 AssignFile(raw_log, 'raw_log.dat');
 rewrite(raw_log, 1);
end;

procedure tSFUcmd.log(msg:string);
begin
 if @onlog<>nil then
  onLog(self, msg);
end;

////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////

function tSFUcmd.send_command(code:byte; cmd_body:pointer = nil; size:word = 0):cardinal;
var
 crc : cardinal;
 pos : integer;
 body : array of byte absolute cmd_body;
begin
 if (size mod 4) <> 0 then
  begin
   log('tSFUcmd.send_command ERROR: size mod 4 <> 0');
   result := 0;
   exit;
  end;
 send_buf[0] := (PACKET_SIGN_TX shr 24) and $FF;
 send_buf[1] := (PACKET_SIGN_TX shr 16) and $FF;
 send_buf[2] := (PACKET_SIGN_TX shr  8) and $FF;
 send_buf[3] := (PACKET_SIGN_TX shr  0) and $FF;

 send_buf[4] := code xor $00;
 send_buf[5] := code xor $FF;

 if cmd_body = nil then
  size := 0;

 send_buf[6] := (size shr (8*0)) and 255;
 send_buf[7] := (size shr (8*1)) and 255;

 for pos := 0 to size - 1 do
  send_buf[8 + pos] := body[pos];

 crc := crc_stm32(@send_buf[4], (size + 4) div 4); //+4: code + ncode + size

 send_buf[8 + size + 0] := (crc shr (8*0)) and 255;
 send_buf[8 + size + 1] := (crc shr (8*1)) and 255;
 send_buf[8 + size + 2] := (crc shr (8*2)) and 255;
 send_buf[8 + size + 3] := (crc shr (8*3)) and 255;

 send_cnt := 8 + size + 4;
 if @onWrite <> nil then
  onWrite(@send_buf[0], send_cnt);
 inc(stat_send);

 result := send_cnt;
end;

////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////

function  tSFUcmd.recive_block_add(data:byte; need:cardinal):boolean;
begin
 result := false;
 if error_check(recive_cnt >= length(recive_buf), 'recive_cnt >= length(recive_buf)', stat_error_overfull) then
  exit;

 recive_buf[recive_cnt] := data;
 inc(recive_cnt);
 inc(recive_block_cnt);

 result := (recive_block_cnt >= need);
 if result then
  recive_block_cnt := 0;
end;

procedure tSFUcmd.recive_reset;
begin
 onParse := self.parse_start;
 recive_block_cnt := 0;
 recive_cnt := 0;
end;

function  tSFUcmd.error_check(error_flag:boolean; msg:string; var stat:cardinal):boolean;
begin
 result := error_flag;
 if not error_flag then exit;
 log('SFUcmd ERROR: ' + msg);
 recive_reset;
 inc(stat_errors);
 inc(stat);
end;

////////////////////////////////////////////////////////////////////////////////////

procedure tSFUcmd.parse_start(data:byte);
var
 pos : integer;
begin
 if data in [9, 32..255] then
  info_string := info_string + ansichar(data);
// else
//  if not (data in [13, 10]) then
//   info_string := info_string + '<' + inttohex(data, 2) + '>';

 recive_seq := (recive_seq shl 8) or data;
 if recive_seq = PACKET_SIGN_RX then
  begin
   delete(info_string, length(info_string) - 3, 4);
   dec(stat_error_start, 3);

   recive_timelimit := gettickcount + RECIVE_TIMEOUT_mS;
   onParse := parse_info;

   recive_block_cnt := 0;
   recive_cnt := 0;
  end
 else
  begin
   inc(stat_error_start);
   if data = 13 then
    begin
     if @onInfoString <> nil then
      onInfoString(self, info_string);
     info_string := '';
    end;
  end;
end;

procedure tSFUcmd.parse_info(data:byte);
begin
 if not recive_block_add(data, 4) then exit;

 recive_code   := recive_buf[0] xor $00;
 recive_code_n := recive_buf[1] xor $FF;
 if error_check(recive_code <> recive_code_n, 'recive_code <> recive_code_n', stat_error_code) then
  exit;

 recive_size := (word(recive_buf[2]) shl 0) or
                (word(recive_buf[3]) shl 8);
 if error_check(recive_size >= (Length(recive_buf) - 8), 'recive_size >= (Length() - 8)', stat_error_size) then
  exit;

 recive_body := @(recive_buf[4]);

 if recive_size = 0 then
  onParse := parse_crc
 else
  onParse := parse_body;
end;

procedure tSFUcmd.parse_body(data:byte);
begin
 if not recive_block_add(data, recive_size) then
  exit;
 onParse := parse_crc;
end;

procedure tSFUcmd.parse_crc(data:byte);
var
 need_crc : cardinal;
begin
 if not recive_block_add(data, 4) then exit;

 need_crc := crc_stm32(pcardinal(@recive_buf[0]), (recive_cnt - 4) div 4);

 recive_crc := (cardinal(recive_buf[recive_cnt - 4]) shl  0) or
               (cardinal(recive_buf[recive_cnt - 3]) shl  8) or
               (cardinal(recive_buf[recive_cnt - 2]) shl 16) or
               (cardinal(recive_buf[recive_cnt - 1]) shl 24);

 if error_check(need_crc <> recive_crc, 'need_crc <> recive_crc', stat_error_crc) then
  exit;
 inc(stat_normals);

 if @onCommand <> nil then
  onCommand(recive_code, recive_body, recive_size);

 recive_reset;
end;

///////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////

procedure tSFUcmd.process_recive(sender:tLinkClient; data:pbyte; size:integer);
var
 p : pointer;
begin
 BlockWrite(raw_log, data^, size);

 if (TMethod(onParse).Code <> @tSFUcmd.parse_start) then
  error_check(GetTickCount > recive_timelimit, 'tSFUcmd:RECIVE_TIMEOUT', stat_error_timeout);

 if data <> nil then
  while size > 0 do
   begin
    onParse(data^);
    inc(data);
    dec(size);
   end;
end;


end.
