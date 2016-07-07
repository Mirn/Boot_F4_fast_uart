unit stm32_SFU;

interface
uses
 linkclient;

type
 tSFU_EventWrite=procedure(data:pbyte; size:integer) of object;
 tSFU_EventCommand=procedure(code:byte; body:pbyte; count:integer) of object;
 tSFU_EventInfoString=procedure(sender:tobject; msg:string) of object;
 tSFU_EventLog=procedure(sender:tobject; msg:string) of object;
 tSFU_internalParser = procedure(data:byte) of object;

 tstm32_sfu = class
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

  recive_block_cnt : cardinal;

  onParse : tSFU_internalParser;


  procedure log(msg:string);
  procedure recive_reset;
  function  error_check(error_flag:boolean; msg:string):boolean;
  function  recive_block_add(data:byte; need:cardinal):boolean;

  procedure parse_start(data:byte);
  procedure parse_info(data:byte);
  procedure parse_body(data:byte);
  procedure parse_crc(data:byte);

 public
  send_buf : array[0..4095] of byte;
  send_cnt : integer;

  onWrite : tSFU_EventWrite;
  onCommand : tSFU_EventCommand;
  onInfoString : tSFU_EventInfoString;
  onLog : tSFU_EventLog;

  constructor create(write:tSFU_EventWrite = nil; command:tSFU_EventCommand = nil; infostring:tSFU_EventInfoString = nil; log:tSFU_EventLog = nil);
  procedure send_command(code:byte; cmd_body:pointer = nil; size:word = 0);
  procedure process_recive(sender:tLinkClient; data:pbyte; size:integer);
 end;

implementation
uses
  SysUtils, CRCunit;

Const
 PACKET_SIGN_TX:cardinal = $817EA345;
 PACKET_SIGN_RX:cardinal = $45A37E81;


constructor tstm32_sfu.create(write:tSFU_EventWrite = nil; command:tSFU_EventCommand = nil; infostring:tSFU_EventInfoString = nil; log:tSFU_EventLog = nil);
begin
 onParse := self.parse_start;

 onWrite := write;
 onCommand := command;
 onInfoString := infostring;
 onLog := log;
end;

procedure tstm32_sfu.log(msg:string);
begin
 if @onlog<>nil then
  onLog(self, msg);
end;

procedure tstm32_sfu.send_command(code:byte; cmd_body:pointer = nil; size:word = 0);
var
 crc : cardinal;
 pos : integer;
 body : array of byte absolute cmd_body;
begin
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
end;

////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////

function  tstm32_sfu.recive_block_add(data:byte; need:cardinal):boolean;
begin
 result := false;
 if error_check(recive_cnt >= length(recive_buf), 'recive_cnt >= length(recive_buf)') then
  exit;

 recive_buf[recive_cnt] := data;
 inc(recive_cnt);
 inc(recive_block_cnt);

 result := (recive_block_cnt >= need);
 if result then
  recive_block_cnt := 0;
end;

procedure tstm32_sfu.recive_reset;
begin
 onParse := self.parse_start;
 recive_block_cnt := 0;
 recive_cnt := 0;
end;

function  tstm32_sfu.error_check(error_flag:boolean; msg:string):boolean;
begin
 result := error_flag;
 if not error_flag then exit;
 log('ERROR: ' + msg);
 recive_reset;
end;
////////////////////////////////////////////////////////////////////////////////////

procedure tstm32_sfu.parse_start(data:byte);
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
   onParse := self.parse_info;
   recive_block_cnt := 0;
   recive_cnt := 0;
  end
 else
  if data = 13 then
   begin
    if @onInfoString <> nil then
     onInfoString(self, info_string);
    info_string := '';
   end;
end;

procedure tstm32_sfu.parse_info(data:byte);
begin
 if not recive_block_add(data, 4) then exit;

 recive_code   := recive_buf[0] xor $00;
 recive_code_n := recive_buf[1] xor $FF;
 if error_check(recive_code <> recive_code_n, 'recive_code <> recive_code_n') then exit;

 recive_size := (word(recive_buf[2]) shl 0) or
                (word(recive_buf[3]) shl 8);
 if error_check(recive_size >= (Length(recive_buf) - 8), 'recive_size >= (Length() - 8)') then exit;

 recive_body := @(recive_buf[4]);
 onParse := parse_body;
end;

procedure tstm32_sfu.parse_body(data:byte);
begin
 if not recive_block_add(data, recive_size) then exit;
 onParse := parse_crc;
end;

procedure tstm32_sfu.parse_crc(data:byte);
var
 need_crc : cardinal;
begin
 if not recive_block_add(data, 4) then exit;

 need_crc := crc_stm32(pcardinal(@recive_buf[0]), (recive_cnt - 4) div 4);

 recive_crc := (cardinal(recive_buf[recive_cnt - 4]) shl  0) or
               (cardinal(recive_buf[recive_cnt - 3]) shl  8) or
               (cardinal(recive_buf[recive_cnt - 2]) shl 16) or
               (cardinal(recive_buf[recive_cnt - 1]) shl 24);

 if error_check(need_crc <> recive_crc, 'need_crc <> recive_crc') then exit;

 if @onCommand <> nil then
  onCommand(recive_code, recive_body, recive_size);

 recive_reset;
end;

procedure tstm32_sfu.process_recive(sender:tLinkClient; data:pbyte; size:integer);
begin
 while size > 0 do
  begin
   onParse(data^);
   inc(data);
   dec(size);
  end;
end;


end.
