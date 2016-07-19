unit COMclient;

interface
uses winsock, sysutils, Windows, Registry, classes, math,
//     D2XXUnit,
     LinkClient,
     u_stm32_id,
     u_millesecond_timer,
     uMAP_debug_log,
     CP2102_classes,
     CP210xRuntimeDLL;

const
 tboot_ack   = $79;
 tboot_error = $1f;
 tiny_cmd_boot  = $35;
 tiny_cmd_reset = $5A;

type
 tgate_lib_set_serial   = function(
        firmware_ptr  : pointer;
        firmware_size : cardinal;
        serial_num      : pchar;
        serial_num_size : cardinal;
        id_type         : cardinal
        ):word; stdcall;
const
 GATE_LIB_CODE_NO_SPGATE_ID     = 1;
 GATE_LIB_CODE_CRC_ERROR        = 2;
 GATE_LIB_CODE_SERIAL_INCORRECT = 3;
 GATE_LIB_CODE_NEW_SERIAL_OK    = 4;

 GATE_LIB_CODE_ID_GATE = 1;

type
 tboot_commands_list=record
  ack      : byte;
  size     : byte;
  Version  : byte;
  GetCmd   : byte;
  VerProt  : byte;
  ID       : byte;
  Read     : byte;
  GO       : byte;
  Write    : byte;
  Erase    : byte;
  WrLock   : byte;
  WrUnLock : byte;
  RdLock   : byte;
  RdUnLock : byte;
  ack_end  : byte;
 end;

 tCOMClient=class;

 tCOMClient_mode = (mode_armka, mode_atm, mode_gate);
 tCOMClient_evWRLoad = procedure(sender:tCOMClient; name:string; data:pointer; size:integer) of object;

 tCOMClient=class(tLinkClient)
 private
  alt_log : tstringlist;
  alt_log_enabled : boolean;
  readed_buf:array of byte;
  boot_commands_list : tboot_commands_list;

  stm32_info         : tSTM32_info;
  stm32_boot_version : byte;
  stm32_options      : word;
  stm32_PID          : word;

  stm32_flash_loaded : cardinal;
  stm32_flash_size   : cardinal;
  stm32_flash_data   : array of byte;
  stm32_flash_readed : boolean;
  stm32_flash_writed : boolean;

  waiting_time_ms : integer;
  tiny_state:boolean;

  active_mode : tCOMClient_mode;

  stat_timer:tmillisecond_timer;

  gate_lib_set_serial_func : tgate_lib_set_serial;

  sp_gatem_only_reset : boolean;
  real_port_serial : array[0..4096] of AnsiChar;

  stm32_erase_ignore : boolean;

  function  modem_status:dword;

  procedure write_break(v:boolean);
  procedure write_dtr(v:boolean);
  procedure write_rts(v:boolean);

  function  read_rng:boolean;
  function  read_dcd:boolean;
  function  read_cts:boolean;
  function  read_dsr:boolean;

  function  RNG_str:string;
  function  DCD_str:string;
  function  CTS_str:string;
  function  DSR_str:string;

  function  com_open:boolean;
  function  com_wait_for(name:string; mask, value:integer; timeout:cardinal):boolean;
  function  com_write_str(s:string):boolean;
  function  com_read_data(data:pointer; data_size:cardinal; data_readed:pcardinal; timeout:integer):boolean;
  function  com_io_data(write_str:string; data:pointer; read_size:cardinal; data_readed:pcardinal; timeout:integer):boolean;

  function addr2str(addr:cardinal):string;
  function byte2str(b:byte):string;
  function word2str(w:word):string;
  function data2str(data:pointer; size:integer):string;
  function cmd2str(cmd:byte):string;
  function xor2str(s:string):string;

  function boot_activate:boolean;
  function boot_get_commands_list:boolean;
  function boot_get_verison:boolean;
  function boot_get_id:boolean;
  function boot_read_lock:boolean;
  function boot_read_unlock:boolean;
  function boot_write(addr:Cardinal; data:pointer;  size:word):boolean;
  function boot_read(addr:Cardinal;  data:pointer;  size:word):boolean;
  function boot_read_info:boolean;
  function boot_read_flash:boolean;
  function boot_verify_flash:boolean;
  function boot_write_flash:boolean;
  function boot_erase_all:boolean;
  function boot_erase_sector(sector_num:word):boolean;
  function boot_go(addr:cardinal):boolean;

  function activate_atm:boolean;
  function activate_gate:boolean;
  function activate_armka:boolean;

  function boot_init:boolean;
  function boot_main_run:boolean;

  procedure tiny_tick;
  procedure tiny_send(v:integer; msg:string; try_n:integer);
  function com_signals_to_version(norm,dtr,rts:boolean; power:integer):integer;
  function  gate_lib_load:boolean;

  procedure alt_log_start;
  procedure alt_log_stop(print:boolean);

  function check_stop : boolean;

  procedure set_serial_name(name : string);

  function cw_upload:boolean;

 protected
  handle     : thandle;
  com_number : cardinal;
  error_code : integer;

  procedure Log_add(msg:string);override;

  function  hardware_open:boolean;override;
  function  hardware_close:boolean;override;
  function  hardware_check(var read_ready:boolean; var write_ready:boolean):boolean;override;
  function  hardware_write(buf:pointer; size:integer; var writed:integer):boolean;override;
  function  hardware_read(buf:pointer; size:integer; var readed:integer):boolean;override;
  function  hardware_errorcode:integer;override;

 public
  port_name   : string;
  port_name_serial : boolean;
  port_speed  : cardinal;
  port_parity : integer;

  invert_rts : boolean;
  invert_dtr : boolean;

  stm32_info_progress_total   : integer;
  stm32_info_progress_current : integer;
  stm32_info_string : string[255];
  stm32_info_result : string[255];

  stm32_task_filename : string;
  stm32_task_read   : boolean;
  stm32_task_erase  : boolean;
  stm32_task_write  : boolean;
  stm32_task_verify : boolean;
  stm32_task_enable : boolean;
  stm32_task_RDlock : boolean;
  stm32_task_UNlock : boolean;
  stm32_task_stop : boolean;

  stm32_find_disable_atm    : boolean;
  stm32_find_disable_armka  : boolean;
  stm32_find_disable_spgate : boolean;

  stm32_chip_id : tSTM32_info;
  stm32_chip_id_readed : boolean;

  task_open_with_reset : boolean;
  no_activate : boolean;

  evWR_loaded : tCOMClient_evWRLoad;

  gate_lib_serial : string;

  current_speed : integer;
  cw_mini_detect_mode : boolean;
  cw_only_reset : boolean;
  cw_m5_bootloader : boolean;

  cw_file : string;

  cw_retry : integer;
  cw_ok : boolean;
  cw_error : boolean;
  cw_progress : integer;
  cw_finished : boolean;
  cw_no_founded : boolean;

  open_simple_fast : boolean;
  procedure COM_init_as(speed:integer; parity:byte);

  property DTR:boolean write write_dtr;
  property RTS:boolean write write_rts;

  property RNG:boolean read  read_rng;
  property DCD:boolean read  read_dcd;
  property CTS:boolean read  read_cts;
  property DSR:boolean read  read_dsr;

  property serial_name:string write set_serial_name;
 end;

function stm32_save_file(file_name:string; data:pointer; data_size:cardinal) : boolean;
function stm32_load_file(file_name:string; data:pointer; max_size:cardinal; readed:pcardinal) : boolean;

implementation

const
 stm32_PID_Low_density           = $0412;
 stm32_PID_Medium_density        = $0410;
 stm32_PID_High_density          = $0414;
 stm32_PID_Connectivity_line     = $0418;
 stm32_PID_Medium_density_vline  = $0420;
 stm32_PID_High_density_vline    = $0428;
 stm32_PID_XL_density            = $0430;
 stm32_PID_ultralow_power_line   = $0416;
 stm32_PID_STM32F2xx             = $0411;
 stm32_PID_STM32F4xx             = $0413;

function stm32_save_file(file_name:string; data:pointer; data_size:cardinal) : boolean;
var
 f:file;
 writed:cardinal;
 old_mode : integer;
begin
 result := false;
 if data_size>100000000 then exit;

 GetLastError;
 old_mode := filemode;
 filemode := fmOpenReadWrite;
 fillchar(f, sizeof(f), 0);
 {$I-}
 ioresult;
 AssignFile(f, file_name);
 rewrite(f,1);
 if IOResult<>0 then
  begin
   filemode := old_mode;
   exit;
  end;
 filemode := old_mode;

 BlockWrite(f, data^, data_size, writed);
 if IOResult<>0 then exit;
 if writed <> data_size then
  begin
   CloseFile(f);
   exit;
  end;

 CloseFile(f); ioresult;
 {$I+}

 result:=true;
end;

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


const
 MS_CTS_GND  = $0010; // The CTS (clear-to-send) signal is on
 MS_DSR_GND  = $0020; // The DSR (data-set-ready) signal is on
 MS_RNG_GND  = $0040; // The ring indicator signal is on
 MS_DCD_GND  = $0080; // The RLSD (receive-line-signal-detect) signal is on

 MS_CTS_VCC = 0;
 MS_DSR_VCC = 0;
 MS_RNG_VCC = 0;
 MS_DCD_VCC = 0;

 MS_CTS_MASK = MS_CTS_GND;
 MS_DSR_MASK = MS_DSR_GND;
 MS_RNG_MASK = MS_RNG_GND;
 MS_DCD_MASK = MS_DCD_GND;

 DTR_SET = true;
 RTS_SET = true;
 BREAK_SET = true;

 DTR_Clear = false;
 RTS_Clear = false;
 BREAK_Clear = false;

 CTS_VCC = true;
 DSR_VCC = true;
 RNG_VCC = true;
 DCD_VCC = true;

 CTS_GND = not CTS_VCC;
 DSR_GND = not DSR_VCC;
 RNG_GND = not RNG_VCC;
 DCD_GND = not DCD_VCC;

function inttobin(i:dword; cnt:integer):string;
var
 k,m:integer;
begin
 result:='';
 if cnt<=0 then exit;
 m:=1 shl (cnt-1);
 for k:=0 to cnt-1 do
  begin
   if (i and m)<>0 then
    result:=result+'1'
   else
    result:=result+'0';
   m:=m shr 1;
  end;
end;

function tCOMClient.com_signals_to_version(norm,dtr,rts:boolean; power:integer):integer;
begin
 result:=0;
 if power>=1 then
  power:=5;

 if (norm=dtr) and (dtr=rts) then
  begin
   if (norm=true) then result:=1*power;
   exit;
  end;

 if (norm=dtr) then
  begin
   result:=2*power;
   exit;
  end;

 if (norm=rts) then
  begin
   result:=3*power;
   exit;
  end;

 result:=4*power;
end;

function tCOMClient.com_open:boolean;
{
 https://www.silabs.com/pages/DownloadDoc.aspx?FILEURL=Support%20Documents/TechnicalDocs/an197.pdf&src=DocumentationWebPart
 AN197 SERIAL COMMUNICATIONS GUIDE FOR THE CP210X

 7. Discovering CP210x COM Port
To use the described functionality with a COM port, the number of the COM port needs to be known. In order to find
out the COM port number of a CP210x device, the VID, PID, and serial number are used to lookup a registry key.
This key is different between Windows XP/2000/Server 2003/Vista and Windows 98. Here are the keys that will
need to be looked up:
WinXP/2000/Server 2003/Vista/7 (Driver Version 5.0 and higher):
HKLM\System\CurrentControlSet\Enum\USB\Vid_xxxx&Pid_yyyy\zzzz\Device Parameters\PortName
 where for CP2102 string Vid_xxxx&Pid_yyyy is VID_10C4&PID_EA60
}
function get_si_coms(numbers:pinteger; var counts:integer):string;
var
 reg:TRegistry;
 list:tstrings;
 k:integer;
 port_name:string;
 port_path:string;
 s:string;

const
 path = 'SYSTEM\\CurrentControlSet\\Enum\\USB\\VID_10C4&PID_EA60';
begin
 counts:=0;
 result:='';

 reg:=TRegistry.Create;
 reg.RootKey := HKEY_LOCAL_MACHINE;
 reg.Access := KEY_READ;
 s:=path;
 if not reg.OpenKey(s, false) then exit;

 list:=TStringlist.Create;
 reg.GetKeyNames(list);
 for k:=0 to list.Count-1 do
  begin
   reg.CloseKey;
   if not reg.OpenKey(path+'\\'+list.Strings[k]+'\\'+'Device Parameters', false) then Continue;
   if not reg.ValueExists('PortName') then Continue;
   if not reg.ValueExists('SymbolicName') then Continue;
   port_name:=reg.ReadString('PortName');
   port_path:=reg.ReadString('SymbolicName');
   log_add('SN# '+list.Strings[k]+#9+port_name+#9+port_path);

   numbers^:=strtoint(copy(port_name,4,10));
   inc(numbers);
   inc(counts);
   if result<>'' then
    result:=result+#13;
   result:=result+port_path;
  end;

 reg.Free;
 list.Free;
end;

function open_first_si_com(var handle:thandle):boolean;
var
 numbers:array[0..255] of integer;
 count:integer;
 names:string;
 s:string;
 k:integer;
 p:integer;

begin
 handle := INVALID_HANDLE_VALUE;
 result:=true;
 count:=0;

 if port_name = '' then
  begin
   names:=get_si_coms(@numbers[0],count);
   if count<=0 then exit;
   result:=false;

   while length(names)>1 do
    begin
     p:=pos(#13, names);
     if p=0 then
      begin
       s:=names;
       names:='';
      end
     else
      begin
       s:=copy(names, 1, p-1);
       delete(names, 1, p);
      end;

     log_add('Open by "'+s+'"');
      handle:=CreateFile(
   //   pchar('\\.\COM'+inttostr(com_number)),
      pchar(s),
      GENERIC_READ or GENERIC_WRITE,
      0,
      nil,
      OPEN_EXISTING,
      FILE_ATTRIBUTE_NORMAL,
      0
     );
     if handle<>INVALID_HANDLE_VALUE then exit;

     if copy(s,1,4)='\??\' then
      begin
       s:='\\.\'+copy(s,5,40000);
//       s:=copy(s,5,40000);

       log_add('Open by "'+s+'"');
        handle:=CreateFile(
     //   pchar('\\.\COM'+inttostr(com_number)),
        pchar(s),
        GENERIC_READ or GENERIC_WRITE,
        0,
        nil,
        OPEN_EXISTING,
        FILE_ATTRIBUTE_NORMAL,
        0
       );
       if handle<>INVALID_HANDLE_VALUE then exit;
      end;
    end;

   for k:=0 to count-1 do
    begin
     log_add('Open by "\\.\COM'+inttostr(numbers[k])+'"');
      handle:=CreateFile(
      pchar('\\.\COM'+inttostr(numbers[k])),
      GENERIC_READ or GENERIC_WRITE,
      0,
      nil,
      OPEN_EXISTING,
      FILE_ATTRIBUTE_NORMAL,
      0
     );
     if handle<>INVALID_HANDLE_VALUE then exit;
    end;
  end
 else
  begin
   for k:=0 to 4 do
    begin
     log_add('Open by "'+port_name+'"');
      handle:=CreateFile(
      pchar(port_name),
      GENERIC_READ or GENERIC_WRITE,
      0,
      nil,
      OPEN_EXISTING,
      FILE_ATTRIBUTE_NORMAL,
      0
     );
     if handle<>INVALID_HANDLE_VALUE then exit;

     s := port_name;
     if copy(s,1,3)='COM' then
      begin
       s:='\\.\'+s;

       log_add('Open by "'+s+'"');
        handle:=CreateFile(
        pchar(s),
        GENERIC_READ or GENERIC_WRITE,
        0,
        nil,
        OPEN_EXISTING,
        FILE_ATTRIBUTE_NORMAL,
        0
       );
       if handle<>INVALID_HANDLE_VALUE then exit;
      end;

     if copy(s,1,4)='\??\' then
      begin
       s:='\\.\'+copy(s,5,40000);
//       s:=copy(s,5,40000);

       log_add('Open by "'+s+'"');
        handle:=CreateFile(
     //   pchar('\\.\COM'+inttostr(com_number)),
        pchar(s),
        GENERIC_READ or GENERIC_WRITE,
        0,
        nil,
        OPEN_EXISTING,
        FILE_ATTRIBUTE_NORMAL,
        0
       );
       if handle<>INVALID_HANDLE_VALUE then exit;
      end;

     Log_add('Open error : '+SysErrorMessage(GetLastError));
     sleep(128);
     if k<>4 then
      begin
       Log_add(' ');
       Log_add('sleep(128ms);');
       Log_add('Try #'+inttostr(k+2));
      end;
    end;
  end;

 result:=true;
end;

begin
 result := true;
 handle := INVALID_HANDLE_VALUE;
 open_first_si_com(handle);

 if (handle=INVALID_HANDLE_VALUE) then
  begin
   Log_add('Open error : '+SysErrorMessage(GetLastError));
   result := true;
   exit;
  end;
 Log_add('Open ok');

 result := false;
end;

function tCOMClient.activate_atm;
var
 cts_norm : boolean;
 dsr_norm : boolean;
 cts_dtr : boolean;
 dsr_dtr : boolean;
 cts_rts : boolean;
 dsr_rts : boolean;
 board_version : byte;
 k:integer;
begin
 result:=false;
 if stm32_find_disable_atm then exit;
 stm32_info_string := 'Activate ATM';

 COM_init_as(port_speed, EVENPARITY);

{ for k:=0 to 10000 do
  begin
   CP210xRT_WriteLatch(handle, 15, random(16));
   sleep(1);
  end;}

 for k:=0 to 3 do
  begin
   Log_add(' ');
   Log_add('After init status        : '+inttobin(modem_status,8));

   Log_add(' ');
   DTR := DTR_Clear;
   RTS := RTS_SET;
   sleep(50);
   cts_rts := CTS;
   dsr_rts := DSR;
   Log_add('DTR = Clear, RTS = Set   : '+inttobin(modem_status,8));

   DTR := DTR_Clear;
   RTS := RTS_Clear;
   sleep(50);
   cts_norm := CTS;
   dsr_norm := DSR;
   Log_add('DTR = Clear, RTS = Clear : '+inttobin(modem_status,8));
   Log_add(' ');

   DTR := DTR_SET;
  // sleep(1000);
   Log_add('DTR := DTR_SET;');
   if com_wait_for('Status '+inttobin(modem_status,8)+', '+'MS_DCD_GND', MS_DCD_MASK, MS_DCD_GND, 100) then
    begin
     write_break(BREAK_SET);
     com_wait_for('Status '+inttobin(modem_status,8)+', '+'MS_RNG_GND', MS_RNG_MASK, MS_RNG_GND, 1000);
     Log_add(' ');

     waiting_time_ms := waiting_time_ms*2;
     if waiting_time_ms>1000 then waiting_time_ms:=1000;
     if waiting_time_ms<250  then waiting_time_ms:=250;
     Log_add('Wait '+inttostr(waiting_time_ms)+' ms');
     sleep(waiting_time_ms);

     cts_dtr := CTS;
     dsr_dtr := DSR;
     write_break(BREAK_CLEAR);
     sleep(10);
     DTR := DTR_Clear;

     Log_add(' ');
     Log_add('DTR := DTR_Clear;');

     com_wait_for('Status '+inttobin(modem_status,8)+', '+'MS_DCD_VCC', MS_DCD_MASK, MS_DCD_VCC, 100);
     com_wait_for('Status '+inttobin(modem_status,8)+', '+'MS_RNG_VCC', MS_RNG_MASK, MS_RNG_VCC, 1000);
    end
   else
    begin
     cts_dtr := CTS;
     dsr_dtr := DSR;

     if cw_mini_detect_mode then
      begin
       if cw_m5_bootloader and cw_only_reset and (system.pos('cheatwatch_ru_5m', real_port_serial) <> 0) then
        begin
         //SetCommBreak(handle); ////BREAK FOR BOOTLOADER PROGRAMMING
         DTR := DTR_SET;
         RTS := RTS_SET;
         sleep(100);
         RTS := RTS_Clear;
         sleep(16);
         //ClearCommBreak(handle);
         Log_add('CW_5m in BOOTLOADER mode started ...');
        end
       else
        begin
         DTR := DTR_SET;   Log_add('DTR := DTR_SET;');
         RTS := RTS_Clear; Log_add('RTS := RTS_Clear;');

         sleep(100);
         RTS := RTS_SET;   Log_add('RTS := RTS_SET;');
         sleep(100);

         DTR := DTR_Clear xor cw_only_reset; Log_add('DTR := DTR_Clear xor cw_only_reset;');
         sleep(32);
         RTS := RTS_Clear; Log_add('RTS := RTS_Clear;');
         sleep(32);
         DTR := DTR_SET;
        end;
      end
     else
      begin
       RTS := RTS_Clear; Log_add('RTS := RTS_Clear;');
       DTR := DTR_Clear; Log_add('RTS := RTS_Clear;');
      end;

     result := (not (stm32_find_disable_atm)) and (stm32_find_disable_armka and stm32_find_disable_spgate);
     result := result and cw_mini_detect_mode;
     if not result then
      begin
       stm32_info_string := 'ERROR: ATM not found';
       exit;
      end;
    end;

   Log_add(' ');
   board_version := com_signals_to_version(dsr_norm, dsr_dtr, dsr_rts, 1)+
                    com_signals_to_version(cts_norm, cts_dtr, cts_rts, 0);
   Log_add('board_version := '+inttostr(board_version));
   Log_add(' ');

   PurgeComm(handle, PURGE_TXABORT);
   PurgeComm(handle, PURGE_RXABORT);
   PurgeComm(handle, PURGE_TXCLEAR);
   PurgeComm(handle, PURGE_RXCLEAR);
   sleep(100);
   PurgeComm(handle, PURGE_TXABORT);
   PurgeComm(handle, PURGE_RXABORT);
   PurgeComm(handle, PURGE_TXCLEAR);
   PurgeComm(handle, PURGE_RXCLEAR);

   if boot_activate then
    begin
     active_mode := mode_atm;

     if boot_init then continue;
     if boot_main_run then
      begin
       result := false;
       exit;
      end;
     break;
    end
   else
    continue;
  end;

 DTR := dtr_clear;
 RTS := rts_clear;
 COM_init_as(500000, NoParity);


 DTR := dtr_clear;
 RTS := rts_clear;

 result := true;
end;

////////////////////////////////////////////////////////////////////////////////////////

function tCOMClient.gate_lib_load:boolean;
var
 hlib:HMODULE;
begin
 result := true;
 if @gate_lib_set_serial_func <> nil then exit;
 result := false;

 hlib := LoadLibrary('Firmware_update_DLL.dll');
 if hlib = 0 then
  Log_add('Firmware_update_DLL.dll not found')
 else
  begin
   gate_lib_set_serial_func := GetProcAddress(hLib, '_set_new_serial@20');
   if @gate_lib_set_serial_func=nil then
    Log_add('Firmware_update_DLL.dll : _set_new_serial@20 not found')
   else
    result := true;
  end;
end;

procedure tCOMClient.tiny_tick;
begin
 if tiny_state then
   DTR:=DTR_SET
 else
   DTR:=DTR_CLEAR;

 tiny_state := not tiny_state;
end;

procedure tCOMClient.tiny_send(v:integer; msg:string; try_n:integer);
var
 k:integer;
 part_number : byte;
 error_code : integer;
 invert : byte;
begin
 part_number := $FF;

 if not gate_lib_load then
  log_add('Can''t load gate serial lib');

 error_code := CP210xRT_GetPartNumber(self.handle, @part_number);
 if error_code = CP210x_SUCCESS then
  Log_add('CP210'+chr(ord('0')+part_number)+' chip detected')
 else
  Log_add('It''s not a CP210x chip');

 invert := 0;
 if (error_code = CP210x_SUCCESS) and (part_number = CP210x_CP2103_VERSION) then
  begin
{   if (try_n and 3) = 0 then invert := $00;
   if (try_n and 3) = 1 then invert := $05;
   if (try_n and 3) = 2 then invert := $0A;
   if (try_n and 3) = 3 then invert := $0F;}
   
{   CP210xRT_WriteLatch(self.handle, $0F, $0F xor $0F); sleep(64);
   CP210xRT_WriteLatch(self.handle, $0F, $0A xor $0F); sleep(256);
   CP210xRT_WriteLatch(self.handle, $0F, $0E xor $0F); sleep(64);
   CP210xRT_WriteLatch(self.handle, $0F, $0F xor $0F); sleep(64);}

   if sp_gatem_only_reset then
    begin
     CP210xRT_WriteLatch(self.handle, $03, $00 xor invert); sleep(16);
     CP210xRT_WriteLatch(self.handle, $03, $02 xor invert); sleep(16);
     CP210xRT_WriteLatch(self.handle, $03, $00 xor invert); sleep(600);
    end
   else
    begin
     CP210xRT_WriteLatch(self.handle, $03, $00 xor invert); sleep(16);
     CP210xRT_WriteLatch(self.handle, $03, $03 xor invert); sleep(16);
     CP210xRT_WriteLatch(self.handle, $03, $01 xor invert); sleep(64);
     CP210xRT_WriteLatch(self.handle, $03, $00 xor invert); sleep(128);
    end;
   exit;
  end;

 Log_add('TINY_Send: 0x'+inttohex(v,2)+' (CMD = "'+msg+'")');

 tiny_tick;

 for k:=0 to 7 do
  begin
   if check_stop then exit;

   if (v and (1 shl k)) <> 0 then
    sleep(64)
   else
    sleep(16);

   tiny_tick;
  end;

 k := GetTickCount;
 while GetTickCount-k<300 do
  begin
   if check_stop then exit;
   sleep(1);
  end;

 PurgeComm(handle, PURGE_TXABORT);
 PurgeComm(handle, PURGE_RXABORT);
 PurgeComm(handle, PURGE_TXCLEAR);
 PurgeComm(handle, PURGE_RXCLEAR);
 sleep(16);
 PurgeComm(handle, PURGE_TXABORT);
 PurgeComm(handle, PURGE_RXABORT);
 PurgeComm(handle, PURGE_TXCLEAR);
 PurgeComm(handle, PURGE_RXCLEAR);
end;

function tCOMClient.activate_armka:boolean;
var
 k:integer;
 flg : boolean;
 part_number : byte;
 t : cardinal;
begin
 result:=false;
 if stm32_find_disable_armka then exit;

 stm32_info_string := 'Activate ARMka';
 stm32_info_progress_total   := 24;
 stm32_info_progress_current := 0;

 if not (stm32_task_read or stm32_task_RDlock or stm32_task_UNlock or stm32_task_erase or stm32_task_write or stm32_task_verify) then
  stm32_info_string:='Reset ARMka';

 if task_open_with_reset then
  COM_init_as(port_speed, NOPARITY)
 else
  COM_init_as(port_speed, EVENPARITY);

 DTR := DTR_CLEAR;
 tiny_state := true;

 k := GetTickCount;
 while GetTickCount-k<300 do
  begin
   if check_stop then exit;
   sleep(1);
  end;

 if stm32_task_enable then
  if not (stm32_task_erase or
          stm32_task_read or
          stm32_task_write or
          stm32_task_verify or
          stm32_task_RDlock or
          stm32_task_UNlock) then
   if CP210x_SUCCESS = CP210xRT_GetPartNumber(self.handle, @part_number) then
    if part_number = CP210x_CP2103_VERSION then
     begin
      CP210xRT_WriteLatch(self.handle, $03, $00); sleep(64);
      CP210xRT_WriteLatch(self.handle, $03, $02); sleep(64);
      CP210xRT_WriteLatch(self.handle, $03, $00); sleep(64);
      result := true;
      exit;
     end;

 flg := true;
 for k:=1 to stm32_info_progress_total do
  begin
   if check_stop then exit;

   if k>1 then
    Log_add(' ');
   Log_add('Try #'+inttostr(k));

   tiny_send(tiny_cmd_boot, 'tiny_cmd_boot', k);
   stm32_info_progress_current := k;

   if boot_activate then
    begin
     flg := false;
     active_mode := mode_armka;

     t := GetTickCount;
     if boot_init then continue;
     if boot_main_run then break;
     Log_add('Total time : ' + inttostr(GetTickCount-t));

     result:=true;
     exit;
    end;
  end;

 if stm32_task_stop then exit;

 Log_add('ERROR: Bootloader not activated');
 if flg then
  stm32_info_string := 'ERROR: Bootloader not activated';

 result:=false;
 exit;
end;

function tCOMClient.activate_gate:boolean;
var
 k:integer;
 flg : boolean;
 time_out : cardinal;
 second : cardinal;
 time : integer;

 part_number : byte;
 error_code : integer;
 t : cardinal;
begin
 result:=false;
 if stm32_find_disable_spgate then exit;

 error_code := CP210xRT_GetPartNumber(self.handle, @part_number);
 if error_code = CP210x_SUCCESS then
  Log_add('CP210'+chr(ord('0')+part_number)+' chip detected')
 else
  Log_add('It''s not a CP210x chip');

 if (error_code = CP210x_SUCCESS) and (part_number = CP210x_CP2103_VERSION) then
  begin
   sp_gatem_only_reset := true;
   stm32_find_disable_armka := false;

   result := activate_armka;
   
   stm32_find_disable_armka := true;
   sp_gatem_only_reset := false;
   exit;
  end
 else
  if cw_mini_detect_mode then
   begin
    cw_only_reset := true;
    cw_m5_bootloader := true;
    stm32_find_disable_atm := false;
    stm32_find_disable_spgate := true;

    result := activate_atm;

    cw_only_reset := false;
    stm32_find_disable_atm := true;
    stm32_find_disable_spgate := false;
    exit;
   end;

 flg := true;

 stm32_info_string := 'Activate GATE';
 stm32_info_progress_total   := 1000;
 stm32_info_progress_current := 0;

 COM_init_as(port_speed, EVENPARITY);

 DTR := DTR_CLEAR;
 RTS := RTS_CLEAR;

 Log_add(DCD_str);
 Log_add(DSR_str);
 Log_add(CTS_str);
 Log_add(RNG_str);
 Log_add(' ');

 if (RNG <> RNG_VCC) or (CTS = CTS_VCC) or (DSR = DSR_VCC) then
  begin
   stm32_info_string := 'it''s not Gate';
   Log_add(stm32_info_string);
   result:=false;
   exit;
  end
 else
  Log_add('it''s Gate');

 t:=GetTickCount;
 if DCD <> DCD_VCC then
{  begin
   stm32_info_string := 'ERROR: Gate''s power is ON, need OFF';
   sleep(100);
   Log_add(stm32_info_string);
   result:=false;
   exit;
  end;}
  begin
   time_out := GetTickCount + 99999;
   second := GetTickCount;
   while GetTickCount < time_out do
    begin
     time := (time_out - GetTickCount) div 100;
     stm32_info_progress_current := time;

     if GetTickCount > (t+1000) then
      begin
       t := GetTickCount;
       Log_add(inttobin(modem_status, 8));
      end;

     if GetTickCount > second then
      begin
       second := GetTickCount + 1000;
       time := (time_out - GetTickCount) div 1000;
       stm32_info_string :=  '['+inttostr(time)+'] Please, power OFF'; //chr(ord('0')+time div 10)+chr(ord('0')+time mod 10)
      end;

     if DCD = DCD_VCC then break;
     if check_stop then exit;
    end;

   if GetTickCount > time_out then
    begin
     stm32_info_string := 'ERROR: Gate power OFF timeout';
     Log_add(stm32_info_string);
     result:=false;
     exit;
    end;

   log_add('Power OFF');
   log_add(' ');
  end;

 t:=GetTickCount;
 time_out := GetTickCount + 99999;
 second := GetTickCount;
 while GetTickCount < time_out do
  begin
   time := (time_out - GetTickCount) div 100;
   stm32_info_progress_current := time;

   if GetTickCount > (t+1000) then
    begin
     t := GetTickCount;
     Log_add(inttobin(modem_status, 8));
    end;

   if GetTickCount > second then
    begin
     second := GetTickCount + 1000;
     time := (time_out - GetTickCount) div 1000;
     stm32_info_string :=  '['+inttostr(time)+'] Please, switch power ON'; //chr(ord('0')+time div 10)+chr(ord('0')+time mod 10)
    end;

   DTR := DTR_SET;   sleep(1);
   DTR := DTR_Clear; sleep(1);

   if DCD <> DCD_VCC then break;
   if check_stop then exit;
  end;

 if GetTickCount > time_out then
  begin
   stm32_info_string := 'ERROR: Gate power ON timeout';
   Log_add(stm32_info_string);
   result:=false;
   exit;
  end;

 log_add('Power ON');
 log_add(' ');

 stm32_info_string := 'Wait 500ms for VCC stabilaize';
 sleep(500);
 log_add('Wait 500ms (for VCC supply stabilaize)');
 log_add(' ');

 for k:=1 to 1 do
  begin
   if check_stop then exit;

   if boot_activate then
    begin
     flg := false;
     active_mode := mode_gate;

     if boot_init then continue;
     if boot_main_run then break;

     //sleep(2000);
     result:=true;
     exit;
    end;
  end;

 if stm32_task_stop then exit;

 Log_add('ERROR: Bootloader not activated');
 if flg then
  stm32_info_string := 'ERROR: Bootloader not activated';

 result:=false;
 exit;
end;

function tCOMClient.boot_init:boolean;
var
 t : cardinal;
begin
 result := true;

 if boot_get_commands_list then exit;
 if boot_get_verison       then exit;
 if boot_get_id            then exit;

 if stm32_task_UNlock then
  begin
   t := GetTickCount;
   if boot_read_unlock then exit;
   stm32_task_UNlock := false;
   stm32_erase_ignore := (GetTickCount - t) > 2000;
   sleep(128);
   exit;
  end;

 stm32_flash_loaded := 0;
 if boot_read_info         then
  begin
   if stm32_PID = stm32_PID_ultralow_power_line then
    stm32_flash_size := 1 shl 17
   else
    stm32_flash_size := 1 shl 22;

   setlength(stm32_flash_data, stm32_flash_size);
   fillchar(stm32_flash_data[0], stm32_flash_size, $FF);

   Log_add('WARNING: read info fault, stm32_flash_size set to default = ' + inttostr(stm32_flash_size));
  end;

 result := false;
end;

function tCOMClient.boot_main_run:boolean;
var
 code : word;
begin
 result := true;

 if stm32_task_read then
  begin
   if boot_read_flash   then exit;

   if stm32_flash_readed then
    stm32_save_file(stm32_task_filename, @stm32_flash_data[0], stm32_flash_size);
  end;

 if stm32_task_erase and (not stm32_task_write) then
  if boot_erase_all then exit;

 if stm32_task_write then
  if stm32_load_file(stm32_task_filename, @stm32_flash_data[0], stm32_flash_size, @stm32_flash_loaded) then
   begin
    if (gate_lib_serial <> '') and gate_lib_load then
     begin
      Log_add(' ');
      Log_add('======= GATE LIB =========');

      code := 0;
      try
       code := gate_lib_set_serial_func(@stm32_flash_data[0], stm32_flash_size,
        pchar(gate_lib_serial), length(gate_lib_serial), GATE_LIB_CODE_ID_GATE);
      except
       on E : Exception do
        begin
         Log_add('WARNING Exception: '+E.ClassName+' msg: '+E.Message);
        end;
      end;

      if code = GATE_LIB_CODE_NO_SPGATE_ID     then Log_add('GATE LIB: NO_SPGATE_ID');
      if code = GATE_LIB_CODE_CRC_ERROR        then Log_add('GATE LIB: CRC_ERROR');
      if code = GATE_LIB_CODE_SERIAL_INCORRECT then Log_add('GATE LIB: SERIAL_INCORRECT');
      if code = GATE_LIB_CODE_NEW_SERIAL_OK    then Log_add('GATE LIB: NEW_SERIAL_OK');
      Log_add('==========================');
      Log_add(' ');
     end;

    if @evWR_loaded <> nil then
     evWR_loaded(self, stm32_task_filename, @stm32_flash_data[0], stm32_flash_size);
    if (not stm32_erase_ignore) or (stm32_PID = stm32_PID_ultralow_power_line) then
     if boot_erase_all    then exit;
    if boot_write_flash  then exit;
    if boot_verify_flash then exit;
   end
  else
   begin
    stm32_info_string := 'Error load flash file';
    log_add(stm32_info_string+' : '+stm32_task_filename);
    stm32_task_stop := true;
   end;

 if stm32_task_verify then
  if stm32_load_file(stm32_task_filename, @stm32_flash_data[0], stm32_flash_size, nil) then
   begin
    if boot_verify_flash then exit
   end
  else
   begin
    stm32_info_string := 'Error load flash file';
    log_add(stm32_info_string+' : '+stm32_task_filename);
    stm32_task_stop := true;
   end;

 if stm32_task_RDlock then
  begin
   if stm32_task_RDlock then if boot_read_lock then exit;
  end
 else
  if boot_go($08000000) then exit;

 if not (stm32_task_read or stm32_task_RDlock or stm32_task_UNlock or stm32_task_erase or stm32_task_write or stm32_task_verify) then
  begin
   stm32_info_string := 'ARMka reset [OK]';
   stm32_info_progress_total   := 1;
   stm32_info_progress_current := 1;
  end;

 result := false;
end;

procedure tCOMClient.set_serial_name(name : string);
begin
 port_name := name;
 port_name_serial := true;
end;

function  tCOMClient.hardware_open;
var
 ticks:cardinal;
 cp_list : tCP210x_enum;
 k : integer;
 modem_stat:dword;
 length_byte : byte;
 cp_result : CP210x_STATUS;
label
 go_work, find_serial;
begin
 cw_retry := 0;
 cw_progress := 0;
 cw_no_founded := false;
 cw_finished := false;
 cw_error := false;
 cw_ok := false;

 stm32_erase_ignore := false;

 result:=false;
 if port_speed=0 then
  port_speed := CBR_115200;

 stm32_chip_id_readed := false;
 FillChar(stm32_chip_id, sizeof(stm32_chip_id), 0);

 self.log_name := port_name;

 stm32_info_progress_current := 0;
 stm32_info_progress_total   := 1;
 stm32_info_result := '';
 stm32_info_string := 'Open COM port';

 ticks :=GetTickCount;

 if port_name_serial then
  begin
   port_name_serial := false;
   goto find_serial;
  end;

 while com_open do
  begin
find_serial:
   cp_list := tCP210x_enum.create(self.Log_add);
   if cp_list <> nil then
    begin
     for k:=0 to cp_list.count-1 do
      if system.pos(UpperCase(port_name), UpperCase(cp_list.list[k].serial)) > 0 then
       begin
        Log_add(port_name + ' is serial in CP210x list founded');
        port_name := cp_list.list[k].com_path;
        if length(port_name) < 4 then
         port_name := cp_list.list[k].com_name;

        Log_add(#9'Serial      : ' + cp_list.list[k].serial);
        Log_add(#9'com_name    : ' + cp_list.list[k].com_name);
        Log_add(#9'com_path    : ' + cp_list.list[k].com_path);
        Log_add(#9'description : ' + cp_list.list[k].description);
        Log_add(#9'!for open!  : ' + port_name);
        Log_add('');
        if com_open then
         begin
          cw_error := true;
          cw_finished := true;
          log_add('Open error');
          exit;
         end;
        goto go_work;
       end;
    end;

   if cw_mini_detect_mode then
    begin
     cw_no_founded := true;
     cw_finished := true;
    end;

   exit;
  end;

go_work:
 ZeroMemory(@real_port_serial[0], sizeof(real_port_serial));
 length_byte := math.Min(255, length(real_port_serial));
 cp_result := CP210xRT_GetDeviceSerialNumber(handle, @real_port_serial[0], @length_byte, true);
 if (cp_result <> CP210x_SUCCESS) or
    (length_byte = 255) or
    (length_byte = 0)
 then
  begin
   Log_add('ERROR detect real port serial');
   StrCopy(real_port_serial, pchar(port_name));
  end
 else
  Log_add('Real port serial : ' + real_port_serial);

 invert_rts := (system.pos('cheatwatch_ru_5m', real_port_serial) <> 0);

 if cw_mini_detect_mode and cw_only_reset then
  begin
    while true do
     begin
      COM_init_as(port_speed, port_parity);
      if system.pos('cheatwatch_ru_5m', real_port_serial) <> 0 then
       begin
        DTR := DTR_SET;
        RTS := RTS_SET;
        sleep(100);
        RTS := RTS_Clear;
        Log_add('CW_5m started ...');
       end
      else
       begin
      DTR := DTR_SET;

      RTS := RTS_Clear;
      sleep(100);
      RTS := RTS_SET;
      sleep(100);
      RTS := RTS_Clear;
      sleep(50);

      DTR := DTR_SET;
      sleep(50);
        Log_add('old CW_v5 started ...');
       end;

      if not GetCommModemStatus(handle, modem_stat) then
       begin
        cw_no_founded := true;
        cw_finished := true;
        break;
       end;

      if cw_upload then
       break
      else
       begin
        inc(cw_retry);
        if cw_retry >= 10 then break;
       end;

      cw_progress := 0;
      cw_finished := false;
      cw_error := false;
      cw_ok := false;

      Log_add(' ');
      Log_add('====================================');
      Log_add('retry #' + IntToStr(cw_retry));
      Log_add('====================================');
      Log_add(' ');
    end;

   result := false;
   FileClose(handle);
   exit;
  end;

 if no_activate then
  begin
   COM_init_as(port_speed, port_parity);
   if system.pos('cheatwatch_ru_5m', real_port_serial) <> 0 then
    begin
     DTR := DTR_SET;
     RTS := RTS_SET;
     sleep(100);
     RTS := RTS_Clear;
    end;
  end
 else
  begin
   if not activate_atm then
    if not activate_armka then
     if not activate_gate then
      begin
       FileClose(handle);
       exit;
      end;
   ticks := GetTickCount - ticks;
   Log_add('Total time = '+FloatToStrF(ticks / 1000.0, ffGeneral, 6, 2)+' sec');

   if not stm32_task_stop then
    begin
     stm32_info_result := datetostr(date)+' '+TimeToStr(time);
     if stm32_task_RDlock then stm32_info_result := stm32_info_result + ' [RDlock]' else
     if stm32_task_read   then stm32_info_result := stm32_info_result + ' [Read]' else
     if stm32_task_erase  then stm32_info_result := stm32_info_result + ' [Erase]' else
     if stm32_task_write  then stm32_info_result := stm32_info_result + ' [Write]' else
     if stm32_task_verify then stm32_info_result := stm32_info_result + ' [Verify]' else
      stm32_info_result := stm32_info_result + ' [RESET]';
     Log_add(' ');
     Log_add(stm32_info_result);
     Log_add(' ');
    end
   else
    begin
     stm32_info_result := '';
     Log_add(' ');
     Log_add('TASK ABORTED');
     Log_add(' ');
    end;
  end;

 if task_open_with_reset then
  begin
   PurgeComm(handle, PURGE_RXCLEAR);
   PurgeComm(handle, PURGE_RXABORT);
   com_read_data(nil, 1000, nil, 10);
   PurgeComm(handle, PURGE_RXABORT);
   PurgeComm(handle, PURGE_RXCLEAR);
  end;

 zero_close:=false;
 if (stm32_task_enable = false) or no_activate or task_open_with_reset then
  result := true
 else
  FileClose(handle);
end;

function tCOMClient.cw_upload:boolean;
var
 str : string[255];
 readed : cardinal;
 timeout : cardinal;
 error : boolean;

 data : array of byte;
 addr : integer;
 size : integer;
const
 activate : array[0..1] of ansichar = '#f';
label
 error_exit;
begin
 result := false;

 cw_progress := 0;
 cw_no_founded := false;
 cw_finished := false;
 cw_error := false;
 cw_ok := false;

 timeout := GetTickCount + 4000;
 while (GetTickCount < timeout) do
  begin
   ZeroMemory(@str[0], 255);
   com_read_data(@str[1], 254, @readed, 33);
   str[0] := ansichar(readed);

   if str <> '' then
    Log_add('RD as string: "' + str + '"');

   if pos('uploader', str) <> 0 then
    begin
     Log_add('Start Uploader');
     break;
    end;
  end;

 if (GetTickCount >= timeout) then
  begin
  error_exit:
   Log_add('Error: Uploader not respond');
   cw_error := true;
   cw_finished := true;
   exit;
  end
 else
  Log_add(' ');

 log_add(' ');
 log_add('Wait GO responce ... ');
 log_add(' ');

 timeout := GetTickCount + 3000;
 while (GetTickCount < timeout) do
  begin
   error := com_io_data('#f', @str[1], 254, @readed, 100);
 str[0] := ansichar(readed);
   if str <> '' then
    Log_add('RD as string, wait go: "' + str + '"');
   if (not error) and (system.Pos('GO!', str) > 0) then
    break;
  end;
 if (GetTickCount >= timeout) then
  goto error_exit;

 SetLength(data, $400000);
 if not stm32_load_file(cw_file, @data[0], length(data), @size) then
  begin
   Log_add('cant load data file');
   cw_error := true;
   cw_finished := true;
   exit;
  end;

 if size and 255 = 0 then
  size := size and $FFFFFF00
 else
  size := size and $FFFFFF00 + 256;

 Log_add('Loaded file, size = ' + inttostr(size));

 addr := 0;
 cw_progress := 0;

 while (addr < size) do
  begin
   alt_log_start;
   error := com_io_data(data2str(@data[addr], 256), @str[1], 2, @readed, 5000);
   alt_log_stop(error);
   str[0] := ansichar(readed);
   if error or (str <> '!N') then
    begin
     Log_add('cant upload in addr ' + inttostr(addr));
     cw_error := true;
     cw_finished := true;
     exit;
    end
   else
    result := true;

   inc(addr, 256);
   cw_progress := round(addr / size * 1000);

   if addr and $1FFF = 0 then
    Log_add('Uploading : '+inttostr(cw_progress) + ' %% ...');
  end;

 Log_add('Upload complite');
 cw_ok := true;
 cw_progress := 1000;
 cw_finished := true;

 result := true;
end;

function  tCOMClient.hardware_close;
begin
 if active_mode = mode_atm then
  begin
   DTR := DTR_SET;
   write_break(BREAK_SET);
   sleep(100);
  end;

 PurgeComm(handle, PURGE_TXABORT or PURGE_RXABORT or PURGE_TXCLEAR or PURGE_RXCLEAR);
 FileClose(handle);
 handle:=$FFFFFFFF;

 result:= true;
end;

function tCOMClient.hardware_errorcode:integer;
begin
 result:=0;
end;

function tCOMClient.hardware_check;
begin
 if tx_fifo.blocks_count=0 then
  sleep(1);
 read_ready  := true;
 result      := True;

// if random(100)<10 then
//  sleep(random(50));
end;

procedure _inf(var v);
begin
end;

function  tCOMClient.hardware_read;
begin
 readed:=0;
 result:=ReadFile(Handle, buf^, size, cardinal(readed), nil);
 if result <> true then
  Log_add('[WinAPI] ReadFile(...) = '+SysErrorMessage(GetLastError));
end;

function  tCOMClient.hardware_write;
begin
 result:=WriteFile(Handle, buf^, size, cardinal(writed), nil);
end;

function tCOMClient.modem_status:dword;
begin
 result:=0;
 GetCommModemStatus(handle, result);
end;

procedure tCOMClient.write_dtr(v:boolean);
begin
 if self=nil then exit;

 v := v xor invert_dtr;

 if v = DTR_SET then
  EscapeCommFunction(Handle, windows.SETDTR)
 else
  EscapeCommFunction(Handle, windows.CLRDTR)
end;

procedure tCOMClient.write_rts(v:boolean);
begin
 if self=nil then exit;

 v := v xor invert_rts;

 if v = RTS_SET then
  EscapeCommFunction(Handle, windows.SETRTS)
 else
  EscapeCommFunction(Handle, windows.CLRRTS)
end;

procedure tCOMClient.write_break(v:boolean);
begin
 if self=nil then exit;
 if v = BREAK_SET then
  EscapeCommFunction(Handle, windows.SETBREAK)
 else
  EscapeCommFunction(Handle, windows.CLRBREAK)
end;

function  tCOMClient.read_rng:boolean;
begin
 result := false;
 if self=nil then exit;

 if (modem_status and MS_RNG_MASK) = MS_RNG_VCC then
  result := RNG_VCC
 else
  result := RNG_GND;
end;

function  tCOMClient.read_dcd:boolean;
begin
 result := false;
 if self=nil then exit;

 if (modem_status and MS_DCD_MASK) = MS_DCD_VCC then
  result := DCD_VCC
 else
  result := DCD_GND;
end;

function  tCOMClient.read_cts:boolean;
begin
 result := false;
 if self=nil then exit;

 if (modem_status and MS_CTS_MASK) = MS_CTS_VCC then
  result := CTS_VCC
 else
  result := CTS_GND;
end;

function  tCOMClient.read_dsr:boolean;
begin
 result := false;
 if self=nil then exit;

 if (modem_status and MS_DSR_MASK) = MS_DSR_VCC then
  result := DSR_VCC
 else
  result := DSR_GND;
end;

function  tcomclient.RNG_str:string;
begin
 result := '';
 if self=nil then exit;

 if (modem_status and MS_RNG_MASK) = MS_RNG_VCC then
  result := 'RNG = VCC'
 else
  result := 'RNG = gnd';
end;

function  tcomclient.DCD_str:string;
begin
 result := '';
 if self=nil then exit;

 if (modem_status and MS_DCD_MASK) = MS_DCD_VCC then
  result := 'DCD = VCC'
 else
  result := 'DCD = gnd';
end;

function  tcomclient.CTS_str:string;
begin
 result := '';
 if self=nil then exit;

 if (modem_status and MS_CTS_MASK) = MS_CTS_VCC then
  result := 'CTS = VCC'
 else
  result := 'CTS = gnd';
end;

function  tcomclient.DSR_str:string;
begin
 result := '';
 if self=nil then exit;

 if (modem_status and MS_DSR_MASK) = MS_DSR_VCC then
  result := 'DSR = VCC'
 else
  result := 'DSR = gnd';
end;

function tcomclient.addr2str(addr:cardinal):string;
begin
 result := '';
 result := result + byte2str(addr shr 24);  addr := addr shl 8;
 result := result + byte2str(addr shr 24);  addr := addr shl 8;
 result := result + byte2str(addr shr 24);  addr := addr shl 8;
 result := result + byte2str(addr shr 24);  addr := addr shl 8;

 result := xor2str(result);
end;

function tcomclient.byte2str(b:byte):string;
begin
 result:=ansichar(b);
end;

function tcomclient.word2str(w:word):string;
begin
 result:=byte2str(w shr 8) + byte2str(w and 255);
end;

function tcomclient.data2str(data:pointer; size:integer):string;
var
 b:pbyte absolute data;
begin
 result:='';
 while size>0 do
  begin
   result:=result+byte2str(b^);
   inc(b);
   dec(size);
  end;
end;

function tcomclient.cmd2str(cmd:byte):string;
begin
 result:=byte2str(cmd)+byte2str(cmd xor $FF);
end;

function tcomclient.xor2str(s:string):string;
var
 val:byte;
 k:integer;
begin
 if length(s)=1 then
  val:=$FF
 else
  val:=$00;

 for k:=1 to length(s) do
  val := val xor ord(s[k]);
 result:=s+byte2str(val);
end;

function str2hex(s:string):string;
begin
 result:='';
 while s<>'' do
  begin
   result:=result+inttohex(ord(s[1]),2)+' ';
   delete(s,1,1);
  end;
end;

function tcomclient.com_wait_for(name:string; mask, value:integer; timeout:cardinal):boolean;
var
 timer:tmillisecond_timer;
begin
 milliseconds_start(timer);

 while ((modem_status and mask)<>value) do
  begin
   sleep(1);
   if round(milliseconds_get(timer))>timeout then
    begin
     result:=false;
     Log_add('Event "'+name+'" timeout = '+milliseconds_str(timer)+' ms');
     exit;
    end;
  end;

 result:=true;
 waiting_time_ms := round(milliseconds_get(timer));
 Log_add('Event "'+name+'" ok, time = '+milliseconds_str(timer)+' ms');
end;

function tcomclient.com_write_str(s:string):boolean;
var
 writed:cardinal;
begin
 writed := 0;
 Log_add('WR: '+str2hex(s));

 if not WriteFile(Handle, s[1], length(s), cardinal(writed), nil) then
  begin
   Log_add('Error write');
   result:=true;
   exit;
  end;

 if writed<>length(s) then
  begin
   Log_add('Error writed '+inttostr(length(s)-writed)+' bytes');
   result:=true;
   exit;
  end;

 result:=false;
end;

function tcomclient.com_read_data(data:pointer; data_size:cardinal; data_readed:pcardinal; timeout:integer):boolean;
var
 timer:tmillisecond_timer;

 readed : cardinal;
 count  : cardinal;
begin
 milliseconds_start(timer);
 setlength(readed_buf, 0);
 setlength(readed_buf, 512);
 count:=0;
 if data_readed<>nil then
  data_readed^ := 0;

 timeout := timeout + round((1000 / port_speed) * (data_size*15)*2);

 while (milliseconds_get(timer) < timeout) and (count < data_size) and (count < length(readed_buf)) do
  begin
   if not ReadFile(Handle, readed_buf[count], length(readed_buf)-count, readed, nil) then
    begin
     Log_add('Error read');
     result:=true;
     exit;
    end;

   if timeout >= 1000 then
    if check_stop then
     begin
      result:=true;
      exit;
     end;

   inc(count,readed);
   if (count >= data_size) or (count >= length(readed_buf)) then
    break;

   sleep(1);
  end;

 setlength(readed_buf, count);

 if count>0 then
  if data<>nil then
   Move(readed_buf[0], data^, math.min(data_size, count));

 if data_readed<>nil then
  data_readed^ := math.min(data_size, count);

 Log_add('RD: '+str2hex(data2str(readed_buf, count))+'('+milliseconds_str(timer)+' ms)');
 result:=false;
end;

function tcomclient.com_io_data(write_str:string; data:pointer; read_size:cardinal; data_readed:pcardinal; timeout:integer):boolean;
var
 ms:integer;
begin
 result := false;
 result := result or com_write_str(write_str);

 ms := round((1000 / port_speed) * ((length(write_str)-read_size)*15));
 if ms<0 then
  ms := 0;

 result := result or com_read_data(data, read_size, data_readed, timeout+ms);
end;

function tcomclient.boot_get_verison:boolean;
var
 error   : boolean;
 readed  : cardinal;

 version : record
  ack_a   : byte;
  version : byte;
  opt_a   : byte;
  opt_b   : byte;
  ack_b   : byte;
 end;
begin
 result:=true;
 fillchar(version, sizeof(version), 0);

 stm32_info_string := 'Get bootloader version';
 Log_add('=== Bootloader version ===');
 error:=com_io_data(cmd2str(boot_commands_list.VerProt), @version, sizeof(version), @readed, 100);

 if error or
   (version.ack_a <> tboot_ack) or
   (version.ack_b <> tboot_ack) or
   (readed<sizeof(version))
 then
  begin
   Log_add('Bootloader version error');
   stm32_info_string := 'Bootloader version error';
   exit;
  end;

 stm32_boot_version := version.version;
 stm32_options      := word(version.opt_a) or (word(version.opt_b) shl 8);

 Log_add('stm32_boot_version : ' + inttohex(stm32_boot_version, 2));
 Log_add('stm32_options      : ' + inttohex(stm32_options, 8));

 Log_add(' ');

 result:=false;
end;

function tcomclient.boot_read_lock:boolean;
var
 error   : boolean;
 readed  : cardinal;
 ack_info : record
  ack_a : byte;
  ack_b : byte;
 end;
begin
 result:=true;
 fillchar(ack_info, sizeof(ack_info), 0);

 stm32_info_string :='Lock STM32 flash';
 Log_add('=== Lock STM32 flash ===');
 error:=com_io_data(cmd2str(boot_commands_list.RdLock), @ack_info, sizeof(ack_info), @readed, 100);

 if error or
   ((readed = 1) and (ack_info.ack_a <> tboot_ack)) or
   ((readed = 2) and (ack_info.ack_a <> tboot_ack) and (ack_info.ack_b <> tboot_ack))
 then
  begin
   Log_add('Lock STM32 flash error');
   stm32_info_string := 'Lock STM32 flash error';
   exit;
  end;

 Log_add('Lock STM32 flash OK');
 stm32_info_string := 'Lock STM32 flash OK';
 Log_add(' ');
 result:=false;
end;

function tcomclient.boot_read_unlock:boolean;
var
 error   : boolean;
 readed  : cardinal;
 ack_info : record
  ack_a : byte;
  ack_b : byte;
 end;
 timeout : integer;
begin
 result:=true;
 fillchar(ack_info, sizeof(ack_info), 0);

 stm32_info_string :='Unlock STM32 flash';
 Log_add('=== Unlock STM32 flash ===');

 if (stm32_PID = stm32_PID_STM32F4xx) or (stm32_PID = stm32_PID_STM32F2xx)  then
  timeout := 30000
 else
  timeout := 3000;

 error:=com_io_data(cmd2str(boot_commands_list.RdUnLock), @ack_info, sizeof(ack_info), @readed, timeout);

 if error or
   ((readed = 1) and (ack_info.ack_a <> tboot_ack)) or
   ((readed = 2) and (ack_info.ack_a <> tboot_ack) and (ack_info.ack_b <> tboot_ack))
 then
  begin
   Log_add('Unlock STM32 flash error');
   stm32_info_string := 'Unlock STM32 flash error';
   exit;
  end;

 Log_add('Unlock STM32 flash OK');
 stm32_info_string := 'Unlock STM32 flash OK';
 Log_add(' ');
 result:=false;
end;

function tcomclient.boot_get_id:boolean;
var
 error   : boolean;
 readed  : cardinal;
 name    : string;

 id_info : record
  ack_a : byte;
  size  : byte;
  PID_H : byte;
  PID_L : byte;
  ack_b : byte;
 end;
begin
 result:=true;
 fillchar(id_info, sizeof(id_info), 0);

 stm32_info_string :='Get STM32 chip ID';
 Log_add('=== STM32 chip ID ===');
 error:=com_io_data(cmd2str(boot_commands_list.ID), @id_info, sizeof(id_info), @readed, 100);

 if error or
   (id_info.ack_a <> tboot_ack) or
   (id_info.ack_b <> tboot_ack) or
   (id_info.size  <> 1) or
   (readed<sizeof(id_info))
 then
  begin
   Log_add('STM32 chip ID error');
   stm32_info_string := 'STM32 chip ID error';
   exit;
  end;

 stm32_PID := word(id_info.PID_L) or (word(id_info.PID_H) shl 8);

 case stm32_PID of
  stm32_PID_Low_density            : name := 'Low-density';
  stm32_PID_Medium_density         : name := 'Medium-density';
  stm32_PID_High_density           : name := 'High-density';
  stm32_PID_Connectivity_line      : name := 'Connectivity line';
  stm32_PID_Medium_density_vline   : name := 'Medium-density value line';
  stm32_PID_High_density_vline     : name := 'High-density value line';
  stm32_PID_XL_density             : name := 'XL-density';
  stm32_PID_ultralow_power_line    : name := 'Mediumdensity ultralow power line';
  stm32_PID_STM32F2xx              : name := 'STM32F2xx devices';
  stm32_PID_STM32F4xx              : name := 'STM32F4xx devices';
 else
  name := 'UNKNOW';
 end;
 Log_add('stm32_PID = [0x' + inttohex(stm32_PID, 4)+'] : "'+name+'"');

 Log_add(' ');

 result:=false;
end;

function tcomclient.boot_get_commands_list;
var
 error:boolean;
 readed:cardinal;
begin
 result:=true;
 stm32_info_string := 'Get commands list';


 Log_add('=== Bootloader GETCMD ===');
 error:=com_io_data(cmd2str(0), @boot_commands_list, sizeof(boot_commands_list), @readed, 100);

 if error or
   (boot_commands_list.ack <> tboot_ack) or
   (boot_commands_list.ack_end <> tboot_ack) or
   (boot_commands_list.size < sizeof(boot_commands_list)-4) or
   (readed<sizeof(boot_commands_list))
 then
  begin
   Log_add('Bootloader GETCMD error');
   stm32_info_string :='GETCMD error';
   exit;
  end;

 Log_add(#9'Size     = '+inttohex(boot_commands_list.size,2));
 Log_add(#9'Version  = '+inttohex(boot_commands_list.Version,2));
 Log_add(#9'GetCmd   = '+inttohex(boot_commands_list.GetCmd,2));
 Log_add(#9'VerProt  = '+inttohex(boot_commands_list.VerProt,2));
 Log_add(#9'ID       = '+inttohex(boot_commands_list.ID,2));
 Log_add(#9'Read     = '+inttohex(boot_commands_list.Read,2));
 Log_add(#9'GO       = '+inttohex(boot_commands_list.GO,2));
 Log_add(#9'Write    = '+inttohex(boot_commands_list.Write,2));
 Log_add(#9'Erase    = '+inttohex(boot_commands_list.Erase,2));
 Log_add(#9'WrLock   = '+inttohex(boot_commands_list.WrLock,2));
 Log_add(#9'WrUnLock = '+inttohex(boot_commands_list.WrUnLock,2));
 Log_add(#9'RdLock   = '+inttohex(boot_commands_list.RdLock,2));
 Log_add(#9'RdUnLock = '+inttohex(boot_commands_list.RdUnLock,2));
 Log_add(' ');

 result:=false;
end;

function tcomclient.boot_read_info:boolean;
type
 pid_string = ^tid_string;
 tid_string = array[0..6] of ansichar;
var
 v   : integer;
 cnt : integer;
 block  : array[0 .. $30-1] of byte;
 id_str : pid_string;
 res_a : boolean;
 res_b : boolean;
begin
 stm32_info_string := 'Read info cmd';
 ZeroMemory(@STM32_info, sizeof(STM32_info));
 ZeroMemory(@block[0], sizeof(block));

 if stm32_PID = stm32_PID_ultralow_power_line then
  begin
   Log_add('boot_read_info (flash_size and id) ignored for stm32_PID_ultralow_power_line series');
   result := true;
   {//http://www.st.com/st-web-ui/static/active/en/resource/technical/document/reference_manual/CD00240193.pdf
   res_a := boot_read($1FF800CC,  @block[$00],  4);
   res_b := boot_read($1FF800D0,  @block[$04], 16);
   result := res_a or res_b;

   if not result then
    begin
     move(block[$00], STM32_info.flash_size, sizeof(STM32_info.flash_size));
     move(block[$04], STM32_info.cpu_id_a, 12);
    end;
   Log_add(' ');
   Log_add('VOID');
   com_read_data(nil, 10, nil, 10);
{   result := true;
   if not boot_read($1FF8004C,  @STM32_info.flash_size, sizeof(STM32_info.flash_size)) then
   if not boot_read($1FF80050,  @STM32_info.cpu_id_a, 12) then
    result := false;}
  end
 else
 if stm32_PID = stm32_PID_STM32F4xx then
  begin
   result := boot_read($1FFF7A00,  @block[0], sizeof(block));

   if not result then
    begin
     move(block[$22], STM32_info.flash_size, sizeof(STM32_info.flash_size));
     move(block[$10], STM32_info.cpu_id_a, 12);
    end;
  end
 else
  begin
   result:=boot_read($1FFFF7E0,  @STM32_info, sizeof(STM32_info));
  end;

 if result=true then
  begin
   stm32_info_string := 'Read mem info cmd error';
   exit;
  end;

 v   := STM32_info.flash_size;
 cnt := 1;
 while v>0 do
  begin
   v:=v shr 1;
   inc(cnt);
  end;
 stm32_flash_size := STM32_info.flash_size * 1024;//1 shl (cnt+8);

 setlength(stm32_flash_data, stm32_flash_size);
 fillchar(stm32_flash_data[0], stm32_flash_size, $FF);

 Log_add('=== STM32_info ===');
 Log_add('flash_size  : '+inttostr(STM32_info.flash_size)+' kb ('+inttostr(stm32_flash_size)+' bytes)');
 Log_add('reserved_a  : '+inttostr(STM32_info.reserved_a));
 Log_add('reserved_b  : '+inttostr(STM32_info.reserved_b));
 Log_add('cpu_id_a    : '+inttostr(STM32_info.cpu_id_a));
 Log_add('cpu_id_b    : '+inttostr(STM32_info.cpu_id_b));
 Log_add('cpu_id_c    : '+inttostr(STM32_info.cpu_id_c));
 Log_add('cpu_id_d    : '+inttostr(STM32_info.cpu_id_d));

 move(STM32_info, stm32_chip_id, sizeof(stm32_chip_id));
 stm32_chip_id_readed := true;

 if stm32_PID = stm32_PID_STM32F4xx then
  begin
   id_str := pid_string(@(block[$15]));
   Log_add('ID_string   : "' + id_str^ + '"');
  end;

 if active_mode = mode_atm then
  begin
   Log_add('ProtectHASH : '+inttostr(stmid2hash(STM32_info)));
   Log_add('CheckHASH   : '+inttostr(hash2checking(stmid2hash(STM32_info))));
  end;

 Log_add(' ');
end;

function tcomclient.boot_erase_sector(sector_num:word):boolean;
var
 error:boolean;
begin
 result := true;
 stm32_info_string := 'Erasing, #' + IntToStr(sector_num) + ' ...';
 if stm32_PID <> stm32_PID_ultralow_power_line then exit;

 if check_stop then
  begin
   result := false;
   exit;
  end;

 Log_add('=== ERASE #'+ IntToStr(sector_num) + ' command ===');
 error:=com_io_data(cmd2str(boot_commands_list.Erase), nil, 1, nil, 100);
 if error or (length(readed_buf)=0) or (readed_buf[0] <> tboot_ack) then
  begin
   stm32_info_string := 'Bootloader Erase #'+ IntToStr(sector_num) + ' error command code';
   Log_add(stm32_info_string);
   exit;
  end;

 error:=com_io_data(xor2str(#$00#$00 + word2str(sector_num)), nil, 1, nil, 2000);
 if error or (length(readed_buf)=0) or (readed_buf[0] <> tboot_ack) then
  begin
   stm32_info_string := 'Erase #'+ IntToStr(sector_num) + ' error parametr';
   Log_add(stm32_info_string);
   exit;
  end
 else
  begin
   stm32_info_string := 'ERASE #'+ IntToStr(sector_num) + ' ok';
   Log_add(stm32_info_string);
  end;
 Log_add(' ');
 result := false;
end;

function tcomclient.boot_erase_all:boolean;
var
 error:boolean;
 sector : word;
begin
 result := true;
 stm32_info_string := 'Erasing, please wait...';
 {if stm32_PID = stm32_PID_ultralow_power_line then
  begin
   result := false;
   exit;
  end;}

 if check_stop then
  begin
   result := false;
   exit;
  end;

 Log_add('=== ERASE ALL command ===');
 if stm32_PID = stm32_PID_ultralow_power_line then
  begin
   sector := 0;
   stm32_info_progress_current := 0;
   stm32_info_progress_total   := stm32_flash_size div 256;

   while sector < stm32_info_progress_total do
    begin
     alt_log_start;
     result := boot_erase_sector(sector);
     alt_log_stop(result);
     if result then break;

     if (sector and 31) = 0 then
      Log_add('Erase sector #' + inttostr(sector) + '...');
     inc(sector);
     stm32_info_progress_current := sector;
    end;
   stm32_info_progress_current := 0;
   stm32_info_progress_total := 1;

   if sector < 8 then
    begin
     stm32_info_string := 'Erase error "all" sector = ' + IntToStr(sector);
     Log_add(stm32_info_string);
     result := true;
     exit;
    end
   else
    Log_add(' ');
  end
 else
  begin
   error:=com_io_data(cmd2str(boot_commands_list.Erase), nil, 1, nil, 100);
   if error or (length(readed_buf)=0) or (readed_buf[0] <> tboot_ack) then
    begin
     stm32_info_string := 'Bootloader Erase error command code';
     Log_add(stm32_info_string);
     exit;
    end;

   if (stm32_PID = stm32_PID_STM32F4xx) or (boot_commands_list.Erase = $44) then
    error:=com_io_data(#$FF#$FF#$00, nil, 1, nil, 32000)
   else
    error:=com_io_data(#$FF#$00, nil, 1, nil, 3000);

   if error or (length(readed_buf)=0) or (readed_buf[0] <> tboot_ack) then
    begin
     stm32_info_string := 'Erase error "all" parametr';
     Log_add(stm32_info_string);
     exit;
    end
   else
    begin
     stm32_info_string := 'ERASE ALL ok';
     Log_add(stm32_info_string);
    end;
   Log_add(' ');
  end;

 stm32_info_progress_total   := 1;
 stm32_info_progress_current := 0;

 result := false;
end;


function tcomclient.boot_verify_flash:boolean;
const
 block_size = 256;
var
 block : array[0 .. block_size-1] of byte;
 addr  : cardinal;
 count : cardinal;
 kb     : cardinal;
 kb_old : cardinal;
 to_verify_size : cardinal;

 k, l :integer;

 s:string;
begin
 result := false;
 if length(stm32_flash_data)=0 then exit;
 if check_stop then
  begin
   result := false;
   exit;
  end;

 addr  := $08000000;
 count := 0;

 to_verify_size := stm32_flash_size-1;
 while (stm32_flash_data[to_verify_size] = $FF) and
       (to_verify_size>0)
 do
  dec(to_verify_size);

 Log_add('=== Verify flash ===');
 Log_add('Start ADDR  = 0x'+inttohex(addr,8));
 Log_add('Verify size = '+inttostr(to_verify_size));

 stm32_info_progress_current := 0;
 stm32_info_progress_total   := to_verify_size;
 stm32_info_string := 'Verify ...';

 kb_old :=0;
 while count < to_verify_size do
  begin
   alt_log_start;
   result := boot_read(addr, @(block[0]), block_size);
   alt_log_stop(result);
   if result then exit;
   if check_stop then exit;

   for k:=0 to block_size-1 do
    if (count + k) < to_verify_size then
     if stm32_flash_data[count + k] <> block[k] then
      begin
       Log_add(' ');
       Log_add('VERIFY ERROR');
       Log_add('flash ADDR  = 0x'+inttohex(addr+k,8));
       Log_add('file offset = 0x'+inttohex(count+k,8));
       stm32_info_string := 'VERIFY ERROR at 0x'+inttohex(addr+k,8);
       stm32_task_stop := true;

       s:='in FLASH : ';
       for l:=0 to math.Min(block_size-k, 16)-1 do
        s:=s+IntToHex(block[l + k],2)+' ';
       Log_add(s);

       s:='in FILE  : ';
       for l:=0 to math.Min(block_size-k, 16)-1 do
        s:=s+IntToHex(stm32_flash_data[count + l + k],2)+' ';
       Log_add(s);

       Log_add(' ');
       result := false;
       exit;
      end;

   kb := count div 1000;
   if kb <> kb_old then
    begin
     stm32_info_string := 'Verify '+inttostr(kb)+' kb ...';
     Log_add(stm32_info_string);
    end;
   kb_old := kb;

   inc(count, block_size);
   inc(addr,  block_size);

   stm32_info_progress_current := count;
   if port_speed<115200 then
    stm32_info_string := 'Verify '+inttostr(count)+' ...';
  end;
 Log_add('Verified '+inttostr(count)+' bytes ok');
 Log_add('Verified flash OK');
 Log_add(' ');

 stm32_info_string := 'Verify ALL '+inttostr(count)+' bytes OK';

 for k:=0 to stm32_flash_size-1 do
  if stm32_flash_data[k]<>$FF then exit;

 result := false;
end;

function tcomclient.boot_read_flash:boolean;
var
 k : integer;
 addr  : cardinal;
 count : cardinal;
 kb     : cardinal;
 kb_old : cardinal;
const
 block_size = 256;
begin
 result := false;
 if length(stm32_flash_data)=0 then exit;
 if check_stop then
  begin
   result := false;
   exit;
  end;

 addr  := $08000000;
 count := 0;

 Log_add('=== Read flash ===');
 Log_add('Start ADDR = '+inttohex(addr,8));
 Log_add('Flash size = '+inttostr(stm32_flash_size));

 stm32_info_progress_current := 0;
 stm32_info_progress_total   := stm32_flash_size;
 stm32_info_string := 'Read ...';

 kb_old :=0;
 while count < stm32_flash_size do
  begin
   alt_log_start;
   result := boot_read(addr, @(stm32_flash_data[count]), block_size);
   alt_log_stop(result);
   if result then exit;
   if check_stop then exit;

   kb := count div 1000;
   if kb <> kb_old then
    begin
     stm32_info_string := 'Read '+inttostr(kb)+' kb ...';
     Log_add(stm32_info_string);
    end;
   kb_old := kb;

   inc(count, block_size);
   inc(addr,  block_size);
   stm32_info_progress_current := count;
   if port_speed<115200 then
    stm32_info_string := 'Read '+inttostr(count)+' ...';
  end;
 Log_add('Readed '+inttostr(count)+' bytes ok');
 Log_add('Read all flash ok');
 Log_add(' ');
 stm32_info_string :='Read all '+inttostr(count)+' bytes flash OK';

 result := false;

 stm32_flash_readed := true;
 for k:=0 to stm32_flash_size-1 do
  if stm32_flash_data[k]<>$FF then exit;
 stm32_flash_readed:=false;
end;

function tcomclient.boot_write_flash:boolean;
var
 addr  : cardinal;
 count : cardinal;
 kb     : cardinal;
 kb_old : cardinal;
 to_write_size : cardinal;
const
 block_size = 256;
begin
 result := false;
 if length(stm32_flash_data)=0 then exit;
 if check_stop then
  begin
   result := false;
   exit;
  end;
 result := false;

 addr  := $08000000;
 count := 0;
 to_write_size := ((stm32_flash_loaded + $FFF) and $FFFFFF000);

 Log_add('=== Write flash ===');
 Log_add('Start ADDR   = 0x'+inttohex(addr,8));
 Log_add('Program size = '+inttostr(stm32_flash_loaded));
 Log_add('Writed size  = '+inttostr(to_write_size)+' by 4k blocks');

 if stm32_PID = stm32_PID_ultralow_power_line then
  begin
   Log_add('WARNING: 0xFF replaced to 0x00 in the end of firmware for stupid unstandard flash in STM32Lxxx series');
   log_add('from: 0x' + inttohex(stm32_flash_loaded, 8) + ' size: ' + inttostr(to_write_size - stm32_flash_loaded));
   ZeroMemory(@(stm32_flash_data[stm32_flash_loaded]), to_write_size - stm32_flash_loaded);
  end;

 stm32_info_progress_current := 0;
 stm32_info_progress_total   := to_write_size;
 stm32_info_string := 'Write ...';

 stm32_flash_writed := false;

 kb_old :=0;
 while count < to_write_size do
  begin
   alt_log_start;
   result := boot_write(addr, @(stm32_flash_data[count]), block_size);
   alt_log_stop(result);
   if result then exit;
   if check_stop then exit;

   kb := count div 1000;
   if kb <> kb_old then
    begin
     stm32_info_string := 'Write '+inttostr(kb)+' kb ...';
     Log_add(stm32_info_string);
    end;
   kb_old := kb;

   inc(count, block_size);
   inc(addr,  block_size);
   stm32_info_progress_current := count;
   if port_speed<115200 then
    stm32_info_string := 'Write '+inttostr(count)+' ...';
  end;
 Log_add('Writed '+inttostr(count)+' bytes ok');
 Log_add('Write all flash ok');
 Log_add(' ');
 stm32_info_string :='Writed all '+inttostr(count)+' bytes flash OK';
 stm32_flash_writed := true;
end;

function tcomclient.boot_read(addr:Cardinal; data:pointer; size:word):boolean;
var
 error:boolean;
begin
 result:=true;

 Log_add('=== Read command ===');
 Log_add('Addr = '+inttohex(addr,8));
 Log_add('Size = '+inttostr(size));

 error:=com_io_data(cmd2str(boot_commands_list.Read), nil, 1, nil, 100);
 if error or (length(readed_buf)=0) or (readed_buf[0]<>tboot_ack) then
  begin
   stm32_info_string := 'Read ERROR [cmd]'+', addr = 0x'+inttohex(Addr,8);
   Log_add(stm32_info_string);
   Log_add(' ');
   exit;
  end;

 error:=com_io_data(addr2str(addr), nil, 1, nil, 100);
 if error or (length(readed_buf)=0) or (readed_buf[0]<>tboot_ack) or (readed_buf[length(readed_buf)-1]=tboot_error) then
  begin
   stm32_info_string := 'Read ERROR [addr]'+', addr = 0x'+inttohex(Addr,8);
   Log_add(stm32_info_string);
   Log_add(' ');
   exit;
  end;

 error:=com_io_data(xor2str(byte2str(size-1)), nil, size+1, nil, 250);
 if error or (length(readed_buf) <> (size+1)) or (readed_buf[0]<>tboot_ack)  then
  begin
   stm32_info_string := 'Read ERROR [data]'+', addr = 0x'+inttohex(Addr,8);
   Log_add(stm32_info_string);
   Log_add(' ');
   exit;
  end;

 Log_add('Bootloader Read data ok');
 Log_add(' ');

 result:=false;

 if data<>nil then
  Move(readed_buf[1], data^, math.min(size, length(readed_buf)-1));
end;

function tcomclient.boot_write(addr:Cardinal; data:pointer; size:word):boolean;
var
 error:boolean;
begin
 result:=true;

 Log_add('=== Write command ===');
 Log_add('Addr = '+inttohex(addr,8));
 Log_add('Size = '+inttostr(size));

 error:=com_io_data(cmd2str(boot_commands_list.Write), nil, 1, nil, 100);
 if error or (length(readed_buf)=0) or (readed_buf[0]<>tboot_ack) then
  begin
   stm32_info_string := 'Write ERROR [cmd]'+', addr = 0x'+inttohex(Addr,8);
   Log_add(stm32_info_string);
   Log_add(' ');
   exit;
  end;

 error:=com_io_data(addr2str(addr), nil, 1, nil, 100);
 if error or (length(readed_buf)=0) or (readed_buf[0]<>tboot_ack) then
  begin
   stm32_info_string := 'Write ERROR [addr]'+', addr = 0x'+inttohex(Addr,8);
   Log_add(stm32_info_string);
   Log_add(' ');
   exit;
  end;

 error:=com_io_data(xor2str(byte2str(size-1)+data2str(data, size)), nil, 1, nil, 250);
 if error or (length(readed_buf)=0) or (readed_buf[0]<>tboot_ack) then
  begin
   stm32_info_string := 'Write ERROR [data]'+', addr = 0x'+inttohex(Addr,8);
   Log_add(stm32_info_string);
   Log_add(' ');
   exit;
  end;

 Log_add('Bootloader Write data ok');
 Log_add(' ');

 result:=false;
end;

function tcomclient.boot_activate:boolean;
var
 error:boolean;
 readed:integer;
begin
 Log_add(' ');
 Log_add('=== Check Bootloader ===');

 com_read_data(nil, 10, nil, 10);
 error:=com_io_data(chr($7F), nil, 1, nil, 32);
 readed:=length(readed_buf);
 if readed<>0 then
  if (((readed_buf[readed-1] = tboot_ack)   and (readed > 1)) or
      ((readed_buf[readed-1] = tboot_error) and (readed > 1)) or
      ((readed_buf[0] = tboot_ack)   and (readed = 1)) or
      ((readed_buf[0] = tboot_error) and (readed = 1))
     ) and (error=false)
  then
   begin
    Log_add('Bootloader present');
    Log_add(' ');

    result := true;
    exit;
   end
  else
   Log_add('Bootloader ERROR, readed:'+data2str(readed_buf, readed))
 else
  Log_add('Bootloader not present');

 result := false;
end;

function tcomclient.boot_go(addr:cardinal):boolean;
var
 error:boolean;
begin
 result := true;

 Log_add('=== GO command ===');
 Log_add('Addr = '+inttohex(addr,8));
 error:=com_io_data(cmd2str(boot_commands_list.GO), nil, 1, nil, 100);
 if error or (length(readed_buf)=0) or (readed_buf[0] <> tboot_ack) then
  begin
   Log_add('Bootloader GO error command code');
   exit;
  end;

 error:=com_io_data(addr2str(addr), nil, 1, nil, 16);
 if error or (length(readed_buf)=0) or (readed_buf[0] <> tboot_ack) then
  begin
   if port_speed > 15000 then
    begin
     Log_add('Bootloader GO error addr');
     exit
    end
   else
    Log_add('Warning: Bootloader GO addr no response');
  end
 else
  Log_add('Bootloader GO ok');
 Log_add(' ');

 result := false;
end;

procedure tcomclient.COM_init_as(speed:integer; parity:byte);
var
 DCB:tDCB;
 CommTimeouts:TCommTimeouts;
begin
 if speed = 0 then speed := CBR_115200;
 current_speed := speed;
 log_add('COM_init_as(speed='+inttostr(speed)+', parity='+inttostr(parity)+');');

 fillchar(DCB, sizeof(DCB), 0);
 DCB.DCBlength := sizeof(DCB);

 if not open_simple_fast then //????????????????
 GetCommState(handle, DCB);
 DCB.BaudRate:=speed;
 DCB.Parity:=parity;
 DCB.ByteSize:=8;
 DCB.StopBits:=ONESTOPBIT;
 DCB.Flags:=1;
 SetCommState(handle, DCB);

 if not open_simple_fast then DTR := dtr_clear;
 if not open_simple_fast then RTS := rts_clear;

 PurgeComm(handle, PURGE_TXABORT or PURGE_RXABORT or PURGE_TXCLEAR or PURGE_RXCLEAR);

 if not open_simple_fast then
 GetCommTimeouts(handle, CommTimeouts);
 CommTimeouts.ReadIntervalTimeout :=MAXDWORD;
 CommTimeouts.ReadTotalTimeoutMultiplier := 0;
 CommTimeouts.ReadTotalTimeoutConstant := 0;
 CommTimeouts.WriteTotalTimeoutMultiplier := 0;
 CommTimeouts.WriteTotalTimeoutConstant := 0;
 SetCommTimeouts(handle, CommTimeouts);

 if not open_simple_fast then
  begin
 DTR := dtr_clear;
 RTS := rts_clear;
 sleep(16);
 DTR := dtr_clear;
 RTS := rts_clear;
 PurgeComm(handle, PURGE_TXABORT or PURGE_RXABORT or PURGE_TXCLEAR or PURGE_RXCLEAR);
 DTR := dtr_clear;
 RTS := rts_clear;
  end;
 Log_add(' ');
end;

procedure tcomclient.log_add;
begin
 if stat_timer.start = 0 then
  milliseconds_start(stat_timer);

 if alt_log_enabled then
  begin
   if alt_log = nil then
    alt_log := TStringList.Create;
   alt_log.Add(msg);
  end
 else
  begin
   msg := milliseconds_str(stat_timer)+#9+msg;
   inherited log_add(msg);
   milliseconds_start(stat_timer);
  end;
end;

procedure tcomclient.alt_log_start;
begin
 alt_log_enabled := true;
end;

procedure tcomclient.alt_log_stop(print:boolean);
var
 k:integer;
begin
 alt_log_enabled := false;

 if print then
  if alt_log <> nil then
   for k:=0 to alt_log.Count-1 do
    log_add(alt_log.Strings[k]);

 alt_log.Clear;
end;

function tcomclient.check_stop : boolean;
var
 v:string;
begin
 result := false;
 if not stm32_task_stop then exit;

 v := 'Stopped by user';
 if  stm32_info_string <> v then
  begin
   stm32_info_string := v;
   Log_add(stm32_info_string);
  end;
 result := true;
end;

end.