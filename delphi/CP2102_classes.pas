unit CP2102_classes;

interface
uses
  Windows, Messages, SysUtils, Variants, Classes, registry,
  CP210xManufacturingDLL;

type
  tCP210x_log_event = procedure(s:string) of object;

  tCP2102_reg_item = record     //Запись информации о ком портах SiLabs CP2102 на базе реестрта
   com_name    : string;        //Имя ком порта в винде, в виде "COMxxx", например "COM7"
   path        : string;        //Путь к символьному файлу для открытия устройства ком порта , рекомендуется по данному именю открывать файл
   serial      : string;        //Серийник для данного ком порта, до 16 символов, количество указано в константе CP210x_MAX_SERIAL_STRLEN в CP210xManufacturingDLL
  end;

  tCP210x_enum_item = record    //Запись информации о ком портах SiLabs CP2102 на базе реестрта и CP210xManufacturingDLL
   com_name    : string;        //Имя ком порта в винде, в виде "COMxxx", например "COM7"
   com_path    : string;        //Путь к символьному файлу для открытия устройства ком порта , рекомендуется по данному именю открывать файл
   serial      : string;        //Серийник для данного ком порта, до 16 символов, количество указано в константе CP210x_MAX_SERIAL_STRLEN в CP210xManufacturingDLL
   usb_path    : string;        //Путь к файлу для настройки и считывания параметров как усб устройства, типа дескриптора, серийника, их прошивки и тд, ВНИМАНИЕ! он отличается от com_path
   description : string;        //Дескриптор ком порта на уровне винды, он отличается от дескриптора на уровне настроек, и всегда равен тому что прописано в драйверах винды, на разных виндах по разному, как правило Silicon Labs CP210x USB to UART Bridge
  end;



  tCP2102_reg_list = class //Класс - список доступных в данный момент устройств на базе инфы с реестра
  private
   v_table : array of tCP2102_reg_item;
   v_Log   : tCP210x_log_event;

   procedure log(s:string);
   function  getter(index:integer):tCP2102_reg_item;
   function  get_count:integer;

  public
   constructor create(evLog:tCP210x_log_event = Nil);
   destructor destroy;override;

   property list[index: integer]:tCP2102_reg_item read getter;          //Список подключенных устройств с информацией, см описание записи tCP2102_reg_item
   property count:integer read get_count;                               //Количество записей в списке
   function find_serial(serial:string; var index:integer):boolean;      //Найти по серийному номеру, если True то найдено и индекс в соотв. переменной
  end;



  tCP210x_enum = class          //Класс - список доступных в данный момент устройств на базе инфы с реестра и AN144 + CP210xManufacturingDLL
  private
   v_table : array of tCP210x_enum_item;
   v_Log   : tCP210x_log_event;

   procedure log(s:string);
   function  getter(index:integer):tCP210x_enum_item;
   function  get_count:integer;

  public
   constructor create(evLog:tCP210x_log_event = Nil);
   destructor destroy;override;

   property list[index: integer]:tCP210x_enum_item read getter;
   property count:integer read get_count;
  end;



  tCP210x_config = class //Класс доступа к настройкам устройства на уровне УСБ - серийник, дескриптор и тд, согласно AN144
  private
   handle : thandle;
   v_Log   : tCP210x_log_event;

   procedure log(s:string);

  public
   function  port_present:boolean;

   function  read_part_num:string;           //Считать тип устройства, = "CP2102"
   function  read_VID:string;
   function  read_PID:string;
   function  read_product_string:string;     //Дескриптор устройства на уровне усб а не реестра
   function  read_serial_string:string;      //Серийник, до 16 символов
   function  read_device_version:string;     //Версия устройства, BCD формат, два байта, от 0 до 99 каждый, преобразуется в строку например вида "01.23"
   function  read_lock:string;               //Считать флаг блокировки изменений, если равен "True" то запись невозможна
   function  read_max_power:string;          //Максимальное потребление тока в миллиамперах, число в виде строки напимер "100" - 100мА
   function  read_port_config:tCP2103_PORT_CONFIG;

   function  write_product_string(value:string):boolean; //Процедуры записи. аналогично чтению, так же принимают строковые параметры. если вернут True - ошибка записи.
   function  write_serial_string(value:string):boolean;
   function  write_device_version(value:string):boolean;
   function  write_max_power(value:string):boolean;
   function  write_vid(value:string):boolean;
   function  write_pid(value:string):boolean;
   function  write_port_config(port_config:tCP2103_PORT_CONFIG):boolean;

   procedure Reset; //Сброс устройства, после сброса нужно деинициализировать класс, дальнейшая устройства невозможно т.к. устройство переподключется на усб логически и сменит хандл.

   constructor create(num:cardinal; evLog:tCP210x_log_event = Nil); //Номер устройства - позиция в списке tCP210x_enum согласно AN144
   destructor destroy;override;
  end;

 function CP210x_check_error(log:tCP210x_log_event; error_code:CP210x_STATUS):boolean; //код статуса проверяет на ошибку и если она есть вывыодит в лог и возвращает True, если ошибки нет то False
 function connected_com_ports(evLog:tCP210x_log_event):tstringlist; //Строит общий список всех доступных ком портов в системме, неважно каких ком портов, любые.

implementation

function CP210x_check_error(log:tCP210x_log_event; error_code:CP210x_STATUS):boolean;
begin
 result:=false;
 if error_code=CP210x_SUCCESS then exit;
 case error_code of
  CP210x_DEVICE_NOT_FOUND       : log('Error #'+inttostr(error_code)+' CP210x_DEVICE_NOT_FOUND');
  CP210x_INVALID_HANDLE         : log('Error #'+inttostr(error_code)+' CP210x_INVALID_HANDLE');
  CP210x_INVALID_PARAMETER      : log('Error #'+inttostr(error_code)+' CP210x_INVALID_PARAMETER');
  CP210x_DEVICE_IO_FAILED       : log('Error #'+inttostr(error_code)+' CP210x_DEVICE_IO_FAILED');
  CP210x_FUNCTION_NOT_SUPPORTED : log('Error #'+inttostr(error_code)+' CP210x_FUNCTION_NOT_SUPPORTED');
  CP210x_GLOBAL_DATA_ERROR      : log('Error #'+inttostr(error_code)+' CP210x_GLOBAL_DATA_ERROR');
  CP210x_FILE_ERROR             : log('Error #'+inttostr(error_code)+' CP210x_FILE_ERROR');
  CP210x_COMMAND_FAILED         : log('Error #'+inttostr(error_code)+' CP210x_COMMAND_FAILED');
  CP210x_INVALID_ACCESS_TYPE    : log('Error #'+inttostr(error_code)+' CP210x_INVALID_ACCESS_TYPE');
 else
  log('Error #'+inttostr(error_code));
 end;
 result:=true;
end;

procedure tCP210x_config.log(s:string);
begin
// assert(self<>nil, 'tCP210x_config.log self=nil');
 if @v_Log=nil then exit;
 v_Log(s);
end;

constructor tCP210x_config.create(num:cardinal; evLog:tCP210x_log_event = Nil);
var
 err:CP210x_STATUS;
begin
 v_Log := evLog;

 handle := 0;
 err:=CP210x_Open(num, @handle);
 log('');
 log('CP210x_Open('+inttostr(num)+', @handle), handle='+inttohex(handle, 8));
 if CP210x_check_error(log, err) then fail;
end;

destructor tCP210x_config.destroy;
var
 err:CP210x_STATUS;
begin
 err:=CP210x_Close(handle);
 log('CP210x_Close('+inttohex(handle, 8)+')');
 CP210x_check_error(log, err);
 log('');
end;

function  tCP210x_config.read_part_num:string;
var
 Part_Num:byte;
 err:integer;
begin
 Part_Num:=$FF;
 err:=CP210x_GetPartNumber(handle, @Part_Num);
 log('CP210x_PartNum       = '+'CP210'+inttohex(Part_Num, 1));
 if CP210x_check_error(log, err) or (Part_Num>$F) then
  result:='_ERROR_'
 else
  result:='CP210'+inttohex(Part_Num, 1);
end;

function  tCP210x_config.read_VID:string;
var
 err:integer;
 VID:word;
begin
 VID:=$FF;
 err:=CP210x_GetDeviceVid(handle, @VID);
 log('CP210x_VID           = '+inttohex(VID, 4));
 if CP210x_check_error(log, err) then
  result:='_ERROR_'
 else
  result:=inttohex(VID, 2);
end;

function  tCP210x_config.read_PID:string;
var
 err:integer;
 PID:word;
begin
 PID:=$FF;
 err:=CP210x_GetDevicePid(handle, @PID);
 log('CP210x_PID           = '+inttohex(PID, 4));
 if CP210x_check_error(log, err) then
  result:='_ERROR_'
 else
  result:=inttohex(PID, 2);
end;

function  tCP210x_config.read_product_string:string;
var
 err:integer;
 info:array [0 .. (CP210x_MAX_PRODUCT_STRLEN*2)] of char;
 info_length:byte;
begin
 info_length:=0;
 FillChar(info, sizeof(info), 0);
 err:=CP210x_GetDeviceProductString(handle, info, @info_length);
 log('CP210x_ProductString = "'+info+'"');
 if CP210x_check_error(log, err) then
  result:='_ERROR_'
 else
  result:=info;
end;

function  tCP210x_config.read_serial_string:string;
var
 err:integer;
 info:array [0 .. (CP210x_MAX_SERIAL_STRLEN*2)] of char;
 info_length:byte;
begin
 info_length:=0;
 FillChar(info, sizeof(info), 0);
 err:=CP210x_GetDeviceSerialNumber(handle, info, @info_length);
 log('CP210x_SerialNumber  = "'+info+'"');
 if CP210x_check_error(log, err) then
  result:='_ERROR_'
 else
  result:=info;
end;

function  tCP210x_config.read_device_version:string;
var
 err:integer;
 DeviceVersion:word;
begin
 DeviceVersion:=$AAAA;
 err:=CP210x_GetDeviceVersion(handle, @DeviceVersion);
 log('CP210x_DeviceVersion = '+copy(IntTohex(DeviceVersion,4),1,2)+'.'+copy(IntTohex(DeviceVersion,4),3,2));
 if CP210x_check_error(log, err) then
  result:='_ERROR_'
 else
  result:=copy(IntTohex(DeviceVersion,4),1,2)+'.'+copy(IntTohex(DeviceVersion,4),3,2);
end;

function  tCP210x_config.read_lock:string;
var
 err:integer;
 Lock:byte;
begin
 Lock:=0;
 err:=CP210x_GetLockValue(handle, @Lock);
 log('CP210x_Lock          = '+booltostr(Lock<>0,true));
 if CP210x_check_error(log, err) then
  result:='_ERROR_'
 else
  result:=booltostr(Lock<>0,true);
end;

function  tCP210x_config.read_max_power:string;
var
 err:integer;
 power:byte;
begin
 power:=0;
 err:=CP210x_GetMaxPower(handle, @power);
 log('CP210x_GetMaxPower   = '+inttostr(power*2));
 if CP210x_check_error(log, err) then
  result:='_ERROR_'
 else
  result:=inttostr(power*2);
end;

function  tCP210x_config.port_present:boolean;
var
 name : string;
begin
 name := read_part_num;
 result :=  not ((read_part_num = 'CP2102') or (read_part_num = 'CP2101'));
end;

function  tCP210x_config.read_port_config:tCP2103_PORT_CONFIG;
var
 err : integer;
 port_config : tCP2103_PORT_CONFIG;
begin
 ZeroMemory(@port_config, sizeof(port_config));
 if not port_present then
  begin
   result := port_config;
   exit;
  end;

 err:=CP210x_GetPortConfig(handle, @port_config);
 log('CP210x_GetPortConfig:');
 log(#9'Mode    : ' + IntToHex(port_config.Mode, 4));
 log(#9'Reset   : ' + IntToHex(port_config.Reset_Latch, 4));
 log(#9'Suspend : ' + IntToHex(port_config.Suspend_Latch, 4));
 log(#9'EngFxn  : ' + IntToHex(port_config.EnhancedFxn, 2));

 if CP210x_check_error(log, err) then
  ZeroMemory(@port_config, sizeof(port_config));

 result := port_config;
end;

function  tCP210x_config.write_product_string(value:string):boolean;
var
 err:integer;
 info:array [0 .. CP210x_MAX_PRODUCT_STRLEN] of char;
 info_length:byte;
begin
 FillChar(info, sizeof(info), 0);
 StrPLCopy(info, pchar(value), sizeof(info));
 info_length:=StrLen(info);

 err:=CP210x_SetProductString(handle, info, info_length, TRUE);
 log('CP210x_SetProductString("'+inttohex(handle,8)+', '+info+', '+inttostr(info_length)+', TRUE);');
 if CP210x_check_error(log, err) then
  result:=true
 else
  result:=false;
end;

function  tCP210x_config.write_serial_string(value:string):boolean;
var
 err:integer;
 info:array [0 .. CP210x_MAX_SERIAL_STRLEN] of char;
 info_length:byte;
begin
 FillChar(info, sizeof(info), 0);
 StrPLCopy(info, pchar(value), sizeof(info));
 info_length:=StrLen(info);

 err:=CP210x_SetSerialNumber(handle, info, info_length, TRUE);
 log('CP210x_SetSerialNumber('+inttohex(handle,8)+', "'+info+'", '+inttostr(info_length)+', TRUE);');
 if CP210x_check_error(log, err) then
  result:=true
 else
  result:=false;
end;

function  tCP210x_config.write_device_version(value:string):boolean;
var
 err:integer;
 v1,v2:string;
 b1,b2:byte;
 pos:integer;
 version:word;
begin
 result:=true;
 pos:=system.pos('.', value);
 if pos<2 then exit;

 v1:=copy(value, 1, pos-1);
 v2:=copy(value, pos+1, 255);
 val(v1, b1, err); if err<>0 then exit;
 val(v2, b2, err); if err<>0 then exit;
 if b1>99 then exit;
 if b2>99 then exit;
 b1:=(b1 mod 10) or ((b1 div 10) shl 4);
 b2:=(b2 mod 10) or ((b2 div 10) shl 4);
 version:=b1*256 + b2;

 err:=CP210x_SetDeviceVersion(handle, version);
 log('CP210x_SetDeviceVersion('+inttohex(handle,8)+', '+inttohex(version,4)+');');
 if CP210x_check_error(log, err) then exit;
 result:=false;
end;

function  tCP210x_config.write_max_power(value:string):boolean;
var
 err:integer;
 max_power:integer;
 b:byte;
begin
 result:=true;

 val(value, max_power, err);
 if err<>0 then exit;
 if max_power<50 then max_power:=50;
 if max_power>CP210x_MAX_MAXPOWER then max_power:=CP210x_MAX_MAXPOWER;
 b:=max_power div 2;

 err:=CP210x_SetMaxPower(handle, b);
 log('CP210x_SetMaxPower('+inttohex(handle,8)+', '+inttostr(b)+');');
 if CP210x_check_error(log, err) then exit;
 result:=false;
end;

function  tCP210x_config.write_vid(value:string):boolean;
var
 err:integer;
 w:word;
begin
 result:=true;

 value := '0x'+value;
 try
  w := StrToInt(value);
 except
  exit;
 end;

 err:=CP210x_SetVid(handle, w);
 log('CP210x_SetVid('+inttohex(handle,8)+', '+inttohex(w, 4)+');');
 if CP210x_check_error(log, err) then exit;
 result:=false;
end;

function  tCP210x_config.write_pid(value:string):boolean;
var
 err:integer;
 w:word;
begin
 result:=true;

 value := '0x'+value;
 try
  w := StrToInt(value);
 except
  exit;
 end;

 err:=CP210x_SetPid(handle, w);
 log('CP210x_SetPid('+inttohex(handle,8)+', '+inttohex(w, 4)+');');
 if CP210x_check_error(log, err) then exit;
 result:=false;
end;

function  tCP210x_config.write_port_config(port_config:tCP2103_PORT_CONFIG):boolean;
var
 err:integer;
begin
 result:=true;
 err:=CP210x_SetPortConfig(handle, @port_config);
 log('CP210x_SetPortConfig('+inttohex(handle,8)+', '+
  inttohex(port_config.Mode, 4) + ', '+
  inttohex(port_config.Reset_Latch, 4)+', '+
  inttohex(port_config.Suspend_Latch, 4)+', '+
  inttohex(port_config.EnhancedFxn, 2)+');');
  
 if CP210x_check_error(log, err) then exit;
 result:=false;
end;

procedure tCP210x_config.reset;
var
 err:integer;
begin
 err:=CP210x_Reset(handle);
 log('CP210x_Reset('+inttohex(handle,8)+');');
 CP210x_check_error(log, err);
end;

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

function connected_com_ports(evLog:tCP210x_log_event):tstringlist;

procedure log(s:string);
begin
 if @evLog<>nil then evLog(s);
end;

var
 reg:TRegistry;
 list:tstrings;
 k:integer;                     
 s:string;
const
 path = 'HARDWARE\DEVICEMAP\SERIALCOMM';
begin
 log('');

 result:=TStringlist.Create;
 result.Sorted := true;
 list:=TStringlist.Create;
 reg:=TRegistry.Create;

 reg.RootKey := HKEY_LOCAL_MACHINE;
 reg.Access := KEY_READ;
 if not reg.OpenKey(path, false) then
  begin
   log('Error: Can''t open HKEY_LOCAL_MACHINE\'+path);
   exit;
  end;

 reg.GetValueNames(list);
 if list.Count<>0 then
  log('=== Com port list ===');

 for k:=0 to list.Count-1 do
  begin
   s:=reg.ReadString(list.Strings[k]);
   result.Add(s);
   log(s+#9+list.Strings[k]);
  end;

 if list.Count<>0 then
  log('');
 list.free;
 reg.Free;
end;

procedure tCP210x_enum.log(s:string);
begin
 assert(self<>nil, 'tCP210x_enum.log self=nil');
 if @v_Log=nil then exit;
 v_Log(s);
end;

function  tCP210x_enum.get_count:integer;
begin
 assert(self<>nil, 'tCP210x_enum.log self=nil');
 result := length(v_table);
end;

function  tCP210x_enum.getter(index:integer):tCP210x_enum_item;
begin
 assert(self<>nil, 'tCP210x_enum.getter self=nil');
 assert(index>=0, 'tCP210x_enum.getter index<0');
 assert(index<length(v_table), 'tCP210x_enum.getter index>=length(v_table)');

 result := v_table[index];
end;

destructor tCP210x_enum.destroy;
begin
 SetLength(v_table, 0);
 v_Log := nil;
end;

constructor tCP210x_enum.create(evLog:tCP210x_log_event = Nil);
var
 dev_count:DWORD;
 count:integer;
 err:CP210x_STATUS;
 info:array [0..4096] of char;
 num:integer;
 reg_list : tCP2102_reg_list;
 index : integer;
begin
 v_Log := evLog;
 reg_list := tCP2102_reg_list.create(v_log);

 dev_count:=$FFFFFFFF;
 err:=CP210x_GetNumDevices(@dev_count);
 log('CP210x_GetNumDevices(dev_count), dev_count='+inttohex(dev_count, 8));
 if CP210x_check_error(log, err) then fail;
 log('');
 setlength(v_table, dev_count);

 count:=dev_count;
 for num:=0 to count-1 do
  begin
   log('  === Device #'+inttostr(num)+' ===');

   FillChar(info, sizeof(info), 0);
   err:=CP210x_GetProductString(num, @info, CP210x_RETURN_FULL_PATH);
   CP210x_check_error(log, err);
   log('USB_PATH      = '+info);
   v_table[num].usb_path := info;

   FillChar(info, sizeof(info), 0);
   err:=CP210x_GetProductString(num, @info, CP210x_RETURN_SERIAL_NUMBER);
   CP210x_check_error(log, err);
   log('SERIAL_NUMBER = '+info);
   v_table[num].serial := info;

   FillChar(info, sizeof(info), 0);
   err:=CP210x_GetProductString(num, @info, CP210x_RETURN_DESCRIPTION);
   CP210x_check_error(log, err);
   log('DESCRIPTION   = '+info);
   v_table[num].description := info;

   if reg_list.find_serial(v_table[num].serial, index) then
    begin
     v_table[num].com_name := reg_list.list[index].com_name;
     v_table[num].com_path := reg_list.list[index].path;
     v_table[num].serial   := reg_list.list[index].serial;
    end;

   log('');
  end;
 reg_list.Free;
end;

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

procedure tCP2102_reg_list.log(s:string);
begin
 assert(self<>nil, 'tCP2102_reg_list.log self=nil');
 if @v_Log=nil then exit;
 v_Log(s);
end;

function  tCP2102_reg_list.get_count:integer;
begin
 assert(self<>nil, 'tCP2102_reg_list.log self=nil');
 result := length(v_table);
end;

function  tCP2102_reg_list.getter(index:integer):tCP2102_reg_item;
begin
 assert(self<>nil, 'tCP2102_reg_list.getter self=nil');
 assert(index>=0, 'tCP2102_reg_list.getter index<0');
 assert(index<length(v_table), 'tCP2102_reg_list.getter index>=length(v_table)');

 result := v_table[index];
end;

destructor tCP2102_reg_list.destroy;
begin
 assert(self<>nil, 'tCP2102_reg_list.destroy self=nil');
 SetLength(v_table, 0);
 v_Log := nil;
end;

constructor tCP2102_reg_list.create(evLog:tCP210x_log_event = Nil);
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
var
 reg:TRegistry;
 list:tstrings;
 k:integer;
 index:integer;
 port_serial:string;
 port_name:string;
 port_path:string;
 s:string;
 com_list:tstringlist;

const
 path = 'SYSTEM\CurrentControlSet\Enum\USB\VID_10C4&PID_EA60';

label
 go_end;
begin
 list := nil;
 v_Log := evLog;
 com_list := connected_com_ports(evlog);

 reg:=TRegistry.Create;
 reg.RootKey := HKEY_LOCAL_MACHINE;
 reg.Access := KEY_READ;
 s:=path;
 if not reg.OpenKey(s, false) then
  begin
   log('Error: Can''t open HKEY_LOCAL_MACHINE\'+s);
//   fail;
   goto go_end;
  end;

 SetLength(v_table, 0);
 list:=TStringlist.Create;
 reg.GetKeyNames(list);
 for k:=0 to list.Count-1 do
  begin
   port_serial := list.Strings[k];
   reg.CloseKey;
   if not reg.OpenKey(path+'\'+list.Strings[k]+'\'+'Device Parameters', false) then
    begin
     log('Error: Can''t open HKEY_LOCAL_MACHINE\'+path+'\'+list.Strings[k]+'\'+'Device Parameters');
     Continue;
    end;

   if not reg.ValueExists('PortName') then
    begin
     log('Error: Can''t open value PortName');
     Continue;
    end;

   if reg.ValueExists('SymbolicName') then
    port_path:=reg.ReadString('SymbolicName')
   else
    port_path := '';

   port_name:=reg.ReadString('PortName');

   if com_list.Find(port_name, index) then
    begin
     SetLength(v_table, length(v_table)+1);

     {log('SN# ' + port_serial);
     log(#9' PortName     = '+port_name);
     log(#9' SymbolicName = '+port_path);}

     log('++# ' + ' ['+port_name+'] '#9 + port_serial + #9 + port_path);

     v_table[length(v_table)-1].serial   := port_serial;
     v_table[length(v_table)-1].com_name := port_name;
     v_table[length(v_table)-1].path     := port_path;
    end
   else
    begin
     {log('--# ' + port_serial);
     log(#9' PortName     = '+port_name);
     log(#9' SymbolicName = '+port_path);}

     log('  # ' + ' ['+port_name+'] '#9 + port_serial + #9 + port_path);
    end;
  end;

go_end:
 log('');
 reg.Free;
 if list<>nil then list.Free;
 com_list.Free;
end;

function tCP2102_reg_list.find_serial(serial:string; var index:integer):boolean;
var
 k:integer;
begin
 assert(self<>nil, 'tCP2102_reg_list.find_serial self=nil');

 for k:=0 to length(v_table)-1 do
  if UpperCase(v_table[k].serial) = UpperCase(serial) then
   begin
    index  := k;
    result := true;
    exit;
   end;

 result := false;
 index  := -1;
end;

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

end.
