unit SLABHIDtoUART;

interface
uses windows, classes;

type
 HID_UART_STATUS = integer;

/////////////////////////////////////////////////////////////////////////////
// Return Code Definitions
/////////////////////////////////////////////////////////////////////////////

const
// Return Codes
 HID_UART_SUCCESS		 = $00;
 HID_UART_DEVICE_NOT_FOUND	 = $01;
 HID_UART_INVALID_HANDLE	 = $02;
 HID_UART_INVALID_DEVICE_OBJECT	 = $03;
 HID_UART_INVALID_PARAMETER	 = $04;
 HID_UART_INVALID_REQUEST_LENGTH = $05;

 HID_UART_READ_ERROR		 = $10;
 HID_UART_WRITE_ERROR		 = $11;
 HID_UART_READ_TIMED_OUT	 = $12;
 HID_UART_WRITE_TIMED_OUT	 = $13;
 HID_UART_DEVICE_IO_FAILED	 = $14;
 HID_UART_DEVICE_ACCESS_ERROR	 = $15;
 HID_UART_DEVICE_NOT_SUPPORTED	 = $16;
 HID_UART_INVALID_CONFIG_VERSION = $17;

 HID_UART_UNKNOWN_ERROR		 = $FF;

// Product String Types
 HID_UART_GET_VID_STR		 = $01;
 HID_UART_GET_PID_STR		 = $02;
 HID_UART_GET_PATH_STR		 = $03;
 HID_UART_GET_SERIAL_STR	 = $04;
 HID_UART_GET_MANUFACTURER_STR	 = $05;
 HID_UART_GET_PRODUCT_STR	 = $06;

// String Lengths
 HID_UART_DEVICE_STRLEN		 = 260;

type
 HID_UART_DEVICE_STR = array [0 .. HID_UART_DEVICE_STRLEN - 1] of ansichar;

const
/////////////////////////////////////////////////////////////////////////////
// UART Definitions
/////////////////////////////////////////////////////////////////////////////

// Error Status
 HID_UART_PARITY_ERROR		= $01;
 HID_UART_OVERRUN_ERROR		= $02;

// Line Break Status
 HID_UART_LINE_BREAK_INACTIVE	= $00;
 HID_UART_LINE_BREAK_ACTIVE	= $01;

// Data Bits
 HID_UART_FIVE_DATA_BITS	= $00;
 HID_UART_SIX_DATA_BITS		= $01;
 HID_UART_SEVEN_DATA_BITS	= $02;
 HID_UART_EIGHT_DATA_BITS	= $03;

// Parity
 HID_UART_NO_PARITY		= $00;
 HID_UART_ODD_PARITY		= $01;
 HID_UART_EVEN_PARITY		= $02;
 HID_UART_MARK_PARITY		= $03;
 HID_UART_SPACE_PARITY		= $04;

// Stop Bits
// Short = 1 stop bit
// Long  = 1.5 stop bits (5 data bits)
//       = 2 stop bits (6-8 data bits)
 HID_UART_SHORT_STOP_BIT	= $00;
 HID_UART_LONG_STOP_BIT		= $01;

// Flow Control
 HID_UART_NO_FLOW_CONTROL	= $00;
 HID_UART_RTS_CTS_FLOW_CONTROL	= $01;

// Read/Write Limits
 HID_UART_MIN_READ_SIZE		= 1;
 HID_UART_MAX_READ_SIZE		= 32768;
 HID_UART_MIN_WRITE_SIZE	= 1;
 HID_UART_MAX_WRITE_SIZE	= 4096;

/////////////////////////////////////////////////////////////////////////////
// Part Number Definitions
/////////////////////////////////////////////////////////////////////////////

// Part Numbers
 HID_UART_PART_CP2110		= $0A;
 HID_UART_PART_CP2114		= $0E;

/////////////////////////////////////////////////////////////////////////////
// User Customization Definitions
/////////////////////////////////////////////////////////////////////////////

// User-Customizable Field Lock Bitmasks
 HID_UART_LOCK_PRODUCT_STR_1	= $0001;
 HID_UART_LOCK_PRODUCT_STR_2	= $0002;
 HID_UART_LOCK_SERIAL_STR	= $0004;
 HID_UART_LOCK_PIN_CONFIG	= $0008;
 HID_UART_LOCK_VID		= $0100;
 HID_UART_LOCK_PID		= $0200;
 HID_UART_LOCK_POWER		= $0400;
 HID_UART_LOCK_POWER_MODE	= $0800;
 HID_UART_LOCK_RELEASE_VERSION	= $1000;
 HID_UART_LOCK_FLUSH_BUFFERS	= $2000;
 HID_UART_LOCK_MFG_STR_1	= $4000;
 HID_UART_LOCK_MFG_STR_2	= $8000;

// Field Lock Bit Values
 HID_UART_LOCK_UNLOCKED		= 1;
 HID_UART_LOCK_LOCKED		= 0;

// Power Max Value (500 mA)
 HID_UART_BUS_POWER_MAX		= $FA;

// Power Modes
 HID_UART_BUS_POWER		= $00;
 HID_UART_SELF_POWER_VREG_DIS	= $01;
 HID_UART_SELF_POWER_VREG_EN	= $02;

// Flush Buffers Bitmasks
 HID_UART_FLUSH_TX_OPEN		= $01;
 HID_UART_FLUSH_TX_CLOSE	= $02;
 HID_UART_FLUSH_RX_OPEN		= $04;
 HID_UART_FLUSH_RX_CLOSE	= $08;

// USB Config Bitmasks
 HID_UART_SET_VID		= $01;
 HID_UART_SET_PID		= $02;
 HID_UART_SET_POWER		= $04;
 HID_UART_SET_POWER_MODE	= $08;
 HID_UART_SET_RELEASE_VERSION	= $10;
 HID_UART_SET_FLUSH_BUFFERS	= $20;

// USB Config Bit Values
 HID_UART_SET_IGNORE		= 0;
 HID_UART_SET_PROGRAM		= 1;

// String Lengths
 HID_UART_MFG_STRLEN		= 62;
 HID_UART_PRODUCT_STRLEN	= 62;
 HID_UART_SERIAL_STRLEN		= 30;

type
 HID_UART_MFG_STR     = array [0 .. HID_UART_MFG_STRLEN - 1] of ansichar;  // HID_UART_MFG_STR
 HID_UART_PRODUCT_STR = array [0 .. HID_UART_PRODUCT_STRLEN - 1] of ansichar; // HID_UART_PRODUCT_STR
 HID_UART_SERIAL_STR  = array [0 .. HID_UART_SERIAL_STRLEN - 1] of ansichar; // HID_UART_PRODUCT_STR

const
/////////////////////////////////////////////////////////////////////////////
// Pin Definitions
/////////////////////////////////////////////////////////////////////////////

// Pin Config Modes
 HID_UART_GPIO_MODE_INPUT	= $00;
 HID_UART_GPIO_MODE_OUTPUT_OD	= $01;
 HID_UART_GPIO_MODE_OUTPUT_PP	= $02;
 HID_UART_GPIO_MODE_FUNCTION1	= $03;
 HID_UART_GPIO_MODE_FUNCTION2	= $04;

// Suspend Value Bit Values
 HID_UART_VALUE_SUSPEND_LO	= 0;
 HID_UART_VALUE_SUSPEND_HI	= 1;

// Suspend Mode Bit Values
 HID_UART_MODE_SUSPEND_OD	= 0;
 HID_UART_MODE_SUSPEND_PP	= 1;

// RS485 Active Levels
 HID_UART_MODE_RS485_ACTIVE_LO	= $00;
 HID_UART_MODE_RS485_ACTIVE_HI	= $01;

type
 HID_UART_DEVICE = pointer;
 pHID_UART_DEVICE = ^HID_UART_DEVICE;


function HidUart_GetNumDevices(var numDevices:DWORD; vid:WORD; pid:WORD):HID_UART_STATUS; stdcall;
function HidUart_GetString(deviceNum:DWORD; vid:WORD; pid:WORD; deviceString:PAnsiChar; options:DWORD):HID_UART_STATUS; stdcall;
function HidUart_GetOpenedString(device:HID_UART_DEVICE; deviceString:PAnsiChar; options:DWORD):HID_UART_STATUS; stdcall;
function HidUart_GetIndexedString(deviceNum:DWORD; vid:WORD; pid:WORD; stringIndex:DWORD; deviceString:PAnsiChar):HID_UART_STATUS; stdcall;
function HidUart_GetOpenedIndexedString(device:HID_UART_DEVICE; stringIndex:DWORD; deviceString:PAnsiChar):HID_UART_STATUS; stdcall;
function HidUart_GetAttributes(deviceNum:DWORD; vid:WORD; pid:WORD ; deviceVid:PWORD; devicePid:PWORD; deviceReleaseNumber:PWORD):HID_UART_STATUS; stdcall;
function HidUart_GetOpenedAttributes(device:HID_UART_DEVICE; deviceVid:PWORD; devicePid:PWORD; deviceReleaseNumber:PWORD):HID_UART_STATUS; stdcall;
function HidUart_Open(device:pHID_UART_DEVICE; deviceNum:DWORD; vid:WORD; pid:WORD):HID_UART_STATUS; stdcall;
function HidUart_Close(device:HID_UART_DEVICE):HID_UART_STATUS; stdcall;
function HidUart_IsOpened(device:HID_UART_DEVICE; opened:PBOOL):HID_UART_STATUS; stdcall;
function HidUart_SetUartEnable(device:HID_UART_DEVICE; enable:BOOL):HID_UART_STATUS; stdcall;
function HidUart_GetUartEnable(device:HID_UART_DEVICE; enable:PBOOL):HID_UART_STATUS; stdcall;
function HidUart_Read(device:HID_UART_DEVICE; buffer:PByte; numBytesToRead:DWORD; numBytesRead:pDWORD):HID_UART_STATUS; stdcall;
function HidUart_Write(device:HID_UART_DEVICE; buffer:PByte; numBytesToWrite:DWORD; numBytesWritten:PDWORD):HID_UART_STATUS; stdcall;
function HidUart_FlushBuffers(device:HID_UART_DEVICE; flushTransmit:BOOL; flushReceive:BOOL):HID_UART_STATUS; stdcall;
function HidUart_CancelIo(device:HID_UART_DEVICE):HID_UART_STATUS; stdcall;
function HidUart_SetTimeouts(device:HID_UART_DEVICE; readTimeout:DWORD; writeTimeout:DWORD):HID_UART_STATUS; stdcall;
function HidUart_GetTimeouts(device:HID_UART_DEVICE; readTimeout:PDWORD; writeTimeout:PDWORD):HID_UART_STATUS; stdcall;
function HidUart_GetUartStatus(device:HID_UART_DEVICE; transmitFifoSize:PWORD; receiveFifoSize:PWORD; errorStatus:PByte; lineBreakStatus:PByte):HID_UART_STATUS; stdcall;
function HidUart_SetUartConfig(device:HID_UART_DEVICE; baudRate:DWORD; dataBits:BYTE; parity:BYTE; stopBits:BYTE; flowControl:BYTE):HID_UART_STATUS; stdcall;
function HidUart_GetUartConfig(device:HID_UART_DEVICE; baudRate:PDWORD; dataBits:PByte; parity:PByte; stopBits:PByte; flowControl:PByte):HID_UART_STATUS; stdcall;
function HidUart_StartBreak(device:HID_UART_DEVICE; duration:BYTE):HID_UART_STATUS; stdcall;
function HidUart_StopBreak(device:HID_UART_DEVICE):HID_UART_STATUS; stdcall;
function HidUart_Reset(device:HID_UART_DEVICE):HID_UART_STATUS; stdcall;
function HidUart_ReadLatch(device:HID_UART_DEVICE; latchValue:PWORD):HID_UART_STATUS; stdcall;
function HidUart_WriteLatch(device:HID_UART_DEVICE; latchValue:WORD; latchMask:WORD):HID_UART_STATUS; stdcall;
function HidUart_GetPartNumber(device:HID_UART_DEVICE; partNumber:PByte; version:PByte):HID_UART_STATUS; stdcall;
function HidUart_GetLibraryVersion(major:PByte; minor:PByte; release:PBOOL):HID_UART_STATUS; stdcall;
function HidUart_GetHidLibraryVersion( major:PByte;  minor:PByte;  release:PBOOL):HID_UART_STATUS; stdcall;
function HidUart_GetHidGuid(guid:pointer):HID_UART_STATUS; stdcall;
function HidUart_SetLock(device:HID_UART_DEVICE; lock:WORD):HID_UART_STATUS; stdcall;
function HidUart_GetLock(device:HID_UART_DEVICE; lock:PWORD):HID_UART_STATUS; stdcall;
function HidUart_SetUsbConfig(device:HID_UART_DEVICE; vid:WORD; pid:WORD; power:BYTE; powerMode:BYTE; releaseVersion:WORD; flushBuffers:BYTE; mask:BYTE):HID_UART_STATUS; stdcall;
function HidUart_GetUsbConfig(device:HID_UART_DEVICE; vid:PWORD; pid:PWORD; power:PByte; powerMode:PByte; releaseVersion:PWORD; flushBuffers:PByte):HID_UART_STATUS; stdcall;
function HidUart_SetManufacturingString(device:HID_UART_DEVICE; manufacturingString:PAnsiChar; strlen:BYTE):HID_UART_STATUS; stdcall;
function HidUart_GetManufacturingString(device:HID_UART_DEVICE; manufacturingString:PAnsiChar; strlen:pBYTE):HID_UART_STATUS; stdcall;
function HidUart_SetProductString(device:HID_UART_DEVICE; productString:PAnsiChar; strlen:BYTE ):HID_UART_STATUS; stdcall;
function HidUart_GetProductString(device:HID_UART_DEVICE; productString:PAnsiChar; strlen:pBYTE):HID_UART_STATUS; stdcall;
function HidUart_SetSerialString(device:HID_UART_DEVICE; serialString:PAnsiChar; strlen:BYTE):HID_UART_STATUS; stdcall;
function HidUart_GetSerialString(device:HID_UART_DEVICE; serialString:PAnsiChar; strlen:pBYTE):HID_UART_STATUS; stdcall;


function hid_status_format(status : integer):string;

type
 thidlog_func = procedure(str:string) of object;

function HidUart_ALL_serials(log_func:thidlog_func):tstringlist;
function HidUart_open_serial(serial:string; baudRate:DWORD; parity:Byte; log_func:thidlog_func):HID_UART_DEVICE;

function is_cp2114(name:string):boolean;

const
 HidUart_NAME_PERFIX = 'CP2114::';

implementation
uses sysutils;

const
  D2XXDLL = 'SLABHIDtoUART.dll';

function HidUart_GetNumDevices           ;external D2XXDLL name 'HidUart_GetNumDevices';
function HidUart_GetString               ;external D2XXDLL name 'HidUart_GetString';
function HidUart_GetOpenedString         ;external D2XXDLL name 'HidUart_GetOpenedString';
function HidUart_GetIndexedString        ;external D2XXDLL name 'HidUart_GetIndexedString';
function HidUart_GetOpenedIndexedString  ;external D2XXDLL name 'HidUart_GetOpenedIndexedString';
function HidUart_GetAttributes           ;external D2XXDLL name 'HidUart_GetAttributes';
function HidUart_GetOpenedAttributes     ;external D2XXDLL name 'HidUart_GetOpenedAttributes';
function HidUart_Open                    ;external D2XXDLL name 'HidUart_Open';
function HidUart_Close                   ;external D2XXDLL name 'HidUart_Close';
function HidUart_IsOpened                ;external D2XXDLL name 'HidUart_IsOpened';
function HidUart_SetUartEnable           ;external D2XXDLL name 'HidUart_SetUartEnable';
function HidUart_GetUartEnable           ;external D2XXDLL name 'HidUart_GetUartEnable';
function HidUart_Read                    ;external D2XXDLL name 'HidUart_Read';
function HidUart_Write                   ;external D2XXDLL name 'HidUart_Write';
function HidUart_FlushBuffers            ;external D2XXDLL name 'HidUart_FlushBuffers';
function HidUart_CancelIo                ;external D2XXDLL name 'HidUart_CancelIo';
function HidUart_SetTimeouts             ;external D2XXDLL name 'HidUart_SetTimeouts';
function HidUart_GetTimeouts             ;external D2XXDLL name 'HidUart_GetTimeouts';
function HidUart_GetUartStatus           ;external D2XXDLL name 'HidUart_GetUartStatus';
function HidUart_SetUartConfig           ;external D2XXDLL name 'HidUart_SetUartConfig';
function HidUart_GetUartConfig           ;external D2XXDLL name 'HidUart_GetUartConfig';
function HidUart_StartBreak              ;external D2XXDLL name 'HidUart_StartBreak';
function HidUart_StopBreak               ;external D2XXDLL name 'HidUart_StopBreak';
function HidUart_Reset                   ;external D2XXDLL name 'HidUart_Reset';
function HidUart_ReadLatch               ;external D2XXDLL name 'HidUart_ReadLatch';
function HidUart_WriteLatch              ;external D2XXDLL name 'HidUart_WriteLatch';
function HidUart_GetPartNumber           ;external D2XXDLL name 'HidUart_GetPartNumber';
function HidUart_GetLibraryVersion       ;external D2XXDLL name 'HidUart_GetLibraryVersion';
function HidUart_GetHidLibraryVersion    ;external D2XXDLL name 'HidUart_GetHidLibraryVersion';
function HidUart_GetHidGuid              ;external D2XXDLL name 'HidUart_GetHidGuid';
function HidUart_SetLock                 ;external D2XXDLL name 'HidUart_SetLock';
function HidUart_GetLock                 ;external D2XXDLL name 'HidUart_GetLock';
function HidUart_SetUsbConfig            ;external D2XXDLL name 'HidUart_SetUsbConfig';
function HidUart_GetUsbConfig            ;external D2XXDLL name 'HidUart_GetUsbConfig';
function HidUart_SetManufacturingString  ;external D2XXDLL name 'HidUart_SetManufacturingString';
function HidUart_GetManufacturingString  ;external D2XXDLL name 'HidUart_GetManufacturingString';
function HidUart_SetProductString        ;external D2XXDLL name 'HidUart_SetProductString';
function HidUart_GetProductString        ;external D2XXDLL name 'HidUart_GetProductString';
function HidUart_SetSerialString         ;external D2XXDLL name 'HidUart_SetSerialString';
function HidUart_GetSerialString         ;external D2XXDLL name 'HidUart_GetSerialString';

function hid_status_format(status : integer):string;
begin
 result := '????';
 if status = HID_UART_SUCCESS                 then result := 'HID_UART_SUCCESS               ';
 if status = HID_UART_DEVICE_NOT_FOUND        then result := 'HID_UART_DEVICE_NOT_FOUND      ';
 if status = HID_UART_INVALID_HANDLE          then result := 'HID_UART_INVALID_HANDLE        ';
 if status = HID_UART_INVALID_DEVICE_OBJECT   then result := 'HID_UART_INVALID_DEVICE_OBJECT ';
 if status = HID_UART_INVALID_PARAMETER       then result := 'HID_UART_INVALID_PARAMETER     ';
 if status = HID_UART_INVALID_REQUEST_LENGTH  then result := 'HID_UART_INVALID_REQUEST_LENGTH';

 if status = HID_UART_READ_ERROR              then result := 'HID_UART_READ_ERROR            ';
 if status = HID_UART_WRITE_ERROR             then result := 'HID_UART_WRITE_ERROR           ';
 if status = HID_UART_READ_TIMED_OUT          then result := 'HID_UART_READ_TIMED_OUT        ';
 if status = HID_UART_WRITE_TIMED_OUT         then result := 'HID_UART_WRITE_TIMED_OUT       ';
 if status = HID_UART_DEVICE_IO_FAILED        then result := 'HID_UART_DEVICE_IO_FAILED      ';
 if status = HID_UART_DEVICE_ACCESS_ERROR     then result := 'HID_UART_DEVICE_ACCESS_ERROR   ';
 if status = HID_UART_DEVICE_NOT_SUPPORTED    then result := 'HID_UART_DEVICE_NOT_SUPPORTED  ';
 if status = HID_UART_INVALID_CONFIG_VERSION  then result := 'HID_UART_INVALID_CONFIG_VERSION';

 if status = HID_UART_UNKNOWN_ERROR           then result := 'HID_UART_UNKNOWN_ERROR         ';
end;

function HidUart_ALL_serials(log_func:thidlog_func):tstringlist;
procedure log(str:string); begin if @log_func<>nil then log_func(str); end;
var
 res : HID_UART_STATUS;
 numDevices : DWORD;
 i : integer;
 buf : array [0 .. 4095] of ansichar;
 serial : string;
begin
 result := TStringList.create;
 res := HidUart_GetNumDevices(numDevices, $10C4, $EAB0);
 log('HidUart_GetNumDevices(numDevices, $10C4, $EAB0); // ' + hid_status_format(res));
 log(#9'numDevices = ' + inttostr(numDevices));

 for i := 0 to integer(numDevices) - 1 do
  begin
   ZeroMemory(@buf[0], length(buf));
   res := HidUart_GetString(i, $10C4, $EAB0, buf, HID_UART_GET_PRODUCT_STR);
   log('HidUart_GetString(' + inttostr(i) + ', $10C4, $EAB0, buf, HID_UART_GET_PRODUCT_STR); // ' + hid_status_format(res));
   log(#9'buf = ' + buf);

   ZeroMemory(@buf[0], length(buf));
   res := HidUart_GetString(i, $10C4, $EAB0, buf, HID_UART_GET_SERIAL_STR);
   log('HidUart_GetString(' + inttostr(i) + ', $10C4, $EAB0, buf, HID_UART_GET_SERIAL_STR); // ' + hid_status_format(res));
   log(#9'buf = ' + buf);
   serial := buf;
   serial := HidUart_NAME_PERFIX + serial;

   if res = HID_UART_SUCCESS then
    result.Add(serial);

   ZeroMemory(@buf[0], length(buf));
   res := HidUart_GetString(i, $10C4, $EAB0, buf, HID_UART_GET_PATH_STR);
   log('HidUart_GetString(' + inttostr(i) + ', $10C4, $EAB0, buf, HID_UART_GET_PATH_STR); // ' + hid_status_format(res));
   log(#9'buf = ' + buf);

   ZeroMemory(@buf[0], length(buf));
   res := HidUart_GetString(i, $10C4, $EAB0, buf, HID_UART_GET_MANUFACTURER_STR);
   log('HidUart_GetString(' + inttostr(i) + ', $10C4, $EAB0, buf, HID_UART_GET_MANUFACTURER_STR); // ' + hid_status_format(res));
   log(#9'buf = ' + buf);

   log(' ');
  end;
end;

function HidUart_open_serial(serial:string; baudRate:DWORD; parity:Byte; log_func:thidlog_func):HID_UART_DEVICE;
procedure log(str:string); begin if @log_func<>nil then log_func(str); end;
var
 res : HID_UART_STATUS;
 numDevices : DWORD;
 open_pos : integer;
 i : integer;
 buf : array [0 .. 4095] of ansichar;
 device : HID_UART_DEVICE;
 b : BOOL;
 str : string;
const
 dataBits:Byte = HID_UART_EIGHT_DATA_BITS;
 stopBits:Byte = HID_UART_LONG_STOP_BIT;
 flowControl:Byte = HID_UART_NO_FLOW_CONTROL;
begin
 result := nil;
 res := HidUart_GetNumDevices(numDevices, $10C4, $EAB0);
 log('HidUart_GetNumDevices(numDevices, $10C4, $EAB0); // ' + hid_status_format(res));
 log(#9'numDevices = ' + inttostr(numDevices));

 open_pos := -1;
 for i := 0 to integer(numDevices) - 1 do
  begin
   ZeroMemory(@buf[0], length(buf));
   res := HidUart_GetString(i, $10C4, $EAB0, buf, HID_UART_GET_PRODUCT_STR);
   log('HidUart_GetString(' + inttostr(i) + ', $10C4, $EAB0, buf, HID_UART_GET_PRODUCT_STR); // ' + hid_status_format(res));
   log(#9'buf = ' + buf);

   ZeroMemory(@buf[0], length(buf));
   res := HidUart_GetString(i, $10C4, $EAB0, buf, HID_UART_GET_SERIAL_STR);
   log('HidUart_GetString(' + inttostr(i) + ', $10C4, $EAB0, buf, HID_UART_GET_SERIAL_STR); // ' + hid_status_format(res));
   log(#9'buf = ' + buf);

   str := buf;
   str := HidUart_NAME_PERFIX + str;

   if res = HID_UART_SUCCESS then
    if str = serial then
     begin
      open_pos := i;
      break;
     end;


   ZeroMemory(@buf[0], length(buf));
   res := HidUart_GetString(i, $10C4, $EAB0, buf, HID_UART_GET_PATH_STR);
   log('HidUart_GetString(' + inttostr(i) + ', $10C4, $EAB0, buf, HID_UART_GET_PATH_STR); // ' + hid_status_format(res));
   log(#9'buf = ' + buf);

   ZeroMemory(@buf[0], length(buf));
   res := HidUart_GetString(i, $10C4, $EAB0, buf, HID_UART_GET_MANUFACTURER_STR);
   log('HidUart_GetString(' + inttostr(i) + ', $10C4, $EAB0, buf, HID_UART_GET_MANUFACTURER_STR); // ' + hid_status_format(res));
   log(#9'buf = ' + buf);

   log(' ');
  end;

 if open_pos < 0 then
  exit;

 device := nil;
 res := HidUart_Open(@device, open_pos, $10C4, $EAB0);
 log('HidUart_Open(@device, ' + inttostr(open_pos) + ', $10C4, $EAB0); // ' + hid_status_format(res));
 log(#9'device = ' + inttohex(cardinal(device), 8));
 log(' ');
 if res <> HID_UART_SUCCESS then
  exit;

 res := HidUart_GetUartEnable(device, @b);
 log('HidUart_GetUartEnable(device, @b); // ' + hid_status_format(res));
 log(#9'b = ' + BoolToStr(b, true));

 if b <> true then
  begin
   res := HidUart_SetUartEnable(device, true);
   log('HidUart_SetUartEnable(device, true); // ' + hid_status_format(res));
  end;
 log(' ');

 res := HidUart_SetUartConfig(device, baudRate, databits, parity, stopbits, flowControl);
 log('HidUart_SetUartConfig(device, @boundrate, @databits, @parity, @stopbits, @flowconrol); // ' + hid_status_format(res));
 log(#9'baudRate = ' + inttostr(baudRate));
 log(#9'dataBits = ' + inttostr(dataBits));
 log(#9'parity   = ' + inttostr(parity));
 log(#9'stopBits = ' + inttostr(stopBits));
 log(#9'flowControl = ' + inttostr(flowControl));
 log(' ');

 res := HidUart_SetTimeouts(Device, 0, 500);
 log('HidUart_SetTimeouts(Device, 10, 500); // ' + hid_status_format(res));
 log(' ');

 result := device;
end;

function is_cp2114(name:string):boolean;
begin
 result:=pos(HidUart_NAME_PERFIX, name)=1;
end;

end.
