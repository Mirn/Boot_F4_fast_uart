unit CP210xManufacturingDLL;
interface
uses
Windows;

type
 CP210x_STATUS=integer;

const
 //GetProductString() function flags
 CP210x_RETURN_SERIAL_NUMBER			=$00;
 CP210x_RETURN_DESCRIPTION			=$01;
 CP210x_RETURN_FULL_PATH			=$02;

 //GetDeviceVersion() return codes
 CP210x_CP2101_VERSION			=$01;
 CP210x_CP2102_VERSION			=$02;
 CP210x_CP2103_VERSION			=$03;
 CP210x_CP2104_VERSION			=$04;
 CP210x_CP2105_VERSION			=$05;
											
 //Return codes
 CP210x_SUCCESS				=$00;
 CP210x_DEVICE_NOT_FOUND		=$FF;
 CP210x_INVALID_HANDLE			=$01;
 CP210x_INVALID_PARAMETER		=$02;
 CP210x_DEVICE_IO_FAILED		=$03;
 CP210x_FUNCTION_NOT_SUPPORTED		=$04;
 CP210x_GLOBAL_DATA_ERROR		=$05;
 CP210x_FILE_ERROR			=$06;
 CP210x_COMMAND_FAILED			=$08;
 CP210x_INVALID_ACCESS_TYPE		=$09;

// Buffer size limits
//
// CP2101/2/3/4
 CP210x_MAX_DEVICE_STRLEN		=256;
 CP210x_MAX_PRODUCT_STRLEN		=126;
 CP210x_MAX_SERIAL_STRLEN		=63;
 CP210x_MAX_MAXPOWER			=250;
//
// CP2105
 CP2105_MAX_PRODUCT_STRLEN		=47;
 CP2105_MAX_INTERFACE_STRLEN		=32;
 CP2105_MAX_SERIAL_STRLEN		=16;

type
CP210x_DEVICE_STRING=array[0..CP210x_MAX_DEVICE_STRLEN-1] of char;
CP210x_PRODUCT_STRING=array[0..CP210x_MAX_PRODUCT_STRLEN-1] of char;
CP210x_SERIAL_STRING=array[0..CP210x_MAX_SERIAL_STRLEN-1] of char;
CP210x_INTERFACE_STRING=array[0..CP2105_MAX_INTERFACE_STRLEN-1] of char;

const
// Baud Rate Aliasing definitions
 NUM_BAUD_CONFIGS	=32;

type
  pBAUD_CONFIG=^BAUD_CONFIG;
  BAUD_CONFIG=record
   BaudGen:WORD;
   Timer0Reload:WORD;
   Prescaler:BYTE;
   BaudRate:DWORD;
  end;

const
// Size of the Baud Config Structure
 BAUD_CONFIG_SIZE	=10;

// Array of all Baud Rate Configurations
type
 BAUD_CONFIG_DATA=array[0..NUM_BAUD_CONFIGS-1] of BAUD_CONFIG;

const
// Flush Buffer definitions
//
// CP2104
 FC_OPEN_TX		=$01;	// When these bits are set, the device will flush that buffer
 FC_OPEN_RX		=$02;
 FC_CLOSE_TX		=$04;
 FC_CLOSE_RX		=$08;
//
// CP2105 - Standard Port
 FC_OPEN_TX_SCI  = FC_OPEN_TX;
 FC_OPEN_RX_SCI  = FC_OPEN_RX;
 FC_CLOSE_TX_SCI = FC_CLOSE_TX;
 FC_CLOSE_RX_SCI = FC_CLOSE_RX;
//
// CP2105 - Enhanced Port
 FC_OPEN_TX_ECI  =$10;
 FC_OPEN_RX_ECI  =$20;
 FC_CLOSE_TX_ECI =$40;
 FC_CLOSE_RX_ECI =$80;

type
//Port Config definitions
//
// CP2103/4 Port Config Structure
 pCP2103_PORT_CONFIG = ^tCP2103_PORT_CONFIG;
 tCP2103_PORT_CONFIG = packed record
	Mode:WORD;		// Push-Pull = 1, Open-Drain = 0
	Reset_Latch   : WORD;	// Logic High = 1, Logic Low = =0
        Suspend_Latch : WORD;	// Logic High = 1, Logic Low = =0
	EnhancedFxn   : byte;
 end;

const
// Define bit locations for Mode/Latch for Reset and Suspend structures
 PORT_RI_ON				=$0001;
 PORT_DCD_ON				=$0002;
 PORT_DTR_ON				=$0004;
 PORT_DSR_ON				=$0008;
 PORT_TXD_ON				=$0010;
 PORT_RXD_ON				=$0020;
 PORT_RTS_ON				=$0040;
 PORT_CTS_ON				=$0080;
//
 PORT_GPIO_0_ON			        =$0100;
 PORT_GPIO_1_ON			        =$0200;
 PORT_GPIO_2_ON			        =$0400;
 PORT_GPIO_3_ON			        =$0800;
//
 PORT_SUSPEND_ON			=$4000;	//  Can't configure latch value
 PORT_SUSPEND_BAR_ON		        =$8000;	//  Can't configure latch value
//
// Define bit locations for EnhancedFxn
 EF_GPIO_0_TXLED			=$01;	//  Under device control
 EF_GPIO_1_RXLED			=$02;	//  Under device control
 EF_GPIO_2_RS485			=$04;	//  Under device control
 //EF_RS485_INVERT			=$08;	//  RS485 Invert bit
 EF_WEAKPULLUP				=$10;	//  Weak Pull-up on
 EF_RESERVED_1				=$20;	//	Reserved, leave bit 5 cleared
 EF_SERIAL_DYNAMIC_SUSPEND	        =$40;	//  For 8 UART/Modem signals
 EF_GPIO_DYNAMIC_SUSPEND		=$80;	//  For 4 GPIO signals

type // CP2105 Dual Port Config Structure
 pDUAL_PORT_CONFIG = ^tDUAL_PORT_CONFIG;
 tDUAL_PORT_CONFIG = packed record
	Mode:word;			// Push-Pull = 1, Open-Drain = 0
	Reset_Latch:word;	// Logic High = 1, Logic Low = =0
	Suspend_Latch:word;	// Logic High = 1, Logic Low = =0
	EnhancedFxn_ECI:byte;
	EnhancedFxn_SCI:byte;
	EnhancedFxn_Device:byte;
 end;

const
// CP2105 Define bit locations for Mode/Latch for Reset and Suspend structures
 PORT_RI_SCI_ON			=$0001;
 PORT_DCD_SCI_ON		=$0002;
 PORT_DTR_SCI_ON		=$0004;
 PORT_DSR_SCI_ON		=$0008;
 PORT_TXD_SCI_ON		=$0010;
 PORT_RXD_SCI_ON		=$0020;
 PORT_RTS_SCI_ON		=$0040;
 PORT_CTS_SCI_ON		=$0080;
 PORT_GPIO_0_SCI_ON		=$0002;
 PORT_GPIO_1_SCI_ON		=$0004;
 PORT_GPIO_2_SCI_ON		=$0008;
 PORT_SUSPEND_SCI_ON		=$0001;	//  Can't configure latch value
//
 PORT_RI_ECI_ON			=$0100;
 PORT_DCD_ECI_ON		=$0200;
 PORT_DTR_ECI_ON		=$0400;
 PORT_DSR_ECI_ON		=$0800;
 PORT_TXD_ECI_ON		=$1000;
 PORT_RXD_ECI_ON		=$2000;
 PORT_RTS_ECI_ON		=$4000;
 PORT_CTS_ECI_ON		=$8000;
 PORT_GPIO_0_ECI_ON		=$0400;
 PORT_GPIO_1_ECI_ON		=$0800;
 PORT_SUSPEND_ECI_ON		=$0100;	//  Can't configure latch value
//
// CP2105 Define bit locations for EnhancedFxn_ECI
 EF_GPIO_0_TXLED_ECI		=$01;	//  Under device control
 EF_GPIO_1_RXLED_ECI		=$02;	//  Under device control
 EF_GPIO_1_RS485_ECI		=$04;	//  Under device control
 EF_RS485_INVERT		=$08;	//  Under device control
 EF_INVERT_SUSPEND_ECI		=$10;	//  RS485 Invert bit
 EF_DYNAMIC_SUSPEND_ECI		=$40;	//  For GPIO signals
//
// CP2105 Define bit locations for EnhancedFxn_SCI
 EF_GPIO_0_TXLED_SCI		=$01;	//  Under device control
 EF_GPIO_1_RXLED_SCI		=$02;	//  Under device control
 EF_INVERT_SUSPEND_SCI		=$10;	//  RS485 Invert bit
 EF_DYNAMIC_SUSPEND_SCI		=$40;	//  For GPIO signals
//
// CP2105 Define bit locations for EnhancedFxn_Device
//EF_WEAKPULLUP			=$10;	//  Weak Pull-up on
//



function CP210x_GetNumDevices(lpdwNumDevices:pcardinal):CP210x_STATUS; stdcall;
function CP210x_GetProductString(dwDeviceNum:cardinal; lpvDeviceString:pchar; dwFlags:cardinal):CP210x_STATUS; stdcall;
function CP210x_Open(dwDevice:dword; HANDLE:phandle):CP210x_STATUS; stdcall;
function CP210x_Close(cyHandle:thandle):CP210x_STATUS; stdcall;

function CP210x_GetPartNumber(
	cyHandle:thandle;
	lpbPartNum:pbyte
	):CP210x_STATUS; stdcall;

function CP210x_SetVid(
	cyHandle:thandle;
	wVid:word
	):CP210x_STATUS; stdcall;

function CP210x_SetPid(
	cyHandle:thandle;
	wPid:word
	):CP210x_STATUS; stdcall;

function CP210x_SetProductString(
	cyHandle:thandle;
	lpvProduct:pchar;
	bLength:byte;
	bConvertToUnicode:bool = FALSE
	):CP210x_STATUS; stdcall;

function CP210x_SetInterfaceString(
	cyHandle:thandle;
	bInterfaceNumber:BYTE;
	lpvInterface:pchar;
	bLength:BYTE;
	bConvertToUnicode:BOOL = FALSE
	):CP210x_STATUS; stdcall;

function CP210x_SetSerialNumber(
	cyHandle:thandle;
	lpvSerialNumber:pchar;
	bLength:BYTE;
	bConvertToUnicode:BOOL = FALSE
	):CP210x_STATUS; stdcall;

function CP210x_SetSelfPower(
	cyHandle:thandle; 
	bSelfPower:BOOL
	):CP210x_STATUS; stdcall;

function CP210x_SetMaxPower(
	cyHandle:thandle;
	bMaxPower:BYTE
	):CP210x_STATUS; stdcall;

function CP210x_SetFlushBufferConfig(
	cyHandle:thandle;
	bFlushBufferConfig:BYTE
	):CP210x_STATUS; stdcall;

function CP210x_SetDeviceMode(
	cyHandle:thandle;
	bDeviceModeECI:BYTE;
	bDeviceModeSCI:BYTE
	):CP210x_STATUS; stdcall;

function CP210x_SetDeviceVersion(
	cyHandle:thandle;
	wVersion:WORD
	):CP210x_STATUS; stdcall;

function CP210x_SetBaudRateConfig(
	cyHandle:thandle;
	baudConfigData:pBAUD_CONFIG
	):CP210x_STATUS; stdcall;

function CP210x_SetPortConfig(
	cyHandle:thandle;
	PortConfig:pCP2103_PORT_CONFIG
	):CP210x_STATUS; stdcall;

function CP210x_SetDualPortConfig(	
	cyHandle:thandle;
	DualPortConfig:pDUAL_PORT_CONFIG
	):CP210x_STATUS; stdcall;

function CP210x_SetLockValue(
	cyHandle:thandle
	):CP210x_STATUS; stdcall;

function CP210x_GetDeviceVid(
	cyHandle:thandle;
	lpwVid:pword
	):CP210x_STATUS; stdcall;

function CP210x_GetDevicePid(
	cyHandle:thandle;
	lpwPid:pword
	):CP210x_STATUS; stdcall;

function CP210x_GetDeviceProductString(	
	cyHandle:thandle;
	lpProduct:pchar;
	lpbLength:pbyte;
	bConvertToASCII:BOOL = TRUE
	):CP210x_STATUS; stdcall;

function CP210x_GetDeviceInterfaceString(
	cyHandle:thandle;
	bInterfaceNumber:BYTE;
	lpInterface:pchar;
	lpbLength:pbyte;
	bConvertToASCII:BOOL = TRUE
	):CP210x_STATUS; stdcall;

function CP210x_GetDeviceSerialNumber(	
	cyHandle:thandle;
	lpSerialNumber:pchar;
	lpbLength:pbyte;
	bConvertToASCII:BOOL = TRUE
	):CP210x_STATUS; stdcall;

function CP210x_GetSelfPower(
	cyHandle:thandle;
	lpbSelfPower:PBOOL
	):CP210x_STATUS; stdcall;

function CP210x_GetMaxPower(
	cyHandle:thandle;
	lpbPower:pbyte
	):CP210x_STATUS; stdcall;

function CP210x_GetFlushBufferConfig(
	cyHandle:thandle;
	lpbFlushBufferConfig:pointer
	):CP210x_STATUS; stdcall;

function CP210x_GetDeviceMode(
	cyHandle:thandle;
	lpbDeviceModeECI:pbyte;
	lpbDeviceModeSCI:pbyte
	):CP210x_STATUS; stdcall;

function CP210x_GetDeviceVersion(
	cyHandle:thandle;
	lpwVersion:pword
	):CP210x_STATUS; stdcall;

function CP210x_GetBaudRateConfig(
	cyHandle:thandle;
	baudConfigData:pBAUD_CONFIG
	):CP210x_STATUS; stdcall;


function CP210x_GetPortConfig(
	cyHandle:thandle;
	PortConfig:pCP2103_PORT_CONFIG
	):CP210x_STATUS; stdcall;

function CP210x_GetDualPortConfig(	
	cyHandle:thandle;
	DualPortConfig:pDUAL_PORT_CONFIG
	):CP210x_STATUS; stdcall;

function CP210x_GetLockValue(	
	cyHandle:thandle;
	lpbLockValue:pbyte
	):CP210x_STATUS; stdcall;

function CP210x_Reset(
	cyHandle:thandle
	):CP210x_STATUS; stdcall;

function CP210x_CreateHexFile(	
	cyHandle:thandle;
	lpvFileName:pchar
	):CP210x_STATUS; stdcall;

function CP210x_GetConfig(
        Handle:thandle;
        lpbConfig:PBYTE;
        bLength:integer
        ):CP210x_STATUS; stdcall;

function CP210x_SetConfig(
        Handle:thandle;
        lpbConfig:PBYTE;
        bLength:integer
        ):CP210x_STATUS; stdcall;


implementation

const
  D2XXDLL = 'CP210xManufacturing.dll';


function CP210x_GetNumDevices;    external D2XXDLL name 'CP210x_GetNumDevices';
function CP210x_GetProductString; external D2XXDLL name 'CP210x_GetProductString';
function CP210x_Open; external D2XXDLL name 'CP210x_Open';
function CP210x_Close; external D2XXDLL name 'CP210x_Close';
function CP210x_GetPartNumber; external D2XXDLL name 'CP210x_GetPartNumber';
function CP210x_SetVid; external D2XXDLL name 'CP210x_SetVid';
function CP210x_SetPid; external D2XXDLL name 'CP210x_SetPid';
function CP210x_SetProductString; external D2XXDLL name 'CP210x_SetProductString';
function CP210x_SetInterfaceString; external D2XXDLL name 'CP210x_SetInterfaceString';
function CP210x_SetSerialNumber; external D2XXDLL name 'CP210x_SetSerialNumber';
function CP210x_SetSelfPower; external D2XXDLL name 'CP210x_SetSelfPower';
function CP210x_SetMaxPower; external D2XXDLL name 'CP210x_SetMaxPower';
function CP210x_SetFlushBufferConfig; external D2XXDLL name 'CP210x_SetFlushBufferConfig';
function CP210x_SetDeviceMode; external D2XXDLL name 'CP210x_SetDeviceMode';
function CP210x_SetDeviceVersion; external D2XXDLL name 'CP210x_SetDeviceVersion';
function CP210x_SetBaudRateConfig; external D2XXDLL name 'CP210x_SetBaudRateConfig';
function CP210x_SetPortConfig; external D2XXDLL name 'CP210x_SetPortConfig';
function CP210x_SetDualPortConfig; external D2XXDLL name 'CP210x_SetDualPortConfig';
function CP210x_SetLockValue; external D2XXDLL name 'CP210x_SetLockValue';
function CP210x_GetDeviceVid; external D2XXDLL name 'CP210x_GetDeviceVid';
function CP210x_GetDevicePid; external D2XXDLL name 'CP210x_GetDevicePid';
function CP210x_GetDeviceProductString; external D2XXDLL name 'CP210x_GetDeviceProductString';
function CP210x_GetDeviceInterfaceString; external D2XXDLL name 'CP210x_GetDeviceInterfaceString';
function CP210x_GetDeviceSerialNumber; external D2XXDLL name 'CP210x_GetDeviceSerialNumber';
function CP210x_GetSelfPower; external D2XXDLL name 'CP210x_GetSelfPower';
function CP210x_GetMaxPower; external D2XXDLL name 'CP210x_GetMaxPower';
function CP210x_GetFlushBufferConfig; external D2XXDLL name 'CP210x_GetFlushBufferConfig';
function CP210x_GetDeviceMode; external D2XXDLL name 'CP210x_GetDeviceMode';
function CP210x_GetDeviceVersion; external D2XXDLL name 'CP210x_GetDeviceVersion';
function CP210x_GetBaudRateConfig; external D2XXDLL name 'CP210x_GetBaudRateConfig';
function CP210x_GetPortConfig; external D2XXDLL name 'CP210x_GetPortConfig';
function CP210x_GetDualPortConfig; external D2XXDLL name 'CP210x_GetDualPortConfig';
function CP210x_GetLockValue; external D2XXDLL name 'CP210x_GetLockValue';
function CP210x_Reset; external D2XXDLL name 'CP210x_Reset';
function CP210x_CreateHexFile; external D2XXDLL name 'CP210x_CreateHexFile';
function CP210x_GetConfig; external D2XXDLL name 'CP210x_GetConfig';
function CP210x_SetConfig; external D2XXDLL name 'CP210x_SetConfig';

end.