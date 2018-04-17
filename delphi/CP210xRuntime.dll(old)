unit CP210xRuntimeDLL;
interface
uses
Windows;

type
 CP210x_STATUS = integer;

const
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
 CP210x_MAX_PRODUCT_STRLEN		=126;
 CP210x_MAX_SERIAL_STRLEN		=63;

type
 CP210x_PRODUCT_STRING   = array[0..CP210x_MAX_PRODUCT_STRLEN-1] of char;
 CP210x_SERIAL_STRING    = array[0..CP210x_MAX_SERIAL_STRLEN-1] of char;

const
 CP210x_GPIO_0 = $01;
 CP210x_GPIO_1 = $02;
 CP210x_GPIO_2 = $04;
 CP210x_GPIO_3 = $08;


function CP210xRT_ReadLatch(cyHandle : thandle;
                    lpbLatch : pbyte
	):CP210x_STATUS; stdcall;

function CP210xRT_WriteLatch(cyHandle : thandle;
                    bMask  : byte;
                    bLatch : byte
	):CP210x_STATUS; stdcall;

function CP210xRT_GetPartNumber(cyHandle : thandle;
                       lpbPartNum : pbyte
	):CP210x_STATUS; stdcall;

function CP210xRT_GetDeviceProductString(cyHandle : thandle;
                                lpProduct : pointer;
                                lpbLength : pbyte;
                                bConvertToASCII : longbool = TRUE
	):CP210x_STATUS; stdcall;

function CP210xRT_GetDeviceSerialNumber(cyHandle : thandle;
                               lpSerialNumber : pointer;
                               lpbLength : pbyte;
                               bConvertToASCII : longbool = TRUE
	):CP210x_STATUS; stdcall;

function CP210xRT_GetDeviceInterfaceString(cyHandle : thandle;
                                  lpInterfaceString : pointer;
                                  lpbLength : pbyte;
                                  bConvertToASCII : longbool = TRUE
	):CP210x_STATUS; stdcall;

implementation

const
  D2XXDLL = 'CP210xRuntime.dll';


function CP210xRT_ReadLatch;                 external D2XXDLL name 'CP210xRT_ReadLatch';
function CP210xRT_WriteLatch;                external D2XXDLL name 'CP210xRT_WriteLatch';
function CP210xRT_GetPartNumber;             external D2XXDLL name 'CP210xRT_GetPartNumber';
function CP210xRT_GetDeviceProductString;    external D2XXDLL name 'CP210xRT_GetDeviceProductString';
function CP210xRT_GetDeviceSerialNumber;     external D2XXDLL name 'CP210xRT_GetDeviceSerialNumber';
function CP210xRT_GetDeviceInterfaceString;  external D2XXDLL name 'CP210xRT_GetDeviceInterfaceString';

end.