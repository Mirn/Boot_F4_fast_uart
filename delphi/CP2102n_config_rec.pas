unit CP2102n_config_rec;

interface

type

 tCP2102n_config_rec = packed record
  configSize	                        : word;
  configVersion	                        : BYTE;
  enableBootloader	                : BYTE;
  enableConfigUpdate	                : BYTE;
  deviceDesc_bLength	                : BYTE;
  deviceDesc_bDescriptorType	        : BYTE;
  deviceDesc_bcdUSB	                : word;
  deviceDesc_bDeviceClass	        : BYTE;
  deviceDesc_bDeviceSubClass	        : BYTE;
  deviceDesc_bDeviceProtocol	        : BYTE;
  deviceDesc_bMaxPacketSize0	        : BYTE;
  cp210x_base_set_ids_VID	        : word;
  cp210x_base_set_ids_PID	        : word;
  cp210x_base_set_ids_releaseVersion	: word;
  deviceDesc_iManufacturer	        : BYTE;
  deviceDesc_iProduct	                : BYTE;
  deviceDesc_iSerialNumber	        : BYTE;
  deviceDesc_bNumConfigurations	        : BYTE;
  configDesc_bLength	                : BYTE;
  configDesc_bDescriptorType	        : BYTE;
  configDesc_wTotalLength	        : word;
  configDesc_bNumInterfaces	        : BYTE;
  configDesc_bConfigurationValue	: BYTE;
  configDesc_iConfiguration	        : BYTE;
  configDesc_bmAttributes	        : BYTE;
  cp210x_base_set_ids_maxPower_real	: BYTE;
  interfaceDescriptor_bLength	        : BYTE;
  interfaceDescriptor_bDescriptorType	: BYTE;
  interfaceDescriptor_bInterfaceNumber	: BYTE;
  interfaceDescriptor_bAlternateSetting	: BYTE;
  interfaceDescriptor_bNumEndpoints	: BYTE;
  interfaceDescriptor_bInterfaceClass	: BYTE;
  interfaceDescriptor_bInterfaceSubClass: BYTE;
  interfaceDescriptor_bInterfaceProtocol: BYTE;
  interfaceDescriptor_iInterface	: BYTE;
  bulkOut_bLength	                : BYTE;
  bulkOut_bDescriptorType	        : BYTE;
  bulkOut_bEndpointAddress	        : BYTE;
  bulkOut_bmAttributes	                : BYTE;
  bulkOut_wMaxPacketSize	        : word;
  bulkOut_bInterval	                : BYTE;
  bulkIn_bLength	                : BYTE;
  bulkIn_bDescriptorType	        : BYTE;
  bulkIn_bEndpointAddress	        : BYTE;
  bulkIn_bmAttributes	                : BYTE;
  bulkIn_wMaxPacketSize	                : word;
  bulkIn_bInterval	                : BYTE;
  langDesc_langDesc0	                : word;
  langDesc_langDesc1	                : word;
  BIT_PADDING_0	                        : BYTE;
  mfrDesc_mfrDescLen	                : BYTE;
  USB_STRING_DESCRIPTOR_0	        : BYTE;
  cp210x_base_set_ids_manufacturerString: array[0..128-1] of byte;
  BIT_PADDING_1	                        : BYTE;
  prodDesc_prodDescLen	                : BYTE;
  USB_STRING_DESCRIPTOR_1	        : BYTE;
  cp210x_base_set_ids_productString	: array[0..256-1] of byte;
  useInternalSerial	                : BYTE;
  serDesc_serDescLen	                : BYTE;
  USB_STRING_DESCRIPTOR_2	        : BYTE;
  cp210x_base_set_ids_serialString	: array[0..128-1] of byte;
  ResetModeP0	                        : BYTE;
  ResetModeP1	                        : BYTE;
  ResetModeP2	                        : BYTE;
  ResetLowPowerP0	                : BYTE;
  ResetLowPowerP1	                : BYTE;
  ResetLowPowerP2	                : BYTE;
  ResetLatchP0	                        : BYTE;
  ResetLatchP1	                        : BYTE;
  ResetLatchP2	                        : BYTE;
  SuspendModeP0	                        : BYTE;
  SuspendModeP1	                        : BYTE;
  SuspendModeP2	                        : BYTE;
  SuspendLowPowerP0	                : BYTE;
  SuspendLowPowerP1	                : BYTE;
  SuspendLowPowerP2	                : BYTE;
  SuspendLatchP0	                : BYTE;
  SuspendLatchP1	                : BYTE;
  SuspendLatchP2	                : BYTE;
  portSettings_control_driveStrength	: BYTE;
  GPIOControl0	                        : BYTE;
  GPIOControl1	                        : BYTE;
  GPIOControl2	                        : BYTE;
  FlushBuffersConfig	                : BYTE;
  commProp_PacketLength	                : word;
  commProp_PacketVersion	        : word;
  commProp_ServiceMask	                : Cardinal;
  commProp_Reserved1	                : Cardinal;
  commProp_MaxTxQueue	                : Cardinal;
  commProp_MaxRxQueue	                : Cardinal;
  commProp_MaxBaud	                : Cardinal;
  commProp_ProvSubType	                : Cardinal;
  commProp_ProvCapabilities	        : Cardinal;
  commProp_SettableParams	        : Cardinal;
  SettableBaud	                        : Cardinal;
  settableData0	                        : word;
  settableData1	                        : word;
  commProp_CurrentTxQueue	        : Cardinal;
  commProp_CurrentRxQueue	        : Cardinal;
  commProp_ProvSpec1	                : Cardinal;
  commProp_ProvSpec2	                : Cardinal;
  commProp_ProvChar_ProvChar0	        : BYTE;
  commProp_ProvChar_ProvChar1	        : BYTE;
  commProp_ProvChar_ProvChar2	        : BYTE;
  commProp_ProvChar_ProvChar3	        : BYTE;
  commProp_ProvChar_ProvChar4	        : BYTE;
  commProp_ProvChar_ProvChar5	        : BYTE;
  rs485Setup	                        : word;
  rs485Hold	                        : word;
  flowOn	                        : BYTE;
  flowOff	                        : BYTE;
  clockDivider	                        : BYTE;
  FLETCHER_CHECKSUM	                : word;
end;

 tCP2102n_config = class
 public
  rec : tCP2102n_config_rec;
  constructor create;
  function load_from_file(fn : string) : boolean;
  function calc_crc : word;
  procedure update_crc;

  function read_serial:WideString;
  function read_product:WideString;
  function read_manufacture:WideString;

  procedure set_serial(str : widestring);
  procedure set_product(str : widestring);
  procedure set_manufacture(str : widestring);
 end;

function fletcher16(dataIn:pbyte; bytes:word; swap:boolean = false) : word;
function bytes2str(data:pbyte; count:Cardinal):ansistring;

implementation

uses sysutils, classes, windows, math;

function fletcher16(dataIn:pbyte; bytes:word; swap:boolean = false) : word;
var
 sum1 : word;
 sum2 : word;
 tlen : word;
begin
 sum1 := $ff;
 sum2 := $ff;
 tlen := 0;

        while (bytes > 0) do
         begin
          tlen := Min(20, bytes);
          dec(bytes, tlen);

          repeat
           inc(sum1, dataIn^);
           inc(sum2, sum1);
           inc(dataIn);
           dec(tlen);
          until (tlen = 0);

          sum1 := (sum1 and $ff) + (sum1 shr 8);
          sum2 := (sum2 and $ff) + (sum2 shr 8);
         end;

        sum1 := (sum1 and $ff) + (sum1 shr 8);
        sum2 := (sum2 and $ff) + (sum2 shr 8);
        result := (sum2 shl 8) or sum1;
        if (swap) then
         result := ((result and $00FF) shl 8) or ((result and $FF00) shr 8);
end;

function bytes2str(data:pbyte; count:Cardinal):ansistring;
begin
 result := '';
 while count > 0 do
  begin
   if data^ >= 32 then
    result := result + ansichar(data^)
   else
    result := result + '_0x'+inttohex(data^, 2) + '_';
   dec(count);
   inc(data);
  end;
end;

//////////////////////////////////////////////////////////////////////////////

constructor tCP2102n_config.create;
begin
 ZeroMemory(@rec, sizeof(rec));
end;

function tCP2102n_config.read_serial:WideString;
var
 buf : array[0..1024] of byte;
begin
 ZeroMemory(@buf, sizeof(buf));
 move(rec.cp210x_base_set_ids_serialString[0], buf, rec.serDesc_serDescLen);
 result := pwidechar(@buf[0]);
end;

function tCP2102n_config.read_product:WideString;
var
 buf : array[0..1024] of byte;
begin
 ZeroMemory(@buf, sizeof(buf));
 move(rec.cp210x_base_set_ids_productString[0], buf, rec.prodDesc_prodDescLen);
 result := pwidechar(@buf[0]);
end;

function tCP2102n_config.read_manufacture:WideString;
var
 buf : array[0..1024] of byte;
begin
 ZeroMemory(@buf, sizeof(buf));
 move(rec.cp210x_base_set_ids_manufacturerString[0], buf, rec.mfrDesc_mfrDescLen);
 result := pwidechar(@buf[0]);
end;

procedure tCP2102n_config.set_serial(str : widestring);
begin
 if Length(str) <= 0 then exit;
 if (Length(str) + 1) > sizeof(rec.cp210x_base_set_ids_serialString) then exit;

 ZeroMemory(@rec.cp210x_base_set_ids_serialString, sizeof(rec.cp210x_base_set_ids_serialString));
 move(str[1], rec.cp210x_base_set_ids_serialString, (Length(str) + 1) * 2);
 rec.serDesc_serDescLen := (Length(str) + 1) * 2;
end;

procedure tCP2102n_config.set_product(str : widestring);
begin
 if Length(str) <= 0 then exit;
 if (Length(str) + 1) > sizeof(rec.cp210x_base_set_ids_productString) then exit;

 ZeroMemory(@rec.cp210x_base_set_ids_productString, sizeof(rec.cp210x_base_set_ids_productString));
 move(str[1], rec.cp210x_base_set_ids_productString, (Length(str) + 1) * 2);
 rec.prodDesc_prodDescLen := (Length(str) + 1) * 2;
end;

procedure tCP2102n_config.set_manufacture(str : widestring);
begin
 if Length(str) <= 0 then exit;
 if (Length(str) + 1) > sizeof(rec.cp210x_base_set_ids_manufacturerString) then exit;

 ZeroMemory(@rec.cp210x_base_set_ids_manufacturerString, sizeof(rec.cp210x_base_set_ids_manufacturerString));
 move(str[1], rec.cp210x_base_set_ids_manufacturerString, (Length(str) + 1) * 2);
 rec.mfrDesc_mfrDescLen := (Length(str) + 1) * 2;
end;

function tCP2102n_config.load_from_file(fn : string) : boolean;
var
  Stream: TFileStream;
begin
 result := false;

 try
 Stream := TFileStream.Create(fn, fmOpenRead or fmShareDenyWrite);
 try
   Stream.Read(rec, SizeOf(rec));
   result := true;
 finally
  Stream.Free;
 end;
 finally
 end;
end;

function tCP2102n_config.calc_crc:word;
begin
 result := fletcher16(@rec, sizeof(rec) - sizeof(rec.FLETCHER_CHECKSUM), true);
end;

procedure tCP2102n_config.update_crc;
var
 newcrc : word;
begin
 rec.FLETCHER_CHECKSUM := $FFFF;
 rec.FLETCHER_CHECKSUM := self.calc_crc;
end;

end.
