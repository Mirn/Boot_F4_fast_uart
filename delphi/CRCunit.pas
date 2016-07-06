unit CRCunit;
{$R-}
{$Q-}
{$O-}

interface

function CRCGet(StPtr: pointer; StLen: integer): integer;
function crc_stm32(data:pcardinal; length:integer):cardinal;
function crc_stm32_sum16(data:pcardinal; length:integer):word;

implementation
var
  CRCtable: array[0..255] of cardinal;

function GetNewCRC(OldCRC: cardinal; StPtr: pointer; StLen: integer): cardinal;
asm
  test edx,edx;
  jz @ret;
  neg ecx;
  jz @ret;
  sub edx,ecx; // Address after last element

  push ebx;
  mov ebx,0; // Set ebx=0 & align @next
@next:
  mov bl,al;
  xor bl,byte [edx+ecx];
  shr eax,8;
  xor eax,cardinal [CRCtable+ebx*4];
  inc ecx;
  jnz @next;
  pop ebx;

@ret:
end;

function CRCGet(StPtr: pointer; StLen: integer): integer;
var
 c:cardinal;
 i:integer absolute c;
begin
  c := not GetNewCRC($FFFFFFFF, StPtr, StLen);
  result := i;
end;

procedure CRCInit;
var
  c: cardinal;
  i, j: integer;
begin
  for i := 0 to 255 do
  begin
    c := i;
    for j := 1 to 8 do
      if odd(c) then
        c := (c shr 1) xor  $EDB88320
      else
        c := (c shr 1);
    CRCtable[i] := c;
  end;
end;

function crc_dword(crc_e:cardinal; data:cardinal):cardinal;
var
 pos : integer;
begin
 crc_e := crc_e xor data;
 for pos:=0 to 31 do
  if (crc_e and $80000000) <> 0 then
   crc_e := (crc_e shl 1) xor $4C11DB7
  else
   crc_e := (crc_e shl 1);
 result := crc_e;
end;

function crc_stm32(data:pcardinal; length:integer):cardinal;
var
 crc_e : cardinal;
begin
 crc_e := $FFFFFFFF;
 while length > 0 do
  begin
   crc_e := crc_dword(crc_e, data^);
   inc(data);
   dec(length);
  end;
 result := crc_e;
end;

function crc_stm32_sum16(data:pcardinal; length:integer):word;
var
 crc : cardinal;
begin
 crc := crc_stm32(data, length);
 result := ((crc and $0000FFFF) shr  0) +
           ((crc and $FFFF0000) shr 16) +
           0;
end;

initialization
  CRCinit;
end.
