unit u_stm32_id;

interface
uses windows;

type
 tSTM32_info =packed record
  flash_size : word;
  reserved_a : word;
  reserved_b : dword;
  cpu_id_a   : word;
  cpu_id_b   : word;
  cpu_id_c   : dword;
  cpu_id_d   : dword;
 end;

function stmid2hash(id:tSTM32_info):int64;
function hash2checking(hash:int64):cardinal;

implementation

function hash2checking(hash:int64):cardinal;
var
 value : cardinal;

 a : array[0..15] of char;
 v : int64;
 k : byte;
begin
 for k:=0 to 15 do
  begin
   if hash and 15>=10 then
    a[15-k] := chr(ord('A')+(hash and 15)-10)
   else
    a[15-k] := chr(ord('0')+(hash and 15));
   hash:=hash shr 4;
  end;

 v:=0;
 for k:=0 to sizeof(a)-1 do
  begin
   v:=v xor (ord(a[k])+k);
   v:=(v shl 17) or (v shr (64-17));
  end;

 v:=v mod 999974651;
 value := cardinal(v);
 result := value;
end;

function stmid2hash(id:tSTM32_info):int64;
var
 v : int64;
 a : int64;
 b : int64;
 c : int64;
 d : int64;
begin
 v := 0;
 inc(v, int64(id.cpu_id_a) shl 32);
 inc(v, int64(id.cpu_id_c) shl  0);
 inc(v, int64(id.cpu_id_d) shl 16);
 inc(v, int64(id.cpu_id_b) shl (32+16));

 a := 0;
 inc(a, int64(id.cpu_id_d)*int64(id.cpu_id_c));
 dec(a, int64(id.cpu_id_a)*int64(id.cpu_id_b));

// result := v xor a xor (v shr 16) xor (a shl 16);
// exit;

// v := v xor a;
 v := v xor a xor (v shr 16) xor (a shl 16);
 a := v mod 983756381;
 b := v mod 193857371;
 c := v mod 72912456;
 d := v mod 777777777;

 inc(v, (a shl 32) xor (d shl (64-16)));
 inc(v, (b shl  0) xor (d shr 16));
 inc(v, (c shl 16));

 result := v mod 999999936854775808;
end;

end.
