unit SFU_other;

interface

function body_get_cardinal(var body:pbyte; var count:word):cardinal;
function body_get_word(var body:pbyte; var count:word):word;
procedure body_get_block(var body:pbyte; var count:word; block_ptr:pbyte; block_size:integer);
function body_get_byte(var body:pbyte; var count:word):byte;

function stm32_load_file(file_name:string; data:pointer; max_size:cardinal; readed:pcardinal) : boolean;

implementation
uses sysutils;

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

//////////////////////////////////////////////////////////////////////////////////////////

function body_get_byte(var body:pbyte; var count:word):byte;
begin
 result := 0;
 if count < 1 then exit;
 if body = nil then exit;
 result := body^;
 inc(body);
 dec(count);
end;

procedure body_get_block(var body:pbyte; var count:word; block_ptr:pbyte; block_size:integer);
begin
 while (block_size > 0) do
  begin
   block_ptr^ := body_get_byte(body, count);
   inc(block_ptr);
   dec(block_size);
  end;
end;

function body_get_word(var body:pbyte; var count:word):word;
begin
 result := 0;
 result := result or word(body_get_byte(body, count)) shl 0;
 result := result or word(body_get_byte(body, count)) shl 8;
end;

function body_get_cardinal(var body:pbyte; var count:word):cardinal;
begin
 result := 0;
 result := result or cardinal(body_get_word(body, count)) shl  0;
 result := result or cardinal(body_get_word(body, count)) shl 16;
end;



end.
 