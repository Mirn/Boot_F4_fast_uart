unit uFIFO_async;

interface

uses windows, sysutils, math,
 uFIFO_rec;

{$Q-}
{$R-}

type
  tFIFO_async=class
  private
   fifo_buffer : array of byte;

   crit_section_read :_RTL_CRITICAL_SECTION;
   crit_section_write :_RTL_CRITICAL_SECTION;

   function get_size : integer;
   function get_free : integer;
   function get_count : integer;

   function get_stat : tFIFO_rec_stat;

  protected
   fifo : pFIFO_rec;
   fixed_size : integer;
   fifo_data_ptr : pointer;
   string_read : string;
   string_next : boolean;


   procedure delete_objects;

   procedure read_begin;virtual;
   procedure read_end;virtual;
   procedure write_begin; virtual;
   procedure write_end;virtual;

  public
   constructor create(v_size : integer);
   destructor destroy;override;

   procedure write(buffer:pointer; buf_size:integer);
   procedure read(buffer:pointer; buf_size:integer);
   procedure read_void(buf_size:integer);

   procedure write_string(str:string);
   function  read_string(var str:string) : boolean;

   procedure reset;
   procedure reset_stats;
   procedure reset_stats_exchange(var v_stats:tFIFO_rec_stat);

   function  ratio_free:integer;
   function  ratio_count:integer;

   property  bytes_size  : integer read get_size;
   property  bytes_count : integer read get_count;
   property  bytes_free  : integer read get_free;

   property stat : tFIFO_rec_stat read get_stat;
  end;


implementation

uses classes;

procedure _inf(var v);
begin
end;

constructor tFIFO_async.create(v_size : integer);
begin
 inherited create();

 new(fifo);
 _FIFO_rec_init(fifo^, v_size, fixed_size);
 setlength(fifo_buffer, fixed_size);
 fifo_data_ptr := @(fifo_buffer[0]);

 InitializeCriticalSection(crit_section_read);
 InitializeCriticalSection(crit_section_write);
end;

destructor tFIFO_async.destroy;
begin
 delete_objects;
 DeleteCriticalSection(crit_section_read);
 DeleteCriticalSection(crit_section_write);
 inherited destroy;
end;

procedure tFIFO_async.delete_objects;
begin
 setlength(fifo_buffer, 0);
 fifo_buffer := nil;
 fifo_data_ptr := nil;

 if fifo <> nil then
  dispose(fifo);
 fifo := nil;
end;

procedure tFIFO_async.reset;
begin
 if self=nil then exit;
 if fifo = nil then exit;
 _FIFO_rec_reset(fifo^);
end;

function  tFIFO_async.get_count;
begin
 result:=0;
 if self = nil then exit;
 if fifo = nil then exit;
 result := _FIFO_rec_count(fifo^, fixed_size);
end;

function  tFIFO_async.get_free;
begin
 result := 0;
 if self = nil then exit;
 if fifo = nil then exit;
 result := _FIFO_rec_free(fifo^, fixed_size);
end;

procedure tFIFO_async.write(buffer:pointer; buf_size:integer);
begin
 if self=nil then exit;
 if fifo = nil then exit;
 if fifo_data_ptr = nil then exit;
 if buffer = nil then exit;
 if bytes_size = 0 then exit;

 write_begin;
 _FIFO_rec_write(fifo^, fifo_data_ptr, fixed_size, buffer, buf_size);
 write_end;
end;

procedure tFIFO_async.read(buffer:pointer;buf_size:integer);
begin
 if self=nil then exit;
 if fifo = nil then exit;
 if fifo_data_ptr = nil then exit;
 if buffer = nil then exit;
 if bytes_size = 0 then exit;

 read_begin;
 _FIFO_rec_read(fifo^, fifo_data_ptr, fixed_size, buffer, buf_size);
 read_end;
end;

procedure tFIFO_async.read_void(buf_size:integer);
begin
 if self=nil then exit;
 if bytes_size = 0 then exit;
 if fifo = nil then exit;

 read_begin;
 _FIFO_rec_read_void(fifo^, fixed_size, buf_size);
 read_end;
end;

procedure tFIFO_async.reset_stats;
begin
 if self = nil then exit;
 if fifo = nil then exit;

 _FIFO_rec_stat_reset(fifo^);
end;

procedure tFIFO_async.reset_stats_exchange(var v_stats:tFIFO_rec_stat);
begin
 if self = nil then exit;
 if fifo = nil then exit;

 v_stats := fifo^.stat;
 _FIFO_rec_stat_reset(fifo^);
end;

function  tFIFO_async.ratio_count:integer;
begin
 result := 0;
 if self = nil then exit;
 if fifo = nil then exit;
 result:=_FIFO_rec_count_ratio(fifo^, fixed_size);
end;

function  tFIFO_async.ratio_free:integer;
begin
 result := 0;
 if self = nil then exit;
 if fifo = nil then exit;
 result:=_FIFO_rec_free_ratio(fifo^, fixed_size);
end;

function tFIFO_async.get_stat : tFIFO_rec_stat;
begin
 fillchar(result, sizeof(result), 0);
 if self = nil then exit;
 if fifo = nil then exit;
 result := fifo^.stat;
end;

function tFIFO_async.get_size : integer;
begin
 result := 0;
 if self = nil then exit;
 if fifo = nil then exit;
 result := fixed_size;
end;

procedure tFIFO_async.write_string(str:string);
var
 bytes : integer;
begin
 if fifo = nil then exit;
 str := str + #13;
 string_next := true;
 bytes := length(str)*sizeof(str[1]);

 if self.bytes_free < bytes*3 then
  inc(fifo^.stat.writed.errors, bytes)
 else
  self.write(@str[1], bytes);
end;

function  tFIFO_async.read_string(var str:string) : boolean;
var
 buf : array [0..4095] of char;
 cnt : integer;
 pos : integer;
begin
 result := false;
 str := '';

 if string_next then
  begin
   cnt := min(sizeof(buf)-1, self.bytes_count);
   if cnt <> 0 then
    begin
     fillchar(buf, sizeof(buf), 0);
     self.read(@buf[0], cnt);

     cnt := cnt div sizeof(buf[0]);
     pos := length(string_read) + 1;
     setlength(string_read, length(string_read) + cnt);
     move(buf[0], string_read[pos], cnt*sizeof(char));
    end
   else
    exit;
  end;

 pos := 1;
 cnt := length(string_read);
 while cnt > 0 do
  begin
   if string_read[pos] in [#0, #13] then
    break;

   inc(pos);
   dec(cnt);
  end;

 if cnt = 0 then
  if length(string_read) > length(buf)*2 then
   begin
    str := string_read;
    string_read := '';
    string_next := false;
    result := length(str) > 0;
    exit;
   end
  else
   begin
    string_next := true;
    exit;
   end;


 str := copy(string_read, 1, pos-1);
 delete(string_read, 1, pos);

 string_next := length(string_read) = 0;
 result := true;
end;

procedure tFIFO_async.read_begin;
begin
 if self = nil then exit;
 EnterCriticalSection(crit_section_read);
end;

procedure tFIFO_async.read_end;
begin
 if self = nil then exit;
 LeaveCriticalSection(crit_section_read);
end;

procedure tFIFO_async.write_begin;
begin
 if self = nil then exit;
 EnterCriticalSection(crit_section_read);
end;

procedure tFIFO_async.write_end;
begin
 if self = nil then exit;
 LeaveCriticalSection(crit_section_read);
end;

end.
