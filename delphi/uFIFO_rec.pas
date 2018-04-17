unit uFIFO_rec;

interface

uses windows, sysutils, math;

{$Q-}
{$R-}

type
  tFIFO_rec_stat_direction = packed record
   count  : int64;
   errors : int64;
  end;

  tFIFO_rec_stat = packed record
   min : integer;
   max : integer;
   readed : tFIFO_rec_stat_direction;
   writed : tFIFO_rec_stat_direction;
  end;

  pFIFO_rec = ^tFIFO_rec;
  tFIFO_rec=packed record
   wr : Cardinal;
   rd : Cardinal;

   size : integer;
   mask : integer;

   stat_readed : int64; // for old prorams compatibility
   stat_writed : int64; // for old prorams compatibility

   errors_readed : cardinal; // for old prorams compatibility
   errors_writed : cardinal; // for old prorams compatibility

   stat : tFIFO_rec_stat; //main statistics
  end;

procedure _FIFO_rec_init(var fifo:tFIFO_rec; v_size:integer; var fixed_size:integer);

procedure _FIFO_rec_reset(var fifo:tFIFO_rec);
procedure _FIFO_rec_stat_reset(var fifo:tFIFO_rec);

function _FIFO_rec_count(var fifo:tFIFO_rec; fixed_size:integer):integer;
function _FIFO_rec_count_ratio(var fifo:tFIFO_rec; fixed_size:integer):integer;
function _FIFO_rec_free(var fifo:tFIFO_rec; fixed_size:integer):integer;
function _FIFO_rec_free_ratio(var fifo:tFIFO_rec; fixed_size:integer):integer;

procedure _FIFO_rec_write(var fifo:tFIFO_rec; data:pointer; fixed_size:integer; buffer:pointer; buf_size:integer);
procedure _FIFO_rec_read(var fifo:tFIFO_rec; data:pointer; fixed_size:integer; buffer:pointer; buf_size:integer);
procedure _FIFO_rec_read_void(var fifo:tFIFO_rec; fixed_size:integer; buf_size:integer);

//const
// fifo_no_tid = $FFFFFFFF;

implementation

uses classes;

////////////////////////////////////////////////////////////////////////////////

procedure MemoryBarrier;
{$IF defined(CPU386)}
asm
      PUSH EAX
      XCHG [ESP],EAX
      POP  EAX

      mfence
end;
{$ELSE}
begin
end;
{$IFEND}

procedure _FIFO_rec_init(var fifo:tFIFO_rec; v_size:integer; var fixed_size:integer);
var
 size_2_pwr : byte;
 k : integer;
begin
 size_2_pwr := 0;

 for k:= 30 downto 1 do
  if ((1 shl k) and v_size) <> 0 then
   if (((1 shl k)-1) and v_size) <> 0 then
    begin
     size_2_pwr := k+1;
     break;
    end
   else
    begin
     size_2_pwr := k;
     break;
    end;

 fixed_size := 1 shl size_2_pwr;
 fifo.size := fixed_size;
 fifo.mask := fixed_size - 1;

 _FIFO_rec_reset(fifo);
end;

procedure _FIFO_rec_reset(var fifo:tFIFO_rec);
begin
 fifo.rd := 0;
 fifo.wr := 0;
 _FIFO_rec_stat_reset(fifo);
 MemoryBarrier;

 fifo.wr := 0;
 fifo.rd := 0;
 _FIFO_rec_stat_reset(fifo);
 MemoryBarrier;
end;

function _FIFO_rec_count(var fifo:tFIFO_rec; fixed_size:integer):integer;
begin
 result := fifo.wr - fifo.rd;

 if result > fixed_size then result := fixed_size;
 if result < 0 then result := 0;
end;

function _FIFO_rec_free(var fifo:tFIFO_rec; fixed_size:integer):integer;
begin
 result := fixed_size - _FIFO_rec_count(fifo, fixed_size);
end;

procedure _FIFO_rec_write(var fifo:tFIFO_rec; data:pointer; fixed_size:integer; buffer:pointer; buf_size:integer);
var
 w,r : Cardinal;
 ptr : pbyte;
 buf : pbyte absolute buffer;
 pos : cardinal;
 cnt : integer;
begin
 if buf_size <= 0 then exit;

 MemoryBarrier;
 inc(fifo.stat_writed,       buf_size);
 inc(fifo.stat.writed.count, buf_size);

 w := fifo.wr;
 r := fifo.rd;

 pos := w mod fixed_size;
 cnt := w - r;

 if cnt < 0 then
  begin
   fifo.wr := r;
   inc(fifo.errors_writed);
   inc(fifo.stat.writed.errors);
   cnt := 0;
  end;

 if fifo.stat.min > cnt  then
  fifo.stat.min := cnt;

 if cnt >= fixed_size then
  begin
   fifo.wr := r + fixed_size;
   inc(fifo.errors_writed);
   inc(fifo.stat.writed.errors);
   exit;
  end;

 if buf_size > fixed_size - cnt then
  begin
   inc(fifo.errors_writed);
   inc(fifo.stat.writed.errors);
   buf_size := fixed_size - cnt;
   if buf_size = 0 then exit;
  end;

 if buf_size <= 0 then exit;

 ptr := data;
 inc(ptr, pos);

 if pos + buf_size > fixed_size then
  begin
   move(buf^, ptr^, fixed_size - pos);
   inc(buf, fixed_size - pos);
   move(buf^, data^, buf_size - (fixed_size - pos));
  end
 else
  move(buf^, ptr^, buf_size);

 inc(cnt, buf_size);
 if fifo.stat.max < cnt  then
  fifo.stat.max := cnt;

 inc(fifo.wr, buf_size);
 MemoryBarrier();
end;

procedure _FIFO_rec_read(var fifo:tFIFO_rec; data:pointer; fixed_size:integer; buffer:pointer; buf_size:integer);
var
 w,r : cardinal;
 ptr : pbyte;
 buf : pbyte absolute buffer;
 pos : cardinal;
 cnt : integer;
begin
 if buf_size <= 0 then exit;

 MemoryBarrier;
 inc(fifo.stat_readed,       buf_size);
 inc(fifo.stat.readed.count, buf_size);

 w := fifo.wr;
 r := fifo.rd;

 pos := r mod fixed_size;
 cnt := w - r;

 if fifo.stat.max < cnt  then
  fifo.stat.max := cnt;

 if cnt < 0 then
  begin
   fifo.rd := w;
   inc(fifo.errors_readed);
   inc(fifo.stat.readed.errors);
   exit;
  end;

 if cnt > fixed_size then
  begin
   fifo.rd := w - fixed_size;
   inc(fifo.errors_readed);
   inc(fifo.stat.readed.errors);
   cnt := fixed_size;
  end;

 if buf_size > cnt then
  begin
   inc(buf, cnt);
   fillchar(buf^, buf_size - cnt, 0);
   dec(buf, cnt);

   inc(fifo.errors_readed);
   inc(fifo.stat.readed.errors);
   buf_size := cnt;
  end;

 if buf_size <=0 then exit;

 ptr := data;
 inc(ptr, pos);

 if pos + buf_size > fixed_size then
  begin
   move(ptr^, buf^, fixed_size - pos);
   inc(buf, fixed_size - pos);
   move(data^, buf^, buf_size - (fixed_size - pos));
  end
 else
  move(ptr^, buf^, buf_size);

 dec(cnt, buf_size);
 if fifo.stat.min > cnt  then
  fifo.stat.min := cnt;

 inc(fifo.rd, buf_size);
 MemoryBarrier;
end;

procedure _FIFO_rec_read_void(var fifo:tFIFO_rec; fixed_size:integer; buf_size:integer);
var
 w,r : int64;
 cnt : integer;
begin
 if buf_size <= 0 then exit;

 MemoryBarrier;
 inc(fifo.stat_readed,       buf_size);
 inc(fifo.stat.readed.count, buf_size);

 w := fifo.wr;
 r := fifo.rd;
 cnt := w - r;

 if fifo.stat.max < cnt  then
  fifo.stat.max := cnt;

 if cnt < 0 then
  begin
   fifo.rd := r;
   inc(fifo.errors_readed);
   inc(fifo.stat.readed.errors);
   exit;
  end;

 if cnt > fixed_size then
  begin
   fifo.rd := r - fixed_size;
   inc(fifo.errors_readed);
   inc(fifo.stat.readed.errors);
   cnt := fixed_size;
  end;

 if buf_size > cnt then
  begin
   inc(fifo.errors_readed);
   inc(fifo.stat.readed.errors);
   buf_size := cnt;
  end;

 dec(cnt, buf_size);
 if fifo.stat.min > cnt  then
  fifo.stat.min := cnt;

 inc(fifo.rd, buf_size);
 MemoryBarrier;
end;

procedure _FIFO_rec_stat_reset(var fifo:tFIFO_rec);
begin
 fillchar(fifo.stat, sizeof(fifo.stat), 0);
 fifo.stat.min := MaxInt;

 fifo.stat_readed := 0;
 fifo.stat_writed := 0;
 fifo.errors_readed := 0;
 fifo.errors_writed := 0;

 MemoryBarrier;
end;

function  _FIFO_rec_count_ratio(var fifo:tFIFO_rec; fixed_size:integer):integer;
begin
 result:=round(_FIFO_rec_count(fifo, fixed_size)/fixed_size*1000);
end;

function  _FIFO_rec_free_ratio(var fifo:tFIFO_rec; fixed_size:integer):integer;
begin
 result:=round(_FIFO_rec_free(fifo, fixed_size)/fixed_size*1000);
end;

end.
