unit fifo;

interface

uses windows, sysutils, math;

type
  tFIFO=class
  private
   data:array of byte;
   write_pos:integer;
   read_pos:integer;
   v_count:integer;
   r_count:integer;
   v_max:integer;
   debug:integer;
   critical_section:_RTL_CRITICAL_SECTION;

   function get_free:integer;
   procedure put(v:byte);
   function  get:byte;

  public
   stat_readed, stat_writed:int64;

   constructor create(size:integer);
   destructor destroy;override;
   procedure write_nonblock(buffer:pointer;size:integer);
   procedure read_nonblock(buffer:pointer;size:integer);
   procedure read_void(size:integer);
   procedure write(buffer:pointer;size:integer);
   procedure write_cmd(buffer:pbyte;size:integer);
   procedure read(buffer:pointer;size:integer);
   procedure reset;
   procedure stat_reset;
   function  free_ratio:integer;
   function  count_ratio:integer;

   property count:integer read r_count;
   property max:integer read v_max;
   property free_bytes:integer read get_free;
  end;

  tfifo_blocks=class
  private
   critical_section:_RTL_CRITICAL_SECTION;
   blocks:tfifo;
   data:tfifo;

   read_blocks_count:integer;
   read_blocks_free:integer;
   read_data_count:integer;
   read_data_free:integer;
   procedure update;

   function read_blocks_max:integer;
   function read_data_max:integer;
  public
   stat_bytes_readed, stat_bytes_writed:int64;
   stat_blocks_readed, stat_blocks_writed:int64;
   stat_blocks_overfulled, stat_bytes_overfulled:int64;
   stat_blocks_lost, stat_bytes_lost:int64;

   constructor create(data_size, blocks_count:integer);
   destructor destroy;override;

   function  read(block:pointer;  maxsize:integer):integer;
   function  read_nonblock(block:pointer;  maxsize:integer):integer;
   function  read_void:integer;
   function  read_str:string;

   procedure write(block:pointer; size:integer);
   procedure write_cmd(buffer:pbyte;size:integer);
   procedure write_nonblock(block:pointer; size:integer);
   procedure write_str(str:string);

   procedure reset;
   procedure stat_reset;
   function  free_ratio:integer;
   function  count_ratio:integer;

   property blocks_count:integer read read_blocks_count;
   property blocks_free:integer  read read_blocks_free;
   property blocks_max:integer   read read_blocks_max;

   property data_count:integer   read read_data_count;
   property data_free:integer    read read_data_free;
   property data_max:integer     read read_data_max;
  end;

procedure fifo_blocks_test;

implementation

uses classes;

constructor tfifo_blocks.create;
begin
 inherited create();
 blocks:=tfifo.create(blocks_count*sizeof(integer));
 data:=tfifo.create(data_size);
 InitializeCriticalSection(critical_section);
 update;
end;

destructor tfifo_blocks.destroy;
begin
 inherited destroy;
 blocks.FreeInstance; blocks:=nil;
 data.FreeInstance; data:=nil;
 DeleteCriticalSection(critical_section);
end;

function tfifo_blocks.read_blocks_max;
begin
 result:=blocks.max div sizeof(integer);
end;

function tfifo_blocks.read_data_max;
begin
 result:=data.max;
end;

procedure tfifo_blocks.update;
begin
 if self=nil then exit;
 if data=nil then exit;
 if blocks=nil then exit;

 read_blocks_count:=blocks.count div sizeof(integer);
 read_blocks_free:=blocks.free_bytes div sizeof(integer);
 read_data_count:=data.count;
 read_data_free:=data.free_bytes;
end;

procedure tfifo_blocks.reset;
begin
 if self=nil then exit;

 EnterCriticalSection(critical_section);
 blocks.reset;
 data.reset;
 update;
 LeaveCriticalSection(critical_section);
end;

procedure tfifo_blocks.stat_reset;
begin
 blocks.stat_reset;
 data.stat_reset;

 stat_bytes_readed:=0;
 stat_bytes_writed:=0;
 stat_blocks_readed:=0;
 stat_blocks_writed:=0;
 stat_blocks_overfulled:=0;
 stat_bytes_overfulled:=0;
 stat_blocks_lost:=0;
 stat_bytes_lost:=0;
end;

procedure tfifo_blocks.write(block:pointer;size:integer);
begin
 if self=nil then exit;

 EnterCriticalSection(critical_section);
 write_nonblock(block, size);
 LeaveCriticalSection(critical_section);
end;

function  tfifo_blocks.read(block:pointer;maxsize:integer):integer;
begin
 result:=0;
 if self=nil then exit;

 EnterCriticalSection(critical_section);
 result:=read_nonblock(block, maxsize);
 LeaveCriticalSection(critical_section);
end;


procedure tfifo_blocks.write_str(str:string);
begin
 if @self=nil then exit;
 if length(str)=0 then exit;

 write(@(str[1]), length(str)*sizeof(str[1]));
end;

function  tfifo_blocks.read_str:string;
var
 len:integer;
begin
 result:='';
 if @self=nil then exit;

 SetLength(result,$4000);
 len:=read(@(result[1]), length(result)*sizeof(result[1]));
 if len<length(result)*sizeof(result[1]) then
  SetLength(result, len div sizeof(result[1]));
end;

procedure tfifo_blocks.write_nonblock(block:pointer; size:integer);
begin
 if self=nil then exit;
 if blocks=nil then exit;
 if data=nil then exit;
 if size>data.max then exit;
 if size=0 then exit;
{ assert(((blocks.free_bytes=0) and (data.free_bytes=0)) or
        ((blocks.free_bytes<>0) and (data.free_bytes<>0)));}
 assert((blocks.free_bytes mod sizeof(integer))=0);

 while (blocks.free_bytes div sizeof(integer)<=0) or (data.free_bytes<size) do
  begin
   inc(stat_bytes_overfulled, read_void);
   inc(stat_blocks_overfulled);
  end;

 assert(((blocks.free_bytes=0) and (data.free_bytes=0)) or
        ((blocks.free_bytes<>0) and (data.free_bytes<>0)));
 assert((blocks.free_bytes mod sizeof(integer))=0);

 data.write_nonblock(block, size);
 blocks.write_nonblock(@size, sizeof(size));
 inc(stat_blocks_writed);
 inc(stat_bytes_writed, size);
 update;
end;

function  tfifo_blocks.read_nonblock(block:pointer;  maxsize:integer):integer;
var
 current,limited:integer;
begin
 result:=0;
 if self=nil then exit;
 if blocks=nil then exit;
 if data=nil then exit;
 if maxsize=0 then exit;
{ assert(((blocks.free_bytes=0) and (data.free_bytes=0)) or
        ((blocks.free_bytes<>0) and (data.free_bytes<>0)),
        ' blocks.free_bytes=' + inttostr(blocks.free_bytes)+
        ' data.free_bytes='   + inttostr(data.free_bytes)
       );                             }
 assert((blocks.free_bytes mod sizeof(integer))=0);

 if blocks.count=0 then
  begin
   result:=0;
   exit;
  end;

 assert(blocks.count<>0);
 blocks.read_nonblock(@current, sizeof(current));
 assert(current<=data.count);
 assert(current<>0);

 if current>maxsize then
  limited:=maxsize
 else
  limited:=current;
 data.read_nonblock(block, limited);
 data.read_void(current-limited);
 if (current-limited)<>0 then
  begin
   inc(stat_bytes_lost, current-limited);
   inc(stat_blocks_lost);
  end;
 result:=current;
 inc(stat_blocks_readed);
 inc(stat_bytes_readed, limited);
 update;
end;

function  tfifo_blocks.read_void;
var
 current:integer;
begin
 result:=0;
 if self=nil then exit;
 if blocks=nil then exit;
 if data=nil then exit;

 blocks.read_nonblock(@current, sizeof(current));
 data.read_void(current);
 result:=current;
 update;
end;

function tfifo_blocks.free_ratio:integer;
var
 p1,p2:integer;
begin
 result:=0;
 if self=nil then exit;
 if blocks.max=0 then exit;
 if data.max=0 then exit;

 p1:=blocks.free_ratio;
 p2:=data.free_ratio;
 result:=min(p1,p2);
end;

function tfifo_blocks.count_ratio:integer;
var
 p1,p2:integer;
begin
 result:=0;
 if self=nil then exit;
 if blocks.max=0 then exit;
 if data.max=0 then exit;

 p1:=blocks.count_ratio;
 p2:=data.count_ratio;
 result:=max(p1,p2);
end;

////////////////////////////////////////////////////////////////////////////////

constructor tfifo.create(size:integer);
begin
 inherited create();

 v_max:=size;
 setlength(data,max);
 v_count:=0;
 r_count:=0;
 write_pos:=0;
 read_pos:=0;
 InitializeCriticalSection(critical_section);
 debug:=0;
end;

destructor tfifo.destroy;
begin
 inherited destroy;

 setlength(data,0);
 DeleteCriticalSection(critical_section);
end;

procedure tfifo.reset;
begin
 if self=nil then exit;

 EnterCriticalSection(critical_section);
 v_count:=0;
 r_count:=0;
 write_pos:=0;
 read_pos:=0;
 LeaveCriticalSection(critical_section);
end;

function  tfifo.get_free;
begin
 result:=0;
 if self=nil then exit;
 result:=max-v_count;
end;

procedure tfifo.put(v:byte);
begin
 if self=nil then exit;

 data[write_pos]:=v;
 inc(write_pos);
 if write_pos>=max then write_pos:=0;

 if v_count<max then
  inc(v_count)
 else
  begin
   inc(read_pos);
   if read_pos>=max then read_pos:=0;
  end;
end;

function  tfifo.get:byte;
begin
 result:=0;
 if self=nil then exit;

 if v_count<=0 then
  begin
   result:=0;
   exit;
  end;
 result:=data[read_pos];
 inc(read_pos);
 if read_pos>=max then read_pos:=0;
 dec(v_count);
end;

procedure tfifo.write(buffer:pointer;size:integer);
begin
 if self=nil then exit;
 EnterCriticalSection(critical_section);
 write_nonblock(buffer, size);
 LeaveCriticalSection(critical_section);
end;

procedure tfifo.read(buffer:pointer;size:integer);
begin
 if self=nil then exit;
 EnterCriticalSection(critical_section);
 read_nonblock(buffer, size);
 LeaveCriticalSection(critical_section);
end;

procedure tfifo.write_nonblock(buffer:pointer;size:integer);
var
 buf:pbyte;
begin
 if self=nil then exit;
 inc(stat_writed, size);

 buf:=buffer;

 if size>max then
  begin
   inc(buf, size-max);
   size:=max;
  end;

 if write_pos+size>max then
  begin
   move(buf^, data[write_pos], max-write_pos);
   inc(buf, max-write_pos);
   move(buf^, data[0], size-(max-write_pos));
  end
 else
  move(buf^, data[write_pos], size);

 write_pos:=(write_pos + size) mod max;

 inc(v_count, size);
 if v_count>max then
  begin
   read_pos:=(read_pos + v_count-max) mod max;
   v_count:=max;
  end;{}

{ while size>0 do
  begin
   put(buf^);
   inc(buf);
   dec(size);
  end;{}

 r_count:=v_count;
end;

procedure tfifo.read_nonblock(buffer:pointer;size:integer);
var
 buf:pbyte;
 need, part:integer;
begin
 if self=nil then exit;
 inc(stat_readed, size);
 buf:=buffer;

 need:=size;
 if need>v_count then
  need:=v_count;

 if need<>0 then
 if read_pos+need>max then
  begin
   part:=max-read_pos;
   move(data[read_pos], buf^, part);
   inc(buf, part);

   part:=size-(max-read_pos);
   move(data[0], buf^, part);
   inc(buf, part);
  end
 else
  begin
   move(data[read_pos], buf^, need);
   inc(buf, need);
  end;

 if need<>size then
  fillchar(buf^, size-need, 0);

 read_pos:=(read_pos + need) mod max;
 dec(v_count, need);

{ while size>0 do
  begin
   buf^:=get;
   inc(buf);
   dec(size);
  end;{}
 r_count:=v_count;
end;

procedure tfifo.read_void(size:integer);
var
 need:integer;
begin
 if self=nil then exit;

 need:=size;
 if need>v_count then
  need:=v_count;

 read_pos:=(read_pos + need) mod max;
 dec(v_count, need);

{ while size>0 do
  begin
   get;
   dec(size);
  end;}
 r_count:=v_count;
end;

function rdtsc:int64;
asm
   db $0F; db $31;
end;

procedure fifo_blocks_test;
var
 a:array of longint;
 b:array of longint;
 k:integer;
 rd_pos:integer;
 wr_pos:integer;
 size:integer;
 s:tstringlist;
// str:string;
 f:file;
 fifo:tfifo_blocks;
begin
 randseed:=rdtsc and $7FFFFFFF;
 setlength(a, 25000000);
 setlength(b, 25000000);
 for k:=0 to length(a)-1 do
  a[k]:=rdtsc and $7FFFFFFF;
 s:=tstringlist.Create;
 fifo:=tfifo_blocks.create(length(a)*sizeof(a), 10000);

 AssignFile(f,'test_before.dat');
 rewrite(f,1);
 blockwrite(f,a[0], length(a)*sizeof(a));
 closefile(f);

 wr_pos:=0;
 rd_pos:=0;

 while wr_pos<length(a) do
  begin
   if random(100)>=50 then
    begin
     size:=1+random(10000);
     if rd_pos+size>length(a) then
      size:=length(a)-rd_pos;
     if rd_pos>=length(a) then
      fifo.write(@(a[0]), size*sizeof(a[0]))
     else
      fifo.write(@(a[rd_pos]), size*sizeof(a[0]));
     s.Add('fifo <-'#9+inttostr(size)+#9+inttostr(fifo.blocks_count)+#9+inttostr(fifo.data_count)+#9+inttostr(rd_pos)+#9+inttostr(wr_pos));
     inc(rd_pos, size);
    end
   else
    begin
     size:=fifo.read(@(b[wr_pos]), 10000*sizeof(a[0]));
     assert((size mod 4)=0);
     size:=size div 4;
     inc(wr_pos, size);
     s.Add('fifo ->'#9+inttostr(size)+#9+inttostr(fifo.blocks_count)+#9+inttostr(fifo.data_count)+#9+inttostr(rd_pos)+#9+inttostr(wr_pos));
    end;
  end;
 s.SaveToFile('fifo.log');

 AssignFile(f,'test_after.dat');
 rewrite(f,1);
 blockwrite(f,b[0], length(b)*sizeof(b));
 closefile(f);

 setlength(a, 0);
 setlength(b, 0);
 fifo.Free;
 s.Free;
end;

procedure tfifo.stat_reset;
begin
 stat_readed:=0;
 stat_writed:=0;
end;

function  tfifo.count_ratio:integer;
begin
 result:=round(count/max*1000);
end;

function  tfifo.free_ratio:integer;
begin
 result:=round(free_bytes/max*1000);
end;

procedure tfifo.write_cmd(buffer:pbyte;size:integer);
begin
 self.write(buffer, size);
end;

procedure tfifo_blocks.write_cmd(buffer:pbyte;size:integer);
begin
 self.write(buffer, size);
end;

end.
