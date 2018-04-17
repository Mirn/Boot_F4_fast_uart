unit uFIFO_map;

interface
uses windows, sysutils, math,
 uMAP_File,
 uFIFO_async,
 uFIFO_rec,
 uMutex;

type
  tFIFO_map=class(tFIFO_async)
  private
   data : pointer;

   map_data : tMapFile;
   map_fifo : tMapFile;

   mode_writer : boolean;

   function open(as_server : boolean; as_writer : boolean) : boolean;
   function get_mode_writer:boolean;
   function get_created : boolean;

  protected
   mutex_read  : tMutex;
   mutex_write : tMutex;
   shadow_fifo : pFIFO_rec;

   procedure read_begin;override;
   procedure read_end;override;
   procedure write_begin;override;
   procedure write_end;override;

  public
   constructor create(name:string; v_size:integer; log:tMapFileLog=nil);
   destructor destroy;override;

   function create_reader : boolean;
   function create_writer : boolean;

   function open_as_reader : boolean;
   function open_as_writer : boolean;

   procedure close;

   procedure update;
   procedure shadow_enable;

   property is_writer  : boolean read get_mode_writer;
   property is_created : boolean read get_created;
  end;

implementation

constructor tFIFO_map.create(name:string; v_size:integer; log:tMapFileLog);
begin
 inherited create(v_size);

 map_data := tMapFile.create(name+'_'+inttostr(fixed_size)+'_data', fixed_size, log);
 map_fifo := tMapFile.create(name+'_'+inttostr(fixed_size)+'_fifo', sizeof(fifo^), log);

 mutex_read  := tMutex.create(name+'_'+inttostr(fixed_size)+'_read', log);
 mutex_write := tMutex.create(name+'_'+inttostr(fixed_size)+'_write', log);

 delete_objects;
end;

destructor tFIFO_map.destroy;
begin
 if self = nil then exit;

 map_data.close;
 map_fifo.close;

 map_data.Free;
 map_fifo.Free;

 map_data := nil;
 map_fifo := nil;

 if shadow_fifo <> nil then
  Dispose(fifo);

 fifo := nil;
 data := nil;
end;

function tFIFO_map.open(as_server : boolean; as_writer : boolean) : boolean;
var
 res_wr : boolean;
 res_rd : boolean;
 res_fifo : boolean;
 res_data : boolean;
begin
 result := false;
 if self = nil then exit;

 res_rd := mutex_read.open(as_server);
 res_wr := mutex_write.open(as_server);
 res_fifo := map_fifo.open(as_server, true);
 res_data := map_data.open(as_server, as_writer);
 if not (res_fifo and res_data and res_wr and res_rd) then
  begin
   mutex_read.close;
   mutex_write.close;
   map_fifo.close;
   map_data.close;
   exit;
  end;

 fifo := map_fifo.data;
 data := map_data.data;
 fifo_data_ptr := data;

 if map_fifo.is_created then
  _FIFO_rec_init(fifo^, map_data.size, fixed_size);

 string_read := '';
 string_next := true;

 mode_writer := as_writer;
 result := true;
end;

function tFIFO_map.create_writer : boolean;
begin
 result := open(true, true);
end;

function tFIFO_map.create_reader : boolean;
begin
 result := open(true, false);
end;

function tFIFO_map.open_as_writer : boolean;
begin
 result := open(false, true);
end;

function tFIFO_map.open_as_reader : boolean;
begin
 result := open(false, false);
end;

procedure tFIFO_map.close;
begin
 if self = nil then exit;

 map_data.close;
 map_fifo.close;

 mutex_read.close;
 mutex_write.close;

 fifo_data_ptr := nil;
 fifo := nil;
 data := nil;
end;

function tFIFO_map.get_mode_writer:boolean;
begin
 result := false;
 if self = nil then exit;
 result := mode_writer;
end;

procedure tFIFO_map.read_begin;
begin
 if self = nil then exit;
 mutex_read.Lock;
end;

procedure tFIFO_map.read_end;
begin
 if self = nil then exit;
 mutex_read.UnLock;
end;

procedure tFIFO_map.write_begin;
begin
 if self = nil then exit;
 mutex_write.Lock;
end;

procedure tFIFO_map.write_end;
begin
 if self = nil then exit;
 mutex_write.UnLock;
end;

function tFIFO_map.get_created : boolean;
begin
 result := false;

 if self = nil then exit;
 if map_data = nil then exit;
 if map_fifo = nil then exit;

 result := map_fifo.is_created or map_data.is_created;
end;

procedure tFIFO_map.update;
begin
 if shadow_fifo = nil then
  exit;

 fifo.wr            := shadow_fifo^.wr;
 fifo.stat_writed   := shadow_fifo^.stat_writed;
 fifo.errors_writed := shadow_fifo^.errors_writed;
 fifo.stat          := shadow_fifo^.stat;
end;

procedure tFIFO_map.shadow_enable;
begin
 shadow_fifo := fifo;
 fifo := nil;
 New(fifo);
 Move(shadow_fifo^, fifo^, sizeof(fifo^));
end;


end.
