{DEFINE DIRECT_LOG_FILE}
unit LinkClient;

interface
uses Classes, SysUtils, fifo, ExtCtrls, Windows, math, umap_debug_log;

type
  tLinkState=(link_idle, link_open, link_establish, link_error, link_close);
  tLinkClient=class;

  tLinkEvent              = procedure(sender:tLinkClient) of object;
  tLinkLogEvent           = procedure(sender:tobject; msg:string) of object;
  tLinkErrorEvent         = procedure(sender:tLinkClient; Code:integer) of object;
  tLinkRxTxEvent          = procedure(sender:tLinkClient; data:pbyte; size:integer) of object;
  tLinkFastRxTxEvent      = function (sender:tLinkClient; data:pbyte; size:integer):boolean of object;
  tLinkFastPreCheckEvent  = function (var read_ready:boolean; var write_ready:boolean):boolean of object;
  tLinkFastPostCheckEvent = function (old_read_ready:boolean; old_write_ready:boolean; var read_ready:boolean; var write_ready:boolean):boolean of object;

  tLinkClient=class(TThread)
  private
    v_State      : tLinkState;
    v_error_code : integer;

    tx_buf  : array of byte;
    rx_buf  : array of byte;
    tx2_buf : array of byte;
    rx2_buf : array of byte;

    api_tread_id : cardinal;
    net_tread_id : cardinal;

    f_connected    : boolean;
    f_disconnected : boolean;
    f_error        : boolean;

    exeptions_count : integer;

    log_file : file;
    log_file_enabled : boolean;
    log_file_error : boolean;

    procedure run_idle;
    procedure run_establish;
    procedure run_establish_main;
    procedure run_close;
    procedure run_open;
    procedure run_error;
    procedure set_state(new_state:tLinkstate);
    procedure run_log(code:integer);

    procedure file_open;
    procedure file_run;
    procedure file_close;

    procedure do_rx_event(readed:integer);

    property  new_state:tLinkstate write set_state;

  protected
    tx_fifo   : tfifo_blocks;
    rx_fifo   : tfifo_blocks;
    txed_fifo : tfifo_blocks;
    log_fifo  : tfifo_blocks;

    tx_max_block : integer;
    rx_max_block : integer;
    zero_close   : boolean;

    map_log : tMAP_debug_main;

    procedure Execute; override;

    function  onFastRx_private(sender:tLinkClient; data:pbyte; size:integer):boolean; virtual;
    function  onFastPreCheck_private(var read_ready:boolean; var write_ready:boolean):boolean; virtual;
    function  onFastPostCheck_private(old_read_ready:boolean; old_write_ready:boolean; var read_ready:boolean; var write_ready:boolean):boolean; virtual;
    procedure onTick_private;virtual;
    procedure onTimerTick;virtual;
    procedure onConnect_private;virtual;
    procedure onDisconnect_private;virtual;

    procedure onInit;virtual;
    procedure onDone;virtual;

    function  hardware_open:boolean;virtual;abstract;
    function  hardware_close:boolean;virtual;abstract;
    function  hardware_check(var read_ready:boolean; var write_ready:boolean):boolean;virtual;abstract;
    function  hardware_errorcode:integer;virtual;abstract;
    function  hardware_write(buf:pointer; size:integer; var writed:integer):boolean;virtual;abstract;
    function  hardware_read(buf:pointer; size:integer; var readed:integer):boolean;virtual;abstract;

  public
    timer        : ttimer;

    log_name   : string[255];

    onLog      : tLinkLogEvent;
    onLogBegin : tLinkEvent;
    onLogEnd   : tLinkEvent;

    onRX     : tLinkRxTxEvent;
    onTX     : tLinkRxTxEvent;
    onFastRx : tLinkFastRxTxEvent;
    onFastTx : tLinkFastRxTxEvent;

    onConnect    : tLinkEvent;
    onDisconnect : tLinkEvent;
    onError      : tLinkErrorEvent;
    noRead       : boolean;

    stat_iterations  : int64;
    stat_readed      : int64;
    stat_writed      : int64;
    stat_read_speed  : int64;
    stat_write_speed : int64;

    stat_select_timeout_none : int64;
    stat_select_timeout_rx   : int64;
    stat_select_timeout_tx   : int64;

    debug : integer;

    file_main : file;
    file_name : string;
    file_block_size  : integer;
    file_block_delay : integer;

    RX_clear : boolean;

    constructor Create(tx_buf_size:integer=65536;
                       rx_buf_size:integer=65536;
                       tx_max_block_size:integer=10000;
                       rx_max_block_size:integer=10000);
    destructor destroy;override;

    procedure Log_add(msg:string);virtual;

    procedure Open;
    procedure Close;
    procedure Write(data:pbyte; size:integer);
    procedure update_log;
    procedure onTimer(sender:tobject); virtual;
    procedure stat_reset_speed;virtual;
    procedure stat_reset_all;virtual;

    function tx_free_bytes:integer;  virtual;
    function tx_free_blocks:integer; virtual;
    function tx_free_ratio:integer;  virtual;

    function TreadString : string; virtual;

    property error_code:integer read v_error_code;
    property State:tLinkstate read v_State;
    property RX_fifo_blocks:tfifo_blocks read rx_fifo;
    property TX_fifo_blocks:tfifo_blocks read tx_fifo;
    property TXed_fifo_blocks:tfifo_blocks read txed_fifo;
  end;

implementation

constructor tLinkclient.Create;
begin
 v_state:=link_idle;
 inherited create(false);
 zero_close := true;
 v_state := link_idle;

 onFastRx := nil;

 tx_max_block:=tx_max_block_size;
 rx_max_block:=rx_max_block_size;

 setlength(tx_buf,  tx_max_block);
 setlength(tx2_buf, tx_max_block);
 setlength(rx_buf,  rx_max_block);
 setlength(rx2_buf, rx_max_block);

 timer := TTimer.Create(nil);
 timer.Interval := 16;
 timer.OnTimer  := onTimer;
 timer.Enabled  := true;

 api_tread_id := GetCurrentThreadId;

 log_fifo  := tfifo_blocks.create($40000, 16384);
 tx_fifo   := tfifo_blocks.create(tx_buf_size, tx_buf_size div 8);
 rx_fifo   := tfifo_blocks.create(rx_buf_size, rx_buf_size div 8);
 txed_fifo := tfifo_blocks.create(tx_buf_size, tx_buf_size div 8);

 Priority:=tpTimeCritical;
 onInit;
end;

destructor tLinkclient.destroy;
begin
 inherited destroy;

 run_log(1);
 onDone;

 setlength(tx_buf, 0);
 setlength(rx_buf, 0);
 setlength(tx2_buf, 0);
 setlength(rx2_buf, 0);

 timer.Free;     timer:=nil;
 tx_fifo.Free;   tx_fifo:=nil;
 rx_fifo.Free;   rx_fifo:=nil;
 log_fifo.Free;  log_fifo:=nil;
 txed_fifo.Free; txed_fifo:=nil;

 if map_log <> nil then
  begin
   map_log.free;
   map_log := nil;
  end;
end;

procedure tLinkClient.onInit;
begin
end;

procedure tLinkClient.onDone;
begin
end;

procedure tLinkClient.set_state;
begin
 if self=nil then exit;
 if (v_State<>link_establish) and (new_state=link_establish) then f_connected:=true;
 if (v_State<>link_error) and (new_state=link_error) then f_error:=true;
 if (v_State<>link_idle) and (new_state=link_idle) then f_disconnected:=true;

 v_State:=new_state;
end;

procedure tLinkClient.Log_add;
var
 s : string;
begin
 if self=nil then exit;

{$IFDEF DIRECT_LOG_FILE}
 if (log_file_enabled = false) and (log_file_error = false) then
  begin
   {$I-}
   s := self.classname + '_'+inttohex(integer(@self), 8)+'.log';
   AssignFile(log_file, s);
   rewrite(log_file, 1);
   if IOResult <> 0 then
    log_file_error := true
   else
    log_file_enabled := true;
   {$I+}
  end;

 if (log_file_enabled = true) and (log_file_error = false) and (length(msg) >= 1) then
  begin
   {$I-}
   s := msg + #13#10;
   BlockWrite(log_file, s[1], length(s));
   if IOResult <> 0 then
    log_file_error := true;
   {$I+}
  end;
{$ENDIF}

 log_fifo.write_str(msg);

 if (map_log = nil) and (log_name <> '') then
  begin
   map_log := tMAP_debug_main.create(log_name + '#link', 'Messages', self.Log_add);
   if not map_log.create_log then
    begin
     log_name := '';
     map_log.free;
     map_log := nil;
    end;
  end;

 if map_log <> nil then
  if pos('To_Commands: ', msg) = 0 then
   map_log.send(msg);
end;

procedure tLinkClient.open;
begin
 if self=nil then exit;
 if State<>link_idle then exit;

 new_state:=link_open;
end;

procedure tLinkclient.write;
begin
 if self=nil then exit;
 if State<>link_establish then exit;

 if size>length(tx_buf) then
  begin
   log_add('write: size>length(tx_buf)');
   exit;
  end;

 tx_fifo.write(data, size);
end;

procedure tLinkClient.close;
begin
 if self=nil then exit;
 if State in [link_open, link_error, link_establish, link_close] then
  new_State:=link_close
 else
  new_State:=link_idle;
end;

procedure tLinkclient.update_log;
begin
 if self=nil then exit;
 run_log(1234);
end;

procedure tLinkclient.run_log(code:integer);
var
 flag:boolean;
 msg:string;
begin
 if self=nil then exit;
{ assert(api_tread_id=GetCurrentThreadId, 'Code = '+inttostr(code)+
   #13' API thread='+inttohex(api_tread_id,8)+
   #13' NET thread='+inttohex(net_tread_id,8)+
   #13' Cur thread='+inttohex(GetCurrentThreadId,8)
       );
 if api_tread_id<>GetCurrentThreadId then exit;  }

 if log_fifo.blocks_count=0 then exit;

 flag:=false;
 if (@onLogBegin)<>nil then
  if log_fifo.blocks_count>2 then
   begin
    onLogBegin(self);
    flag:=true;
   end;

 while log_fifo.blocks_count<>0 do
  begin
   msg:=log_fifo.read_str;
   if (@onLog)<>nil then onLog(self, ClassName+': '+msg);
  end;

 if flag then
  if (@onLogEnd)<>nil then
    onLogEnd(self);
end;

procedure tLinkClient.ontimer;
var
 readed, writed, blocks_count:integer;
 last_time:cardinal;
begin
 if self=nil then exit;
 run_log(0);
 onTimerTick();

 if f_connected then
  begin
   Log_add('Connected');
   if @onConnect<>nil then onConnect(self);
   f_connected:=false;
  end;

 if f_error then
  begin
   Log_add('f_error');
   if @onError<>nil then onError(self, v_error_code);
   f_error:=false;
  end;

 blocks_count:=0;
 last_time:=GetTickCount;
 if @onRX<>nil then
  while rx_fifo.blocks_count<>0 do
   begin
    readed:=rx_fifo.read(@(rx_buf[0]), length(rx_buf));

//    if @onRX<>nil then
    onRX(self, @(rx_buf[0]), readed);
    inc(stat_readed, readed);
    inc(stat_read_speed, readed);

    inc(blocks_count);
    if blocks_count>1000 then
     begin
      blocks_count:=0;
      if GetTickCount-last_time>=64 then
       begin
 //       Log_add('CPU timeout');
        break;
       end;
     end;
   end
 else
  if RX_clear then
   while rx_fifo.blocks_count<>0 do
    begin
     readed:=rx_fifo.read(@(rx_buf[0]), length(rx_buf));
     inc(stat_read_speed, readed);
    end; 

 if @onTX<>nil then
  while txed_fifo.blocks_count<>0 do
   begin
    writed:=txed_fifo.read(@(tx_buf[0]), length(tx_buf));
    if @onTX<>nil then
     onTX(self, @(tx_buf[0]), writed);

    inc(blocks_count);
    if blocks_count>1000 then
     begin
      blocks_count:=0;
      if GetTickCount-last_time>=64 then
       begin
        Log_add('CPU timeout');
        break;
       end;
     end;
   end;

 if (f_disconnected) and (rx_fifo.blocks_count=0) then
  begin
   Log_add('Disconnected');
   if @onDisconnect<>nil then onDisconnect(self);
   f_disconnected:=false;
  end;
end;

procedure tLinkClient.run_idle;
begin
end;

procedure tLinkClient.run_open;
begin
 txed_fifo.reset;
 tx_fifo.reset;
 rx_fifo.reset;

 try
  if not hardware_open then
   begin
    Log_add('not hardware_open');
    new_State:=link_idle;
    exit;
   end;
 except
  on E : Exception do
   begin
    hardware_close;
    Log_add('ERROR Exception: '+E.ClassName+' msg: '+E.Message);
    Log_add('not hardware_open');
    new_State:=link_idle;
    exit;
   end;
 end;

 Log_add('TID = '+inttostr(windows.GetCurrentThreadId));

 f_connected:=false;
 f_disconnected:=false;
 f_error:=false;

 v_error_code:=0;

 txed_fifo.reset;
 tx_fifo.reset;
 if self.ClassName <> 'tE1Client' then
  rx_fifo.reset;

 if state = link_open then
  new_State:=link_establish;
 onConnect_private;
end;

procedure tLinkClient.do_rx_event(readed:integer);
begin
 if @onFastRx=nil then
  rx_fifo.write(@(rx2_buf[0]), readed)
 else
  if onFastRx(self, @(rx2_buf[0]), readed) then
   rx_fifo.write(@(rx2_buf[0]), readed)
  else
   begin
    inc(stat_readed, readed);
    inc(stat_read_speed, readed);
   end;
end;

procedure tLinkClient.run_establish;
begin
 try
  run_establish_main;

  if exeptions_count > 0 then
   dec(exeptions_count);
 except
  on E : Exception do
   begin
    Log_add('ERROR ('+inttostr(exeptions_count)+'): '+E.ClassName+': '+E.Message);
    inc(exeptions_count, 11);
    if exeptions_count > 255 then
     begin
      exeptions_count := 0;
      self.close;
     end;
    exit;
   end;
 end;
end;

procedure tLinkClient.run_establish_main;
var
 readed:integer;
 writed:integer;
 tx_size:integer;

 f_read, f_write, f_write_old, f_read_old:Boolean;

begin
 if self=nil then exit;

 onTick_private;

 f_read:=rx_fifo.data_free>0;
 f_write:=tx_fifo.blocks_count>0;
 f_write_old:=f_write;
 f_read_old:=f_read;

 if not onFastPreCheck_private(f_read, f_write) then
  begin
   Log_add('onFastPreCheck_private error');
   new_State:=link_error;
   v_error_code:=97;
   exit;
  end;

 if not hardware_check(f_read, f_write) then
  begin
   Log_add('hardware_check: '+inttostr(hardware_errorcode));
   new_State:=link_error;
   v_error_code:=90;
   exit;
  end;

 if not onFastPostCheck_private(f_read_old, f_write_old, f_read, f_write) then
  begin
   Log_add('onFastPostCheck_private error');
   new_State:=link_error;
   v_error_code:=97;
   exit;
  end;

 if (not f_read) and (not f_write) then
  begin
   inc(stat_select_timeout_none);
   exit;
  end;

 if f_read then
 if noRead then
  sleep(1)
 else
  //if rx_fifo.data_free > 0 then
   begin
    inc(stat_select_timeout_rx);
    if not hardware_read(rx2_buf, min(rx_fifo.data_free, length(rx2_buf)), readed) then
     begin
      Log_add('Read error: '+inttostr(hardware_errorcode));
      new_State:=link_error;
      v_error_code:=1;
      exit;
     end;

 //   Log_add('Readed '+inttostr(readed));
    if zero_close and (readed=0) then
     begin
      Log_add('Closed by remote host');
      new_State:=link_close;
      exit;
     end;

    if onFastRx_private(self, @(rx2_buf[0]), readed) then
     do_rx_event(readed)
    else
     begin
      inc(stat_readed, readed);
      inc(stat_read_speed, readed);
     end;
   end;

  if (f_write=false) and (f_write_old=false) and (tx_fifo.blocks_count>0) then
   begin
    f_read:=false;
    f_write:=true;
    if not hardware_check(f_read, f_write) then
     begin
      Log_add('hardware_check_2: '+inttostr(hardware_errorcode));
      new_State:=link_error;
      v_error_code:=91;
      exit;
     end;
   end;



 if f_write then
  begin
   inc(stat_select_timeout_tx);
   tx_size:=tx_fifo.read(@(tx2_buf[0]), length(tx2_buf));
//   writed := tx_size;
   if not hardware_write(tx2_buf, tx_size, writed) then
    begin
     Log_add('Write error: '+inttostr(hardware_errorcode));
     new_State:=link_error;
     v_error_code:=2;
     exit;
    end;
   if writed<>tx_size then
    begin
     Log_add('Write error: writed<>tx_size');
     new_State:=link_error;
     v_error_code:=3;
     exit;
    end;

//    Log_add('Write '+inttostr(writed));
   inc(stat_writed, writed);
   inc(stat_write_speed, writed);
   if @onFastTx=nil then
    begin
     if @onTX<>nil then
      txed_fifo.write(@(tx2_buf[0]), tx_size);
    end
   else
    if onFastTx(self, @(tx2_buf[0]), tx_size) then
     begin
      if @onTX<>nil then
       txed_fifo.write(@(tx2_buf[0]), tx_size);
     end;
  end;
end;

procedure tLinkClient.run_close;
begin
 if self=nil then exit;
 if not hardware_close then
  log_add('ERROR: hardware_close');
 log_add('Closed');

 new_State:=link_idle;
 onDisconnect_private;
end;

procedure tLinkClient.run_error;
begin
 if self=nil then exit;
 new_State:=link_close;
end;

procedure tLinkClient.file_open;
var
 result:integer;
begin
 {$I-}
 AssignFile(file_main, file_name);
 FileMode := fmOpenRead;
 reset(file_main, 1);
 {$I+}
 result:=IOResult;
 if result<>0 then
  begin
   Log_add('Error open file : "'+file_name+'", IOResult = '+inttostr(result));
   new_State:=link_idle;
   exit;
  end
 else
  Log_add('"'+file_name+'" open ok');

 f_connected:=false;
 f_disconnected:=false;
 f_error:=false;

 v_error_code:=0;

 txed_fifo.reset;
 tx_fifo.reset;
 rx_fifo.reset;

 new_State:=link_establish;
 onConnect_private;
end;

procedure tLinkClient.file_run;
var
 readed:integer;
 size:integer;
 result:integer;
begin
 tx_fifo.reset;
 txed_fifo.reset;

 {$I-}
 size := Min(length(rx2_buf), file_block_size);
 BlockRead(file_main, rx2_buf[0], size, readed);
 {$I+}

 result:=IOResult;
 if result<>0 then
  begin
   Log_add('Error read file : "'+file_name+'", IOResult = '+inttostr(result));
   sleep(500);
   new_state := link_close;
  end;

 if onFastRx_private(self, @(rx2_buf[0]), readed) then
  do_rx_event(readed)
 else
  begin
   inc(stat_readed, readed);
   inc(stat_read_speed, readed);
  end;

 if readed<>size then
  begin
   Log_add('EOF');
   sleep(2000);
   new_state := link_close;
   exit;
  end;
  
 sleep(file_block_delay);
end;

procedure tLinkClient.file_close;
var
 result:integer;
begin
 {$I-}
 CloseFile(file_main);
 {$I+}
 result:=IOResult;
 if result<>0 then
  Log_add('Error close file : "'+file_name+'", IOResult = '+inttostr(result))
 else
  Log_add('"'+file_name+'" close ok');

 file_name := '';
 file_block_size  := 0;
 file_block_delay := 1;

 log_add('Closed');

 new_State:=link_idle;
 onDisconnect_private;
end;

procedure tLinkClient.Execute;
begin
 if self=nil then exit;
 net_tread_id:=GetCurrentThreadId;

 while not Terminated do
  begin

   if file_name <> '' then
    case State of
     link_idle:      run_idle;
     link_open:      file_open;
     link_establish: file_run;
     link_close:     file_close;
     link_error:     run_error;
    end
   else
    begin
     case State of
      link_idle:      run_idle;
      link_open:      run_open;
      link_establish: run_establish;
      link_close:     run_close;
      link_error:     run_error;
     end;
     if state<>link_establish then sleep(1);
    end;

   inc(stat_iterations);
  end;
end;

procedure tLinkClient.stat_reset_speed;
begin
 if self=nil then exit;
 stat_read_speed:=0;
 stat_write_speed:=0;
end;

procedure tLinkClient.stat_reset_all;
begin
 if self=nil then exit;

 stat_reset_speed;
 
 stat_readed := 0;
 stat_writed := 0;
 tx_fifo.stat_reset;
 rx_fifo.stat_reset;
 txed_fifo.stat_reset;
 log_fifo.stat_reset;
end;

function tLinkClient.tx_free_bytes:integer;
begin
 result:=tx_fifo.data_free;
end;

function tLinkClient.tx_free_blocks:integer;
begin
 result:=tx_fifo.blocks_free;
end;

function tLinkClient.tx_free_ratio:integer;
begin
 result:=tx_fifo.free_ratio;
end;

function tLinkClient.onFastRx_private(sender:tLinkClient; data:pbyte; size:integer):boolean;
begin
 result:=true;
end;

function tLinkClient.onFastPreCheck_private(var read_ready:boolean; var write_ready:boolean):boolean;
begin
 result:=true;
end;

function tLinkClient.onFastPostCheck_private(old_read_ready:boolean; old_write_ready:boolean; var read_ready:boolean; var write_ready:boolean):boolean;
begin
 result:=true;
end;

procedure tLinkClient.onConnect_private;
begin
end;

procedure tLinkClient.onDisconnect_private;
begin
end;

procedure tLinkClient.onTick_private;
begin
end;

procedure tLinkClient.onTimerTick;
begin
end;

function tLinkClient.TreadString : string; 
begin
 result := '';
 if self = nil then exit;
 
 result := inttostr(net_tread_id);
end;


end.
