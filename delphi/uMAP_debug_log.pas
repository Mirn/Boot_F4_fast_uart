unit uMAP_debug_log;

interface
uses windows, sysutils, math, classes,
 uFIFO_map,
 uMAP_file,
 u_millesecond_timer;

type
 tMAP_debug_log = class(tstringlist)
 public
  log_name  : string;
  log_title : string;
 end;

 tMAP_debug_string_event = procedure(list:tMAP_debug_log; num:integer; name:string; msg:string) of object;
 tMAP_deubg_new_event = procedure(list:tMAP_debug_log; num:integer; name:string) of object;

 tMAP_debug_main = class(tobject)
 private
  timer : tmillisecond_timer;

  log_file   : file;
  log_enable : boolean;

  title_countout : integer;

  f_log_name  : string;
  f_log_title : string;

  f_list: tlist;

  procedure file_open;
  function  file_get_name(filename:string; dirname:string):string;
  procedure file_write(msg:string);
  procedure file_close;

  procedure fifo_send_title;
  procedure fifo_send(msg:string);

  function list_get(index:integer):tMAP_debug_log;
  function list_get_count:integer;
  procedure list_add(name:string; msg:string; istitle : boolean);

  function get_time_str:string;

 public
  fifo  : tFIFO_map;
  num_width : integer;

  evMessage : tMAP_debug_string_event;
  evTitle   : tMAP_debug_string_event;
  evNewLog  : tMAP_deubg_new_event;

  constructor create(v_log_name:string; v_title:string; evlog:tMapFileLog);
  destructor destroy;override;

  function create_log : boolean;
  function open_logs  : boolean;

  procedure send(msg:string);
  procedure send_common(msg:string);
  procedure send_nums(nums : array of const);

  procedure recive_check;

  property log_name:string  read f_log_name;
  property log_title:string read f_log_title;

  property list[index:integer]:tMAP_debug_log read list_get;
  property list_count:integer read list_get_count;
 end;

implementation

{$I-}

const
             //0123456701234567 !!!!!!!!! need: size mod 8 = 0 for tabs
            //'00:00:00 '
 time_title = 'Time    ';
            //'000.00'
 ms_title   = ' dlt_T';
 add_title = time_title + ms_title;

 key_title   = '#`@~';
 key_message = ':@~`';
 common_name = '<no_name>';

procedure fix_length(var s:string; len:integer);
begin
 if length(s) > len then
  begin
//   if len = 1 then
   s := copy(s,1, len)
//   else
//    s := copy(s,1, len-1)+'~';
  end
 else
  while length(s) < len do
   s := s + ' ';
end;

constructor tMAP_debug_main.create(v_log_name:string; v_title:string; evlog:tMapFileLog);
var
 name : string;
begin
 inherited create;

 name := ExtractFileName(ParamStr(0));
 if pos('.exe', name) > 0 then
  delete(name, length(name)-3, 4);

 f_log_name  := v_log_name; // f_log_name  := name + '#' + v_log_name;
 f_log_title := add_title + '  ' + v_title;

 fifo := tFIFO_map.create('Debug_Main', $40000, evLog);
end;

destructor tMAP_debug_main.destroy;
var
 k : integer;
begin
 send_common('Debug log close, name = "' + log_name + '", title = "' + log_title + '"');
 fifo_send(get_time_str + '================ DISCONNECT ================');
 fifo_send(' ');

 file_close;
 fifo.close;
 fifo.Free;

 if f_list <> nil then
  begin
   for k:=0 to list_count-1 do
    (list[k] as tMAP_debug_log).Free;
   f_list.Free;
   f_list := nil;
  end;

 inherited destroy;
end;

function  tMAP_debug_main.file_get_name(filename:string; dirname:string):string;
var
 s : string;
 k : integer;
// old_dir : string;
begin
 s:=DateToStr(Date)+'_'+timetostr(Time);
 for k:=1 to length(s)-1 do
  if not(s[k] in ['0'..'9']) then s[k]:='_';

{ old_dir := GetCurrentDir; IOResult;
 chdir(ExtractFilePath(ParamStr(0))); IOResult;
 MkDir(dirname); IOResult;
 chdir(old_dir); IOResult;      }

 result:=dirname+'\'+s+'_'+filename;
end;

procedure tMAP_debug_main.file_open;
begin
 log_enable := false;
 AssignFile(log_file, file_get_name(log_name, 'logs_m'));
 Rewrite(log_file, 1);
 log_enable := ioresult = 0;
end;

procedure tMAP_debug_main.file_write;
begin
 if not log_enable then exit;

 msg := msg + #13#10;
 BlockWrite(log_file, msg[1], length(msg)*sizeof(msg[1]));
 log_enable := IOResult = 0;
end;

procedure tMAP_debug_main.file_close;
begin
 CloseFile(log_file); IOResult;
 log_enable := false;
end;

function tMAP_debug_main.create_log:boolean;
begin
 result := false;

 if not fifo.open_as_writer then exit;

 file_open;
 file_write(log_name);
 file_write(log_title);
 file_write('');

 send_common('Debug log open,  name = "' + log_name + '", title = "' + log_title + '"');
 fifo_send_title;
 fifo_send(' ');
 fifo_send(get_time_str + '================ CONNECT ================');
 title_countout := 0;
 result := true;
end;

function tMAP_debug_main.open_logs:boolean;
var
 new_log : tMAP_debug_log;
begin
 result := false;

 if not fifo.open_as_writer then exit;

 f_list := tlist.Create;

 new_log := tMAP_debug_log.Create;
 new_log.log_name := common_name;
 new_log.log_title := add_title + '  ================ Message ================';
 f_list.Add(new_log);

 result := true;
end;

procedure tMAP_debug_main.recive_check;
var
 old_rx : int64;

 pos_message : integer;
 pos_title : integer;

 name : string;
 msg : string;
 istitle : boolean;

 timeout : cardinal;
begin
 if self = nil then exit;
 if f_list = nil then exit;

 old_rx := fifo.stat.readed.count;
 timeout := GetTickCount + 100;
 while ((fifo.stat.readed.count - old_rx) < fifo.bytes_size) and (GetTickCount < timeout) do
  begin
   if not fifo.read_string(msg) then break;

   pos_message := pos(key_message, msg);
   pos_title   := pos(key_title, msg);

   if (pos_message = 0) and (pos_title = 0) then
    begin
     list_add(common_name, msg, false);
     continue;
    end;

   if (pos_message <> 0) and (pos_title <> 0) then
    begin
     if pos_message <= pos_title then
      pos_title := 0
     else
      pos_message := 0;
    end;

   istitle := pos_title <> 0;
   pos_message := pos_message or pos_title;
   name := copy(msg, 1, pos_message-1);
   delete(msg, 1, pos_message + length(key_message)-1);

   list_add(name, msg, istitle);
  end;
end;

procedure tMAP_debug_main.list_add(name:string; msg:string; istitle : boolean);
var
 k : integer;
 new_log : tMAP_debug_log;
begin
 for k:=0 to list_count-1 do
  if list[k].log_name = name then
   begin
    if istitle then
     begin
      list[k].log_title := msg;
      if @evTitle <> nil then
       evTitle(list[k], k, name, msg);
     end
    else
     begin
      list[k].Add(msg);
      if @evMessage <> nil then
       evMessage(list[k], k, name, msg);
     end;
    exit;
   end;

 new_log := tMAP_debug_log.Create;
 new_log.log_name := name;

 f_list.Add(new_log);
 k := f_list.IndexOf(new_log);

 if (@evNewLog <> nil) and (k >= 0) then
  evNewLog(list[k], k, name);

 if k >= 0 then
  if istitle then
   begin
    list[k].log_title := msg;
    if @evTitle <> nil then
     evTitle(list[k], k, name, msg);
   end
  else
   begin
    new_log.Add(msg);
    if @evMessage <> nil then
     evMessage(list[k], k, name, msg);
   end;
end;

function tMAP_debug_main.list_get(index:integer):tMAP_debug_log;
begin
 result := nil;
 if index >= f_list.Count then exit;
 result := tobject(f_list.Items[index]) as tMAP_debug_log;
end;

function tMAP_debug_main.list_get_count;
begin
 result := 0;
 if f_list = nil then exit;
 result := f_list.Count;
end;

procedure tMAP_debug_main.fifo_send_title;
begin
 fifo.write_string(log_name + key_title + log_title);
end;

procedure tMAP_debug_main.fifo_send(msg:string);
begin
 dec(title_countout);
 if title_countout < 0 then
  begin
   title_countout := 10;
   fifo_send_title;
  end;

 fifo.write_string(log_name + key_message + msg);
end;

function tMAP_debug_main.get_time_str:string;
var
 s : string;
 t : double;
begin
 result := '';
 if self = nil then exit;

 t := milliseconds_get(timer);
 milliseconds_start(timer);
 s:='';

 s := s + timetostr(time);
 fix_length(s, length(time_title));
 s := s + ' ';

 if t>99999 then
  s := s + '99999+'
 else
  s := s + FloatToStrF(t, ffFixed, 4, 3);
 fix_length(s, length(add_title) + 1);
 s := s + ' ';

 result := s;
end;

procedure tMAP_debug_main.send(msg : string);
var
 s : string;
begin
 if self = nil then exit;

 s := get_time_str;
 file_write(s + msg);
 fifo_send(s + msg);
end;

procedure tMAP_debug_main.send_common(msg : string);
begin
 if self = nil then exit;
 if fifo = nil then exit;

 fifo.write_string(get_time_str + msg);
end;

procedure tMAP_debug_main.send_nums(nums : array of const);
var
 msg : string;
 s : string;
 i : integer;
begin
 if self = nil then exit;
 s:='';
 msg := '';

  for I := 0 to High(nums) do
   begin
    with TVarRec(nums[I]) do
     case VType of
       vtInteger  : s := IntToStr(math.min(9999999, max(-999999, VInteger)));
       vtInt64    : s := IntToStr(math.min(9999999, max(-999999, VInt64^)));
       vtBoolean  : s := BoolToStr(VBoolean, true);
       vtChar     : s := vchar;
       vtExtended : s := FloatToStrF(vextended^, ffFixed, 4,3);
       vtString   : s := vstring^;
       vtPointer  : s := IntToHex(cardinal(VPointer), 8);
       vtPChar    : s := vpchar^;
       vtObject   : s := vobject.ClassName;
       vtClass    : s := vclass.ClassName;
     else
       s:= '???';
     end;

    if num_width = 0 then
     s:=s + #9
    else
     fix_length(s, num_width);

    msg := msg + s;
   end;

 send(msg);
end;

end.
