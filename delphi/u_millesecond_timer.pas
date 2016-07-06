unit u_millesecond_timer;

interface
uses windows, sysutils;

type
 tmillisecond_timer = record
  start:int64;
  millisecond:double;
 end;

procedure milliseconds_start(var timer:tmillisecond_timer);
function  milliseconds_get(var timer:tmillisecond_timer):double;
function  milliseconds_str(var timer:tmillisecond_timer):string;

implementation

procedure milliseconds_start(var timer:tmillisecond_timer);
var
 freq:int64;
begin
 with timer do
  begin
   QueryPerformanceFrequency(freq);
   QueryPerformanceCounter(start);
   millisecond := freq / 1000;
  end;
end;

function milliseconds_get(var timer:tmillisecond_timer):double;
var
 current:int64;
begin
 result := 0;
 with timer do
  begin
   QueryPerformanceCounter(current);

   if millisecond = 0 then
    result := 0
   else
    result:=(current-start)/millisecond;
  end;
end;

function milliseconds_str(var timer:tmillisecond_timer):string;
begin
 result:= FloatTostrf(milliseconds_get(timer), ffFixed,4,3);
end;

end.
