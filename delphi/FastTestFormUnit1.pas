unit FastTestFormUnit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  CRCunit,
  comclient, linkclient, ExtCtrls,
  stm32_SFU;

type
  TForm1 = class(TForm)
    MemoDevice: TMemo;
    Timer1mS: TTimer;
    MemoCMD: TMemo;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Timer1mSTimer(Sender: TObject);
  private
    device : tCOMClient;
    sfu : tstm32_sfu;

    procedure onLog(sender:tobject; msg:string);
    procedure onInfoString(sender:tobject; msg:string);
    procedure onLogBegin(sender:tLinkClient);
    procedure onLogEnd(sender:tLinkClient);

    procedure onCommand(code:byte; body:pbyte; count:integer);

  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
 device := tCOMClient.Create($4000, $4000);

 device.onLog := self.onLog;
 device.onLogBegin := self.onLogBegin;
 device.onLogEnd := self.onLogEnd;

 device.port_name_serial := true;
 device.port_name := 'GM18_E_0010';
 device.port_speed := 500000;
 device.port_parity := NOPARITY;
 device.no_activate := true;

 sfu := tstm32_sfu.create;
 sfu.onWrite := device.Write;
 sfu.onLog := self.onLog;
 sfu.onInfoString := self.onInfoString;
 sfu.onCommand := self.onCommand;

 device.onRX := sfu.process_recive;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
 device.Open;
 randomize;
 MemoDevice.Lines.Add('randseed = ' + IntToStr(RandSeed));
 MemoDevice.Lines.Add('');
end;

procedure TForm1.onInfoString(sender:tobject; msg:string);
begin
 MemoDevice.Lines.Add('SFU: ' + msg);
end;

procedure TForm1.onLog(sender:tobject; msg:string);
begin
 MemoDevice.Lines.Add(msg);
end;

procedure TForm1.onLogBegin(sender:tLinkClient);
begin
 MemoDevice.Lines.BeginUpdate;
end;

procedure TForm1.onLogEnd(sender:tLinkClient);
begin
 MemoDevice.Lines.EndUpdate;
end;

procedure TForm1.onCommand(code:byte; body:pbyte; count:integer);
var
 str : string;
begin
 str := '';
 setlength(str, count);
 move(body^, str[1], count);
 MemoCMD.Lines.Add('CMD: ' + str);
end;

procedure rand_array(buf:pbyte; cnt:cardinal);
begin
 while cnt > 0 do
  begin
   buf^ := random(256);
   inc(buf);
   dec(cnt);
  end;
end;

procedure TForm1.Timer1mSTimer(Sender: TObject);
var
 body : array[0..$1000-12] of byte;
 size : integer;
 code : byte;
begin
 if device.State <> link_establish then exit;

 while device.tx_free_bytes > sizeof(body)*2 do
  begin
   size := random(sizeof(body) div 4) * 4;
   code := random(256);
   rand_array(@body[0], size);

   sfu.send_command(code, @body[0], size);
  end;
end;

end.
