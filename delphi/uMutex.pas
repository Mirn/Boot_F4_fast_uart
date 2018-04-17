unit umutex;

interface
uses Windows, SysUtils;

type
  tMutexLog = procedure(msg:string) of object;

  tMutex = class
  private
   onLog  : tMutexLog;
   handle : cardinal;
   f_name : string;

   procedure log(s:string);
   function name_local : string;
   function name_global : string;
   function name_current : string;

  public
   constructor create(v_name : string; v_onLog : tMutexLog = nil);
   destructor destroy; override;

   function open(create_new : boolean = false) : boolean;
   procedure close;

   procedure Lock;
   procedure UnLock;

   property name : string  read name_current;
  end;

var
 open_mutex_local : boolean;

implementation
uses
 AccCtrl, AclAPI;

constructor tMutex.create(v_name : string; v_onLog : tMutexLog = nil);
begin
 inherited create;

 handle := 0;

 f_name := 'SpRecordNEW_'+self.ClassName+'_'+v_name;
 onLog := v_onLog;
end;

destructor tMutex.destroy;
begin
 if self = nil then exit;
 if handle <> 0 then
  self.close;

 inherited destroy;
end;

procedure tMutex.log(s:string);
begin
 if self = nil then exit;
 if @onLog = nil then exit;
 onLog(self.ClassName+#9+s);
end;

function tMutex.name_local : string;
begin
 if self = nil then exit;
 result := 'Local\'+f_name;
end;

function tMutex.name_global : string;
begin
 if self = nil then exit;
 result := 'Global\'+f_name;
end;

function tMutex.name_current : string;
begin
 if self = nil then exit;
 if open_mutex_local then
  result :=name_local
 else
 result :=name_global;
end;

function tMutex.open(create_new : boolean = false) : boolean;
var
 error_code : cardinal;
begin
 result := false;
 if self = nil then exit;
 self.close;

 handle := CreateMutex(nil, false, pchar(name_current));
 if handle = 0 then
  begin
   log('ERROR: tMutex "'+name_current+'" create error: '+SysErrorMessage(GetLastError));
   result := false;
   exit;
  end;

 if (GetLastError = ERROR_ALREADY_EXISTS) and create_new then
  begin
   log('ERROR: tMutex "'+name_current+'" param create_new = TRUE, '+SysErrorMessage(GetLastError));
   self.close;
   result := false;
   exit;
  end;

 error_code := SetSecurityInfo(handle,
        SE_KERNEL_OBJECT,
        DACL_SECURITY_INFORMATION,
        nil,
        nil,
        nil,
        nil);

 if error_code <> ERROR_SUCCESS then
  log('ERROR: Map file of "'+name_current+'" SetSecurityInfo return : '+inttostr(error_code));

 result := true;
end;

procedure tMutex.close;
begin
 if self = nil then exit;

 if handle <> 0 then
  if not CloseHandle(handle) then
   log('ERROR: tMutex "'+name_current+'" CloseHandle return false: '+SysErrorMessage(GetLastError));
 handle := 0;
end;

procedure TMutex.Lock;
var
 result : cardinal;
begin
 if self = nil then exit;
 if handle = 0 then
  begin
   log('Lock with handle = 0');
   exit;
  end;

 Result := WaitForSingleObject(handle, 2000);
 if result = WAIT_OBJECT_0 then exit;

 if result = WAIT_ABANDONED then
  log('Lock WAIT_ABANDONED')
 else
  if result = WAIT_TIMEOUT then
   log('Lock WAIT_TIMEOUT')
  else
   raise Exception.Create('Mutex lock error'); //WAIT_FAILED and ect
end;

procedure TMutex.UnLock;
begin
 if self = nil then exit;
 if handle = 0 then
  begin
   log('UnLock with handle = 0');
   exit;
  end;

 if not ReleaseMutex(Handle) then
  raise Exception.Create('Mutex unlock error')
end;

end.