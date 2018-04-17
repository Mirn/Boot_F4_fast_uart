unit uMAP_File;

interface
uses Windows, SysUtils;

type
  tMapFileLog = procedure(msg:string) of object;

  tMapFile = class
  private
   onLog : tMapFileLog;

   handle : cardinal;

   f_name : string;
   f_data : pointer;
   f_size : integer;
   f_created : boolean;

   procedure log(s:string);
   function name_local : string;
   function name_global : string;
   function name_current : string;

  public
   constructor create(v_name:string; v_size:integer; v_onLog:tMapFileLog=nil);
   destructor destroy; override;

   function open(create_new:boolean=false; write:boolean=true) : boolean;
   procedure close;

   property name : string  read name_current;
   property data : pointer read f_data;
   property size : integer read f_size;
   property is_created : boolean read f_created;
  end;

var
 open_map_local : boolean;

implementation
uses AccCtrl, AclAPI;

constructor tMapFile.create(v_name : string; v_size : integer; v_onLog : tMapFileLog = nil);
begin
 inherited create;

 f_data := nil;
 handle := 0;

 f_name := 'SpRecordNEW_'+self.ClassName+'_'+v_name;
 f_size := v_size;
 onLog := v_onLog;
end;

destructor tMapFile.destroy;
begin
 if self = nil then exit;

 if handle <> 0 then
  self.close;

 inherited destroy;
end;

procedure tMapFile.log(s:string);
begin
 if self = nil then exit;
 if @onLog = nil then exit;
 onLog(self.ClassName+#9+s);
end;

function tMapFile.name_local : string;
begin
 if self = nil then exit;
 result := 'Local\'+f_name;
end;

function tMapFile.name_global : string;
begin
 if self = nil then exit;
 result := 'Global\'+f_name;
end;

function tMapFile.name_current : string;
begin
 if self = nil then exit;
 if open_map_local then
  result :=name_local
 else
 result :=name_global;
end;

function tMapFile.open(create_new : boolean = false; write : boolean = true) : boolean;
var
 protect_create : cardinal;
 protect_map : cardinal;
 error_code : cardinal;

{ SIDAuthWorld : SID_IDENTIFIER_AUTHORITY;
 everyone_sid : PSID;
 ea  : EXPLICIT_ACCESS;
 acl : PACL;
 sd  : PSECURITY_DESCRIPTOR;
 sa  : SECURITY_ATTRIBUTES;
 error : cardinal;  }

const
 SECURITY_WORLD_SID_AUTHORITY : _SID_IDENTIFIER_AUTHORITY = (value:(0,0,0,0,0,1));

label
 go_create;
begin
 result := false;
 if self = nil then exit;
 self.close;


{ SIDAuthWorld := SECURITY_WORLD_SID_AUTHORITY;
 everyone_sid := Nil;
 if not AllocateAndInitializeSid(SIDAuthWorld, 1, 0,
   0, 0, 0, 0, 0, 0, 0, everyone_sid) then
  log('ERROR: Map file of "'+name_current+'" AllocateAndInitializeSid: '+SysErrorMessage(GetLastError));

 ZeroMemory(@ea, sizeof(ea));
 ea.grfAccessPermissions := SPECIFIC_RIGHTS_ALL or STANDARD_RIGHTS_ALL;
 ea.grfAccessMode := SET_ACCESS;
 ea.grfInheritance := NO_INHERITANCE;
 ea.Trustee.TrusteeForm := TRUSTEE_IS_SID;
 ea.Trustee.TrusteeType := TRUSTEE_IS_WELL_KNOWN_GROUP;
 ea.Trustee.ptstrName  := everyone_sid;

 acl := nil;
 error := SetEntriesInAcl(1, @ea, nil, acl);
 if error <> ERROR_SUCCESS then
  log('ERROR: Map file of "'+name_current+'" InitializeSecurityDescriptor error: '+inttostr(error));

 sd := 0;
 sd := PSECURITY_DESCRIPTOR(LocalAlloc(LPTR, SECURITY_DESCRIPTOR_MIN_LENGTH));
 if not InitializeSecurityDescriptor(sd, SECURITY_DESCRIPTOR_REVISION) then
  log('ERROR: Map file of "'+name_current+'" InitializeSecurityDescriptor: '+SysErrorMessage(GetLastError));
 if not SetSecurityDescriptorDacl(sd, TRUE, acl, FALSE) then
  log('ERROR: Map file of "'+name_current+'" SetSecurityDescriptorDacl: '+SysErrorMessage(GetLastError));

 sa.nLength := sizeof(SECURITY_ATTRIBUTES);
 sa.lpSecurityDescriptor := sd;
 sa.bInheritHandle := FALSE;   }

 if write then
  begin
   protect_map    := FILE_MAP_ALL_ACCESS;
   protect_create := SEC_COMMIT or PAGE_READWRITE
  end
 else
  begin
   protect_map    := FILE_MAP_READ;
   protect_create := SEC_COMMIT or PAGE_READONLY;
  end;

 if create_new then
  begin
go_create:
   handle := CreateFileMapping(INVALID_HANDLE_VALUE, nil, protect_create, 0, f_size, pchar(name_current));
   if handle = 0 then
    begin
     log('ERROR: Map file of "'+name_current+'" create error: '+SysErrorMessage(GetLastError));
     result := false;
     exit;
    end
   else
    f_created := true;
  end
 else
  begin
   handle := OpenFileMapping(protect_map, false, pchar(name_current));
   if handle = 0 then
    goto go_create
   else
    f_created := false;
  end;

 if (GetLastError = ERROR_ALREADY_EXISTS) and create_new then
  begin
   log('ERROR: Map file of "'+name_current+'" param create_new = TRUE, '+SysErrorMessage(GetLastError));
   self.close;
   result := false;
   exit;
  end;

{ FreeSid(everyone_sid);
 LocalFree(cardinal(sd));
 LocalFree(cardinal(acl));}

 error_code:= SetSecurityInfo(handle,
        SE_KERNEL_OBJECT,
        DACL_SECURITY_INFORMATION,
        nil,
        nil,
        nil,
        nil);
 if error_code <> ERROR_SUCCESS then
  log('ERROR: Map file of "'+name_current+'" SetSecurityInfo return : '+inttostr(error_code));

 f_data := nil;
 f_data :=  MapViewOfFile(handle, protect_map, 0, 0, f_size);
 if not Assigned(f_data) then
  begin
   log('ERROR: Map file of "'+name_current+'" MapViewOfFile return NULL: '+SysErrorMessage(GetLastError));
   self.close;
   result := false;
   exit;
  end;
 result := true;
end;

procedure tMapFile.close;
begin
 if self = nil then exit;
 if f_data <> nil then
  if not UnmapViewOfFile(f_data) then
   log('ERROR: Map file of "'+name_current+'" UnmapViewOfFile return false: '+SysErrorMessage(GetLastError));
 f_data := nil;

 if handle <> 0 then
  if not CloseHandle(handle) then
   log('ERROR: Map file of "'+name_current+'" CloseHandle return false: '+SysErrorMessage(GetLastError));
 handle := 0;

 f_created := false;
end;

end.
