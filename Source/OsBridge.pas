unit OsBridge;

// https://github.com/MahdiSafsafi/delphi-detours-library

{
  Bridge between the DDL core and the low level functions of the OS.
  This allows for cross compatibilities between different os :
  Windows,POSIX based system (Linux,MacOS,...).
}

{$I Defs.inc}

interface

uses

{$IF DEFINED (MSWINDOWS)}
  Windows,
{$ELSE IF DEFINED(POSIX)}
  BaseUnix,
  pthreads,
{$ENDIF}
  SysUtils,
  Classes,
  RtlBridge;

const
  { Memory Protection }
  {
    For all platforms, memory protection could be
    ONLY one of the following constants !
    => This allows compatibility with Windows.
  }
{$IFDEF POSIX}
  VMP_READ = PROT_READ;
  VMP_WRITE = PROT_WRITE;
  VMP_EXECUTE = PROT_EXEC;
  VMP_READ_WRITE = VMP_READ or VMP_WRITE;
  VMP_EXECUTE_READ = VMP_EXECUTE or VMP_READ;
  VMP_EXECUTE_READ_WRITE = VMP_EXECUTE_READ or VMP_WRITE;
{$ELSE IF DEFINED(MSWINDOWS) }
  VMP_READ = PAGE_READONLY;
  VMP_WRITE = PAGE_READWRITE;
  VMP_EXECUTE = PAGE_EXECUTE;
  VMP_READ_WRITE = VMP_WRITE;
  VMP_EXECUTE_READ = PAGE_EXECUTE_READ;
  VMP_EXECUTE_READ_WRITE = PAGE_EXECUTE_READWRITE;
{$ENDIF POSIX}

type
  TSystemInfo = record
    PageSize: SIZE_T;
    NumberOfProcessors: Cardinal;
    ActiveProcessorMask: Cardinal;
  end;

  PSystemInfo = ^TSystemInfo;

  TMemoryInfo = record
    StartAddress: Pointer;
    Size: NativeUInt; // Size of region.
    Protection: Cardinal; // One of VMP_XX
  end;

  PMemoryInfo = ^TMemoryInfo;

  TThreadPriority = LongInt;
  TThreadPolicy = LongInt;

  { Functions }
function GetCurrentThreadId: TThreadID;
procedure QuerySystemInfo(var SystemInfo: TSystemInfo);
function QueryMemoryInfo(const Address: Pointer; var Info: TMemoryInfo): Boolean;
function SetMemoryPermission(const Address: Pointer; Size: SIZE_T; Permission: Cardinal; OldPermission: PCardinal): Boolean;
function GetThreadPriority(ThreadId: TThreadID; ThreadHandle: THandle; var Policy: TThreadPolicy; var Priority: TThreadPriority): Boolean;
function SetThreadPriority(ThreadId: TThreadID; ThreadHandle: THandle; Policy: TThreadPolicy; Priority: TThreadPriority): Boolean;

implementation

const
  SFeatureRequired = 'Feature required but not implemented.';

{$IF DEFINED (MSWINDOWS)}
{$I Win32Api.inc}
{$ELSE IF DEFINED (POSIX)}
{$I POSIXApi.inc}
{$ENDIF}

var
  SysInfo: TSystemInfo;

procedure Error(const SError: string);
begin
  raise Exception.Create(SError);
end;

function GetCurrentThreadId: TThreadID;
begin
{$IF DEFINED (POSIX)}
  Result := pthread_self();
{$ELSE IF DEFINED (MSWINDOWS)}// Windows
  Result := Windows.GetCurrentThreadId();
{$ENDIF}
end;

{$IFDEF POSIX}
{$IFDEF LINUX}

function GetNumberOfProcessors: Integer;
const
  UNIX_SYS_CPU = '/sys/devices/system/cpu/';
var
  DirList: TStringList;
  Regx: TRegExpression;
  I: Integer;
  sCPU: String;
begin
  { Number Of Processors = number of folders
    that their name are in this form "cpuX"
    where X is a digit representing a processor }
  Result := 0;
  Regx := TRegExpression.Create('^cpu\d+$');
  DirList := TStringList.Create;
  try
    FindAllDirectories(UNIX_SYS_CPU, DirList, False);
    for I := 0 to DirList.Count - 1 do
    begin
      sCPU := ExtractFileName(DirList[I]);
      if Regx.Exec(sCPU) then
      begin
        Inc(Result);
      end;
    end;
  finally
    Regx.Free;
    DirList.Free;
  end;
end;

function GetActiveProcessorMask: Cardinal;
const
  UNIX_CPUINFO = '/proc/cpuinfo';
var
  F: TextFile;
  Regx: TRegExpression;
  Line: String;
  Id: Integer;
begin
  Result := 0;
  { Parse "/proc/cpuinfo" file.
    NB:cpuinfo contains ONLY on-line processors. }
  { Use "sysconf(_SC_NPROCESSORS_ONLN)"
    for UNIX versions that implement sysconf function. }
  Regx := TRegExpression.Create('processor\s+:\s+(\d+)');
  AssignFile(F, UNIX_CPUINFO);
  try
    Reset(F);
    while not EOF(F) do
    begin
      ReadLn(F, Line);
      if Regx.Exec(Line) then
      begin
        Id := StrToInt(Regx.Match[1]);
        Result := Result or (1 shl Id);
      end;
    end;
  finally
    CloseFile(F);
    Regx.Free;
  end;
end;

function QueryMemoryInfo(const Address: Pointer; var Info: TMemoryInfo): Boolean;
var
  F: TextFile;
  FileName: String;
  Line: String;
  Regx: TRegExpression;
  pStart: NativeUInt;
  pEnd: NativeUInt;
  LAddress: NativeUInt;
  S: String;
  Protection: Cardinal;
begin
  { The only way to query memory info is to parse the
    "/proc/pid/maps" file. }
  Result := False;
  Protection := 0;
  LAddress := NativeUInt(Address);
  Regx := TRegExpression.Create('\s*([0-9a-f]+)-([0-9a-f]+)\s+(.+?)\s(.+?)\s(.+?)\s(.+?)\s+(.*)');
  try
    FileName := Format('/proc/%d/maps', [GetProcessID]);
    AssignFile(F, FileName);
    Reset(F);
    try
      while not EOF(F) do
      begin
        ReadLn(F, Line);
        if Regx.Exec(Line) then
        begin
{$IFDEF CPU64BITS}
          pStart := StrToInt64('$' + Regx.Match[1]);
          pEnd := StrToInt64('$' + Regx.Match[2]);
{$ELSE !CPU64BITS}
          pStart := StrToInt('$' + Regx.Match[1]);
          pEnd := StrToInt('$' + Regx.Match[2]);
{$ENDIF CPU64BITS}
          Result := (LAddress >= pStart) and (LAddress < pEnd);
          if Result then
          begin
            Info.StartAddress := Pointer(pStart);
            Info.Size := pEnd - pStart;
            S := Regx.Match[3];
            if Pos('r', S) > 0 then
              Protection := PROT_READ;
            if Pos('w', S) > 0 then
              Protection := Protection or PROT_WRITE;
            if Pos('x', S) > 0 then
              Protection := Protection or PROT_EXEC;
            Info.Protection := Protection;
            Break;
          end;
        end;
      end;
    finally
      CloseFile(F);
    end;
  finally
    Regx.Free;
  end;
end;

{$ELSE !LINUX}
Error(SFeatureRequired);
{$ENDIF LINUX}
{$ELSE IF DEFINED (MSWINDOWS)}

function QueryMemoryInfo(const Address: Pointer; var Info: TMemoryInfo): Boolean;
var
  mbi: TMemoryBasicInformation;
begin
  Result := VirtualQuery(Address, mbi, SizeOf(mbi)) > 0;
  if Result then
  begin
    Info.StartAddress := mbi.BaseAddress;
    Info.Size := mbi.RegionSize;
    Info.Protection := mbi.Protect;
  end;
end;

{$ENDIF POSIX}

function FlushInsCache(const lpBaseAddress: Pointer; dwSize: SIZE_T): Boolean;
begin
{$IF NOT DEFINED(CPUX32) AND NOT DEFINED (CPUX64)}
  { No need to update instruction cache if CPU is x86. }
{$IFDEF MSWINDOWS}
  Result := FlushInstructionCache(GetCurrentProcess, lpBaseAddress, dwSize);
{$ELSE !MSWINDOWS}
  Error(SFeatureRequired);
{$ENDIF MSWINDOWS}
{$ELSE}// x86
  Result := True;
{$ENDIF}
end;

function SetMemoryPermission(const Address: Pointer; Size: SIZE_T; Permission: Cardinal; OldPermission: PCardinal): Boolean;
var
{$IFDEF POSIX}
  Info: TMemoryInfo;
  PageSize: SIZE_T;
{$ENDIF POSIX}
  P: Pointer;
begin
  P := Address;
{$IFDEF MSWINDOWS}
  Result := VirtualProtect(P, Size, Permission, OldPermission);
{$ELSE IF DEFINED (POSIX)}
  if Assigned(OldPermission) and QueryMemoryInfo(P, Info) then
  begin
    OldPermission^ := Info.Protection; // Old Permission.
  end;
  PageSize := SysInfo.PageSize;
  P := Pointer(NativeUInt(P) and not(PageSize - 1)); // Address must be aligned to a page boundary !
  Result := mprotect(P, Size, Permission) = 0;
{$ENDIF MSWINDOWS}
  case Permission of
    VMP_EXECUTE, //
      VMP_EXECUTE_READ, //
      VMP_EXECUTE_READ_WRITE: FlushInsCache(P, Size);
  end;
end;

procedure QuerySystemInfo(var SystemInfo: TSystemInfo);
{$IFDEF MSWINDOWS}
var
  LInfo: Windows.TSystemInfo;
{$ENDIF MSWINDOWS}
begin
{$IF DEFINED (POSIX)}
  SystemInfo.PageSize := getpagesize();
  SystemInfo.NumberOfProcessors := GetNumberOfProcessors;
  SystemInfo.ActiveProcessorMask := GetActiveProcessorMask;
{$ELSE IF DEFINED (MSWINDOWS)}// Windows
  GetSystemInfo(LInfo);
  SystemInfo.PageSize := LInfo.dwPageSize;
  SystemInfo.NumberOfProcessors := LInfo.dwNumberOfProcessors;
  SystemInfo.ActiveProcessorMask := LInfo.dwActiveProcessorMask;
{$ENDIF}
end;

function GetThreadPriority(ThreadId: TThreadID; ThreadHandle: THandle; var Policy: TThreadPolicy; var Priority: TThreadPriority): Boolean;
{$IFDEF POSIX}
var
  Param: TSchedParam;
{$ENDIF POSIX}
begin
{$IFDEF MSWINDOWS}
  Priority := Windows.GetThreadPriority(ThreadHandle);
  Result := Priority <> THREAD_PRIORITY_ERROR_RETURN;
{$ELSE IF DEFINED (POSIX)}
  Result := pthread_getschedparam(ThreadId, @Policy, @Param) = 0;
  Priority := Param.sched_priority;
{$ENDIF MSWINDOWS}
end;

function SetThreadPriority(ThreadId: TThreadID; ThreadHandle: THandle; Policy: TThreadPolicy; Priority: TThreadPriority): Boolean;
{$IFDEF POSIX}
var
  Param: TSchedParam;
{$ENDIF POSIX}
begin
{$IFDEF POSIX}
  Param.sched_priority := Priority;
  Result := pthread_setschedparam(ThreadId, Policy, @Param) = 0;
{$ELSE IF DEFINED (Windows)}// Windows
  Result := Windows.SetThreadPriority(ThreadHandle, Priority);
{$ENDIF POSIX}
end;

initialization

QuerySystemInfo(SysInfo);

end.
