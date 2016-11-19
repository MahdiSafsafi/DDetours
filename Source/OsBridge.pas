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

  { Functions }
function GetCurrentThreadId: TThreadID;
procedure QuerySystemInfo(var SystemInfo: TSystemInfo);
function QueryMemoryInfo(Address: Pointer; var Info: TMemoryInfo): Boolean;

implementation

{$IF DEFINED (MSWINDOWS)}
{$I Win32Api.inc}
{$ELSE IF DEFINED (POSIX)}
{$I POSIXApi.inc}
{$ENDIF}

function GetCurrentThreadId: TThreadID;
begin
{$IF DEFINED (POSIX)}
  Result := pthread_self();
{$ELSE IF DEFINED (MSWINDOWS)}// Windows
  Result := Windows.GetCurrentThreadId();
{$ENDIF}
end;

{$IFDEF POSIX}

function GetNumberOfProcessors: Integer;
const
  UNIX_SYS_CPU = '/sys/devices/system/cpu/';
var
  DirList: TStringList;
  Regx: TRegExpression;
  I: Integer;
  sCPU: String;
begin
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

function QueryMemoryInfo(Address: Pointer; var Info: TMemoryInfo): Boolean;
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
  Result := False;
  if not Assigned(Address) then
    Exit;
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

{$ELSE IF DEFINED (MSWINDOWS)}

function QueryMemoryInfo(Address: Pointer; var Info: TMemoryInfo): Boolean;
var
  mbi: TMemoryBasicInformation;
begin
  Result := False;
  if not Assigned(Address) then
    Exit;
  Result := VirtualQuery(Address, mbi, SizeOf(mbi)) > 0;
  if Result then
  begin
    Info.StartAddress := mbi.BaseAddress;
    Info.Size := mbi.RegionSize;
    Info.Protection := mbi.Protect;
  end;
end;

{$ENDIF POSIX}

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

end.
