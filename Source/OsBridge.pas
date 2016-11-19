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

type
  TSystemInfo = record
    PageSize: SIZE_T;
    NumberOfProcessors: Cardinal;
    ActiveProcessorMask: Cardinal;
  end;

  PSystemInfo = ^TSystemInfo;

  { Functions }
function GetCurrentThreadId: TThreadID;
procedure QuerySystemInfo(var SystemInfo: TSystemInfo);

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
    FindAllDirectories(UNIX_SYS_CPU, DirList,False);
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
