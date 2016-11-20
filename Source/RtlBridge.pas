unit RtlBridge;

// https://github.com/MahdiSafsafi/delphi-detours-library

{
  Runtime bridge between FPC and Delphi compiler.
  This allows cross compiling under Delphi and FPC.
}

{$I Defs.inc}
{$IFDEF DELPHI_XE4_UP}
{$LEGACYIFEND ON}
{$ENDIF DELPHI_XE4_UP}

interface

uses
  SysUtils,
  Classes,
  Types,
{$IFDEF FPC}
  RegExpr,
  FileUtil,
  LazFileUtils,
{$ELSE !FPC}    // Delphi
{$IFDEF DELPHI_2010_UP}
  IOUtils,
  RegularExpressions,
{$ENDIF DELPHI_2010_UP}
{$ENDIF FPC}
  CommonTypes;

type
  TLock = record
{$IFDEF UseCriticalsection}
    Criticalsection: TRTLCriticalSection;
{$ELSE !UseCriticalsection}
    case Boolean of
      False: (LockValue: NativeUInt);
      True: (LockObj: TObject);
{$ENDIF UseCriticalsection}
  end;

{$IFDEF FPC}

  TRegExpression = TRegExpr;
{$ELSE !FPC}
{$IFDEF DELPHI_2010_UP}

  TRegExpression = class(TObject)
  private
    FRegExp: TRegEx;
    FMatch: TMatch;
    function GetMatch(Index: Integer): string;
  public
    constructor Create(const Expression: string); virtual;
    destructor Destroy; override;
    function Exec(const InputString: string): Boolean;
    property Match[Index: Integer]: string read GetMatch;
  end;
{$ENDIF DELPHI_2010_UP}
{$ENDIF FPC}

procedure FindAllDirectories(const Path: string; AList: TStringList; WalkSubDirectories: Boolean);
procedure InitLock(var LockStruct: TLock);
procedure FinalizeLock(var LockStruct: TLock);
procedure Lock(var LockStruct: TLock);
procedure Unlock(var LockStruct: TLock);

implementation

{$IFDEF FPC}
// {$MODE Delphi}
{$ASMMODE intel}
{$ENDIF FPC}
{$IFDEF UseSimpleLock}

function BoolCmpExchange(Address: Pointer; CompareValue, NewValue: Boolean): Boolean;{$IFDEF FPC} assembler; {$ENDIF FPC}
asm
  {$IFDEF CPUX32}
  { eax = Address
  cl    =  CompareValue
  dl    =  NewValue }
  xchg eax,ecx
  lock cmpxchg byte [ecx],dl
  {$ELSE !CPUX32}
  { rcx = Address
  dl    = CompareValue
  r8b   = NewValue }
  mov al,dl
  lock cmpxchg byte [rcx],r8b
  {$ENDIF CPUX32}
end;
{$ENDIF UseSimpleLock}

procedure InitLock(var LockStruct: TLock);
begin
{$IF DEFINED (UseMonitor)}
  LockStruct.LockObj := TObject.Create;
{$ELSEIF DEFINED (UseCriticalsection)}
{$IF DEFINED (FPC)}
  InitCriticalSection(LockStruct.Criticalsection);
{$ELSEIF DEFINED(MSWINDOWS)}
  InitializeCriticalSection(LockStruct.Criticalsection);
{$IFEND FPC}
{$ELSEIF DEFINED (UseSimpleLock)}
  LockStruct.LockValue := 0;
{$IFEND UseMonitor}
end;

procedure FinalizeLock(var LockStruct: TLock);
begin
{$IF DEFINED (UseMonitor)}
  FreeAndNil(LockStruct.LockObj);
{$ELSEIF DEFINED (UseCriticalsection)}
{$IF DEFINED (FPC)}
  DoneCriticalSection(LockStruct.Criticalsection);
{$ELSEIF DEFINED (MSWINDOWS)}
  DeleteCriticalSection(LockStruct.Criticalsection);
{$IFEND FPC}
{$ELSEIF DEFINED (UseSimpleLock)}
  LockStruct.LockValue := 0;
{$IFEND UseMonitor}
end;

procedure Lock(var LockStruct: TLock);
{$IFDEF UseSimpleLock}
const
  SleepTime = 20;
{$ENDIF UseSimpleLock}
begin
{$IF DEFINED (UseMonitor)}
  System.TMonitor.Enter(LockStruct.LockObj);
{$ELSEIF DEFINED (UseCriticalsection)}
{$IF DEFINED (FPC)}
  System.EnterCriticalsection(LockStruct.Criticalsection);
{$ELSEIF DEFINED(MSWINDOWS)}
  Windows.EnterCriticalsection(LockStruct.Criticalsection);
{$ELSE}
  Error();
{$IFEND FPC}
{$ELSEIF DEFINED (UseSimpleLock)}
  if IsMultiThread then
  begin
    while BoolCmpExchange(@LockStruct.LockValue, False, True) do
    begin
      Sleep(SleepTime);
    end;
  end;
{$ELSE}
  Error();
{$IFEND UseMonitor}
end;

procedure Unlock(var LockStruct: TLock);
begin
{$IF DEFINED (UseMonitor)}
  System.TMonitor.Exit(LockStruct.LockObj);
{$ELSEIF DEFINED (UseCriticalsection)}
{$IF DEFINED (FPC)}
  System.LeaveCriticalSection(LockStruct.Criticalsection);
{$ELSEIF DEFINED(MSWINDOWS)}
  Windows.LeaveCriticalSection(LockStruct.Criticalsection);
{$IFEND FPC}
{$ELSEIF DEFINED (UseSimpleLock)}
  LockStruct.LockValue := 0;
{$IFEND UseMonitor}
end;

procedure FindAllDirectories(const Path: string; AList: TStringList; WalkSubDirectories: Boolean);
{$IF NOT DEFINED (FPC) AND DEFINED (DELPHI_2010_UP)}
var
  LArray: TStringDynArray;
  i: Integer;
  Dir: string;
  Option: TSearchOption;
{$IFEND}
begin
{$IF DEFINED (DELPHI_2010_UP)}
  if WalkSubDirectories then
    Option := TSearchOption.soAllDirectories
  else
    Option := TSearchOption.soTopDirectoryOnly;
  LArray := TDirectory.GetDirectories(Path, Option,
    function(const Path: string; const SearchRec: TSearchRec): Boolean
    begin
{$WARNINGS OFF}
      Result := SearchRec.Attr and (faHidden) = $00;
{$WARNINGS ON}
    end);
  for i := 0 to Length(LArray) - 1 do
  begin
    Dir := LArray[i];
    AList.Add(Dir);
  end;
{$ELSEIF DEFINED (FPC)}
  FileUtil.FindAllDirectories(AList, Path, WalkSubDirectories);
{$ELSE}
  //Error;
{$IFEND}
end;

{$IFDEF DELPHI_2010_UP}
{ TRegExpression }

constructor TRegExpression.Create(const Expression: string);
begin
  FRegExp := TRegEx.Create(Expression);
end;

destructor TRegExpression.Destroy;
begin
  inherited;
end;

function TRegExpression.Exec(const InputString: string): Boolean;
begin
  FMatch := FRegExp.Match(InputString);
  Result := FMatch.Success;
end;

function TRegExpression.GetMatch(Index: Integer): string;
begin
  Result := FMatch.Groups[Index].Value;
end;
{$ENDIF DELPHI_2010_UP}

end.
