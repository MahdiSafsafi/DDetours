unit RtlBridge;

// https://github.com/MahdiSafsafi/delphi-detours-library

{
  Runtime bridge between compilers.
  This allows cross compiling under Delphi and FPC.
}

{$I Defs.inc}

interface

uses
  SysUtils,
  Classes,
  Types,
{$IFDEF FPC}
  RegExpr,
  FileUtil,
  LazFileUtils
{$ELSE !FPC}    // Delphi
    IOUtils,
  RegularExpressions
{$ENDIF FPC}
    ;

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

function BoolCmpExchange(Address: Pointer; CompareValue, NewValue: Boolean): Boolean; assembler;
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
{$ENDIF FPC}
{$ELSEIF DEFINED (UseSimpleLock)}
  LockStruct.LockValue := 0;
{$ENDIF}
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
{$ENDIF FPC}
{$ELSEIF DEFINED (UseSimpleLock)}
  LockStruct.LockValue := 0;
{$ENDIF}
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
{$ENDIF FPC}
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
{$ENDIF}
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
{$ENDIF FPC}
{$ELSEIF DEFINED (UseSimpleLock)}
  LockStruct.LockValue := 0;
{$ENDIF}
end;

procedure FindAllDirectories(const Path: string; AList: TStringList; WalkSubDirectories: Boolean);
{$IFNDEF FPC}
var
  LArray: TStringDynArray;
  i: Integer;
  Dir: string;
  Option: TSearchOption;
{$ENDIF !FPC}
begin
{$IFNDEF FPC}
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
{$ELSE FPC}
{$ENDIF !FPC}
end;

{$IFNDEF FPC}
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
{$ENDIF !FPC}

end.
