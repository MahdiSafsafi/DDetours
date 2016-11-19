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

implementation

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
