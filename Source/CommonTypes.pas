unit CommonTypes;

// https://github.com/MahdiSafsafi/delphi-detours-library

{
  CommonTypes => This allows compatibility between
  different Delphi compilers versions.
}

interface

{$I Defs.inc}

type
  TThreadPriority = LongInt;
  TThreadPolicy = LongInt;
{$IFNDEF FPC}
{$IFNDEF DELPHI_2010_UP}
  SIZE_T = NativeUInt;
  TThreadID = LongWord;
{$ENDIF !DELPHI_2010_UP}
{$ENDIF !FPC}

implementation

end.
