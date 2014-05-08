{$IFDEF FPC}
  {$IFDEF CPU64}
    {$IFNDEF CPUX64}
      {$DEFINE CPUX64}
    {$ENDIF !CPUX64}
  {$ENDIF CPU64}
{$ENDIF FPC}

{$IFNDEF CPUX64}
  {$IFNDEF CPUX86}
    {$DEFINE CPUX86}
  {$ENDIF !CPUX86}
{$ENDIF !CPUX64}

{$IFDEF DEBUG}
{$R+} // Range check On
{$ENDIF}

{$DEFINE UseInline}
{$DEFINE BuildThreadSafe}

{$IFNDEF FPC}
  {$IF CompilerVersion >17}
    {$DEFINE CanInline}
  {$IFEND}
{$IFEND !FPC}

{$IF DEFINED(UseInline) and DEFINED(CanInline)}
{$DEFINE MustInline}
{$IFEND}


