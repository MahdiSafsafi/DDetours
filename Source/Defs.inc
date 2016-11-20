
{
-CPU32BITS = CPU is in a 32-bit environment. 
-CPU64BITS = CPU is in a 64-bit environment.
-CPUX86,CPUX32 = Intel 386 or later.
-CPUX64 = x86-64 CPU.
-X86ASM = x86 assembly.
-UseSimpleLock = Use simple lock based on the hardware implementation.
-UseCriticalsection = Use FPC or Windows critical section.
-UseMonitor = Use Delphi Monitor.
}

{$IFNDEF FPC}
	
	{$IF CompilerVersion >= 22}
		{$DEFINE DELPHI_XE_UP }
	{$IFEND}
	
{$ELSE FPC}
	
	{$IFDEF CPUi386} // CPU386
		{$IFDEF CPU32}
			{$DEFINE CPUX86} // x86-32
		{$ELSE IF DEFINED(CPU64}
			{$DEFINE CPUX64} // x86-64
		{$ENDIF CPU32}
	{$ENDIF CPUi386}
	
	{$IFDEF UNIX}
		{$DEFINE POSIX}
	{$ENDIF UNIX}
	
{$ENDIF !FPC}

{$IFDEF CPUX86}
	{$DEFINE CPUX32}
{$ENDIF CPUX86}

{$IFNDEF CPU32BITS}
	{$IF DEFINED(CPU32) OR DEFINED(CPUX32)}
		{$DEFINE CPU32BITS}
	{$ENDIF}
{$ENDIF !CPU32BITS}

{$IFNDEF CPU64BITS}
	{$IF DEFINED(CPU64) OR DEFINED(CPUX64)}
		{$DEFINE CPU64BITS}
	{$ENDIF}
{$ENDIF !CPU64BITS}

{$IF DEFINED(CPUX32) OR DEFINED(CPUX64)}
	{$DEFINE X86ASM}
{$ENDIF}

{$IF DEFINED (X86ASM)}
	{$DEFINE UseSimpleLock}
{$ELSEIF DEFINED(FPC)}
	{$DEFINE UseCriticalsection}
{$ELSE} // Delphi
	{$DEFINE UseMonitor}
{$ENDIF X86ASM}
