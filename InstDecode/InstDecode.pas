unit InstDecode;

interface

// https://github.com/MahdiSafsafi/delphi-detours-library

{$I Defs.inc}

const
  { Instructions ID }
  { This contains only legacy instructions. }
  INST_ID_ADC = 1;
  INST_ID_ADD = 2;
  INST_ID_AND = 3;
  INST_ID_CALL = 4;
  INST_ID_CMP = 5;
  INST_ID_DEC = 6;
  INST_ID_DIV = 7;
  INST_ID_ENTER = 8;
  INST_ID_IDIV = 9;
  INST_ID_IMUL = 10;
  INST_ID_INC = 11;
  INST_ID_INT3 = 12;
  INST_ID_IRET = 13;
  INST_ID_JB = 14;
  INST_ID_JBE = 15;
  INST_ID_JL = 16;
  INST_ID_JLE = 17;
  INST_ID_JMP = 18;
  INST_ID_JNB = 19;
  INST_ID_JNBE = 20;
  INST_ID_JNL = 21;
  INST_ID_JNLE = 22;
  INST_ID_JNO = 23;
  INST_ID_JNP = 24;
  INST_ID_JNS = 25;
  INST_ID_JNZ = 26;
  INST_ID_JO = 27;
  INST_ID_JP = 28;
  INST_ID_JS = 29;
  INST_ID_JZ = 30;
  INST_ID_LEA = 31;
  INST_ID_LEAVE = 32;
  INST_ID_LOOP = 33;
  INST_ID_LOOPE = 34;
  INST_ID_LOOPNE = 35;
  INST_ID_MOV = 36;
  INST_ID_MUL = 37;
  INST_ID_NEG = 38;
  INST_ID_NOP = 39;
  INST_ID_NOT = 40;
  INST_ID_OR = 41;
  INST_ID_POP = 42;
  INST_ID_POPA = 43;
  INST_ID_POPF = 44;
  INST_ID_PUSH = 45;
  INST_ID_PUSHA = 46;
  INST_ID_PUSHF = 47;
  INST_ID_RET = 48;
  INST_ID_SBB = 49;
  INST_ID_SHL = 50;
  INST_ID_SHR = 51;
  INST_ID_SUB = 52;
  INST_ID_TEST = 53;
  INST_ID_XCHG = 54;
  INST_ID_XOR = 55;
  INST_ID_INVALID = 56;
  INST_ID_UNKNOWN = 0;

  { Groups ID }
  INST_ID_Grp1 = 1;
  INST_ID_Grp10 = 2;
  INST_ID_Grp11A = 3;
  INST_ID_Grp11B = 4;
  INST_ID_Grp12 = 5;
  INST_ID_Grp13 = 6;
  INST_ID_Grp14 = 7;
  INST_ID_Grp15 = 8;
  INST_ID_Grp16 = 9;
  INST_ID_Grp17 = 10;
  INST_ID_Grp18 = 11;
  INST_ID_Grp19 = 12;
  INST_ID_Grp1A = 13;
  INST_ID_Grp2 = 14;
  INST_ID_Grp3_1 = 15;
  INST_ID_Grp3_2 = 16;
  INST_ID_Grp4 = 17;
  INST_ID_Grp5 = 18;
  INST_ID_Grp6 = 19;
  INST_ID_Grp7 = 20;
  INST_ID_Grp8 = 21;
  INST_ID_Grp9 = 22;
  INST_ID_GrpP = 23;
  INST_ID_GrpPDLK = 24;
  INST_ID_GrpRNG = 25;

  { Prefixes ID }
  INST_ID_PREFIX_ADDRESS_SIZE = 1;
  INST_ID_PREFIX_EVEX = 2;
  INST_ID_PREFIX_LOCK = 3;
  INST_ID_PREFIX_OPERAND_SIZE = 4;
  INST_ID_PREFIX_REP = 5;
  INST_ID_PREFIX_REPNE = 6;
  INST_ID_PREFIX_REX = 7;
  INST_ID_PREFIX_SEG_CS = 8;
  INST_ID_PREFIX_SEG_DS = 9;
  INST_ID_PREFIX_SEG_ES = 10;
  INST_ID_PREFIX_SEG_FS = 11;
  INST_ID_PREFIX_SEG_GS = 12;
  INST_ID_PREFIX_SEG_SS = 13;
  INST_ID_PREFIX_VEX = 14;

  { OpCode Flags }
  OPF_NONE = $00; // Valid.
  OPF_SPECIAL = $00; // Just to mark special case that must be handled manually !
  OPF_INVALID = $FF; // Invalid opcode.
  OPF_IMM = $04; // Immediate.
  OPF_OFFSET = $08; // Offset.
  OPF_ADDRESS = $0C; // Address.
  OPF_MODRM = $10; // ModRm.
  OPF_I64 = $20; // Invalid when x64.
  OPF_O64 = $00; // SYSCALL & SYSRET .
  OPF_PREFIX = $40; // Opcode is a prefix.
  OPF_GRP = $80; // Opcode represents a group.

  OPF_Ap = OPF_ADDRESS;

  OPF_SIZE_B = 0;
  OPF_SIZE_W = 1;
  OPF_SIZE_V = 2;
  OPF_SIZE_Z = 3;

  OPF_Ib = OPF_IMM or OPF_SIZE_B;
  OPF_Iv = OPF_IMM or OPF_SIZE_V;
  OPF_Iw = OPF_IMM or OPF_SIZE_W;
  OPF_Iz = OPF_IMM or OPF_SIZE_Z;

  OPF_Jb = OPF_OFFSET or OPF_SIZE_B;
  OPF_Jw = OPF_OFFSET or OPF_SIZE_W;
  OPF_Jz = OPF_OFFSET or OPF_SIZE_Z;

  OPF_Ob = OPF_OFFSET or OPF_SIZE_B;
  OPF_Ov = OPF_OFFSET or OPF_SIZE_V;

  OPF_SIZE_MASK = OPF_SIZE_B or OPF_SIZE_W or OPF_SIZE_V or OPF_SIZE_Z;
  OPF_TYPE_MASK = OPF_IMM or OPF_OFFSET or OPF_ADDRESS;

  { Sizes }
  SizeOfByte = SizeOf(Byte);
  SizeOfWord = SizeOf(Word);
  SizeOfDword = SizeOf(Cardinal);
  SizeOfQword = SizeOf(Int64);

  { OpTables }
  TABLE_ONE_BYTE = 1;
  TABLE_TWO_BYTE = 2;
  TABLE_THREE_BYTE_38 = 3;
  TABLE_THREE_BYTE_3A = 4;

  { Address mode }
  am16 = 0; { 16-bit addressing mode }
  am32 = 1; { 32-bit addressing mode }
  am64 = 2; { 64-bit addressing mode }

  { Prefixes }
  PF_ADDRESS_SIZE = $01;
  PF_OPSIZE = $02;
  PF_LOCK = $04;
  PF_REP = $08;
  PF_REPNE = $10;
  PF_SEG = $20;
  PF_REX = $40;

  { Segments }
  SEG_DS = $01;
  SEG_CS = $02;
  SEG_SS = $03;
  SEG_FS = $04;
  SEG_GS = $05;
  SEG_ES = $06;

  { Branch flags }
  BF_BRANCH = $01; // Branch used.
  BF_REL = $02; // Relative to EIP|RIP.
  BF_MEM = $04; // Target encoded into memory.
  BF_JCC = $08; // Conditional jump.

  { Error }
  NO_ERROR = 0;
  ERROR_UNKNOWN_GROUP = 1;
  ERROR_UNKNOWN_PREFIX = 2;
  ERROR_BRANCH_MEMORY = 3;
  ERROR_INSTRUCTION_LENGTH = 4;

  { Instruction length }
  PREFIX_LENGTH = 1;
  VEX3_LENGTH = 3;
  OPCODE_LENGTH = 1;
  MODRM_LENGTH = 1;
  SIB_LENGTH = 1;
  DISP32_LENGTH = 4;
  IMM32_LENGTH = 4;
  IMM64_LENGTH = 8;
  MAX_INSTRUCTION_LENGTH_X32 = PREFIX_LENGTH { Address size } + OPCODE_LENGTH + VEX3_LENGTH + MODRM_LENGTH + SIB_LENGTH + DISP32_LENGTH + IMM32_LENGTH;
  MAX_INSTRUCTION_LENGTH_X64 = PREFIX_LENGTH { Address size } + OPCODE_LENGTH + VEX3_LENGTH + MODRM_LENGTH + SIB_LENGTH + DISP32_LENGTH + IMM64_LENGTH;
{$IFDEF CPUX64}
  MAX_INSTRUCTION_LENGTH = MAX_INSTRUCTION_LENGTH_X64;
{$ELSE !CPUX64}
  MAX_INSTRUCTION_LENGTH = MAX_INSTRUCTION_LENGTH_X32;
{$ENDIF CPUX64}

type
  TCpuArchitecture = (CPUX86, CPUX64);
  TAddressMode = am16 .. am64;

  // ---------------- Delphi compatibility ----------------
{$IFNDEF DELPHI_XE_UP}
  IntPtr = NativeInt;
{$ENDIF}

  TSib = packed record
    Pos: ShortInt; { Relative position to address. }
    Scale: Byte; { SIB.Scale Field. }
    Index: Byte; { Register Index. }
    Base: Byte; { Register Base. }
    Value: Byte; { SIB Value. }
  end;

  PSib = ^TSib;

  TModRm = packed record
    Pos: ShortInt; { Relative position to address. }
    Flags: Byte; { ModRm flags. }
    Value: Byte; { ModRm Value. }
    _Mod: Byte;
    Reg: Byte;
    Rm: Byte;
  end;

  PModRm = ^TModRm;

  TDisplacement = packed record
    Pos: ShortInt; { Relative position to address. }
    Size: Byte; { Size of displacement. }
    Value: Integer; { Displacement value. }
  end;

  PDisplacement = ^TDisplacement;

  TImmediate = packed record
    Pos: ShortInt; { Relative position to address. }
    Size: ShortInt; { Size of immediate. }
    { NB:
      "ENTER Iw,Ib":
      ENTER instruction does not respect legacy rules
      and it uses two immediate !
      - The first imm (Iw) will be decoded in Imm16 field.
      - The second imm (Ib) will be decoded in ExtraImm16 field. }
    case Integer of
      0: (Value: Int64);
      1: (Imm64: Int64);
      2: (Imm32: Integer);
      3: (Imm16, ExtraImm16: SmallInt);
      4: (Imm8: ShortInt);
  end;

  PImmediate = ^TImmediate;

  TOffset = packed record
    Pos: ShortInt; { Relative position to address. }
    Size: ShortInt; { Size of offset. }
    Value: Int64; { Offset value. }
  end;

  POffset = ^TOffset;

  TFarAddress = packed record
    Pos: ShortInt; { Relative position to address. }
    { Size of far address.
      NB: Size of selector is counted !
      To get size of offset subtract 2 byte ! }
    Size: ShortInt;
    Seg: Word;
    case Integer of
      0: (Value: Integer);
      1: (Offset32: Integer);
      2: (Offset16: SmallInt);
  end;

  PFarAddress = ^TFarAddress;

  TBranch = packed record
    Flags: Byte; { Sets of BF_XX. }
    Target: Pointer; { Branch target. }
  end;

  PBranch = ^TBranch;

  TRex = packed record
    R: Boolean; { REX.R Field }
    X: Boolean; { REX.X Field }
    B: Boolean; { REX.B Field }
    W: Boolean; { REX.W Field }
    Value: Byte; { REX Value = [$40..$4F] }
  end;

  PRex = ^TRex;

  { Don't care ... it remains on the stack
    and it's never available for user.
    Instruction.InternalData is always nil. }
  TInternalData = packed record
    vSize: ShortInt;
    zSize: ShortInt;
  end;

  PInternalData = ^TInternalData;

  TInstruction = record
    Arch: TCpuArchitecture; { CPU architecture. }
    OpTable: ShortInt; { One of TABLE_XX. }
    InstId: Byte; { Instruction ID => One of INST_ID_XX. }
    Address: Pointer; { The address where the function will start decoding. }
    NextAddress: PByte; { Pointer to the next instruction. }
    { VirtualAddress to calculate branch destination.
      If it's nil, the function will use NextAddress. }
    VirtualAddress: Pointer;
    AddressMode: TAddressMode; { One of amXX. }
    ModRm: TModRm; { ModRm. }
    Sib: TSib; { Sib. }
    Displacement: TDisplacement; { Displacement. }
    Immediate: TImmediate; { Immediate. }
    Offset: TOffset; { Offset. }
    FarAddress: TFarAddress; { Address. }
    Seg: ShortInt; { Segment register. }
    Prefixes: Cardinal; { Sets of PF_XX. }
    Rex: TRex; { Rex prefix. }
    Branch: TBranch; { Branch }
    InternalData: PInternalData; { Internal data => Do not use. }
    Error: Byte;
  end;

  PInstruction = ^TInstruction;

const
{$IFDEF CPUX64}
  CPUX = CPUX64;
{$ELSE !CPUX64}
  CPUX = CPUX86;
{$ENDIF CPUX64}
function DecodeInst(PInst: PInstruction): ShortInt;

implementation

{$I OpCodesTables.inc}
{$I ModRmFlags.inc}

const
  AM2ModRmFlags: array [TAddressMode] of PModRmFlagsArray = ( //
    @ModRmFlags16, { AM 16 bit }
    @ModRmFlags32, { AM 32 bit }
    @ModRmFlags32 { AM 64 bit }
    );

  CPUX2AddressMode: array [TCpuArchitecture] of ShortInt = (am32, am64);
  CPUX2MaxInstLength: array [TCpuArchitecture] of ShortInt = (MAX_INSTRUCTION_LENGTH_X32, MAX_INSTRUCTION_LENGTH_X64);

type
  TPairFlagsInstId = packed record
    Flags: Byte;
    InstId: Byte;
  end;

  PPairFlagsInstId = ^TPairFlagsInstId;

  TGroupArray = array [0 .. $07] of Word;
  PGroupArray = ^TGroupArray;

function GetMod(ModRm: Byte): Byte;
begin
  Result := (ModRm and $C0) shr 6;
end;

function GetReg(ModRm: Byte): Byte;
begin
  Result := (ModRm and $38) shr 3;
end;

function GetRm(ModRm: Byte): Byte;
begin
  Result := ModRm and 7;
end;

function GetBase(Sib: Byte): Byte;
begin
  Result := Sib and 7;
end;

function GetIndex(Sib: Byte): Byte;
begin
  Result := (Sib and $38) shr $03;
end;

function GetScale(Sib: Byte): Byte;
begin
  Result := (1 shl (Sib shr 6));
end;

procedure SetError(PInst: PInstruction; Error: Byte);
begin
  PInst^.Error := Error;
end;

function GetSizeFromFlags(PInst: PInstruction; Flags: Byte): ShortInt;
var
  T: ShortInt;
begin
  Result := -1;
  T := Flags and OPF_SIZE_MASK;
  case T of
    OPF_SIZE_B: Result := SizeOfByte;
    OPF_SIZE_W: Result := SizeOfWord;
    OPF_SIZE_V: Result := PInst^.InternalData^.vSize;
    OPF_SIZE_Z: Result := PInst^.InternalData^.zSize;
  end;
end;

procedure DecodeModRm(PInst: PInstruction);
var
  ModRmFlags: Byte;
  ModRm: Byte;
begin
  if PInst^.ModRm.Pos > 0 then
    Exit; // From Group !

  ModRm := PInst^.NextAddress^;
  ModRmFlags := AM2ModRmFlags[PInst^.AddressMode]^[ModRm];
  with PInst^.ModRm do
  begin
    Pos := NativeUInt(PInst^.NextAddress) - NativeUInt(PInst^.Address);
    Value := ModRm;
    Flags := ModRmFlags;
    if PInst^.Arch = CPUX64 then
      Flags := Flags or MF_REL;
    _Mod := GetMod(ModRm);
    Reg := GetReg(ModRm);
    Rm := GetRm(ModRm);
  end;
  Inc(PInst^.NextAddress); // skip modrm.

  if ModRmFlags and MF_SIB <> $00 then
  begin
    with PInst^.Sib do
    begin
      Pos := NativeUInt(PInst^.NextAddress) - NativeUInt(PInst^.Address);
      Value := PInst^.NextAddress^;
      Scale := GetScale(Value);
      Base := GetBase(Value);
      Index := GetIndex(Value);
    end;
    Inc(PInst^.NextAddress); // skip sib.
  end;

  if ModRmFlags and MF_SBYTE <> $00 then
  begin
    with PInst^.Displacement do
    begin
      Size := SizeOfByte;
      Value := PShortInt(PInst^.NextAddress)^;
    end;
  end else if ModRmFlags and MF_SWORD <> $00 then
  begin
    with PInst^.Displacement do
    begin
      Size := SizeOfWord;
      Value := PSmallInt(PInst^.NextAddress)^;
    end;
  end else if ModRmFlags and MF_SDWORD <> $00 then
  begin
    with PInst^.Displacement do
    begin
      Size := SizeOfDword;
      Value := PInteger(PInst^.NextAddress)^;
    end;
  end;
  if PInst^.Displacement.Size > 0 then
    PInst^.Displacement.Pos := NativeUInt(PInst^.NextAddress) - NativeUInt(PInst^.Address);

  Inc(PInst^.NextAddress, PInst^.Displacement.Size);
end;

procedure DecodeImmediate(PInst: PInstruction; ImmSize: ShortInt);
begin
  PInst^.Immediate.Pos := NativeUInt(PInst^.NextAddress) - NativeUInt(PInst^.Address);
  PInst^.Immediate.Size := ImmSize;
  case ImmSize of
    SizeOfByte: PInst^.Immediate.Value := PShortInt(PInst^.NextAddress)^;
    SizeOfWord: PInst^.Immediate.Value := PSmallInt(PInst^.NextAddress)^;
    SizeOfDword: PInst^.Immediate.Value := PInteger(PInst^.NextAddress)^;
    SizeOfQword: PInst^.Immediate.Value := PInt64(PInst^.NextAddress)^;
  end;
  Inc(PInst^.NextAddress, ImmSize);
end;

procedure DecodeOffset(PInst: PInstruction; OffsetSize: ShortInt);
begin
  PInst^.Offset.Pos := NativeUInt(PInst^.NextAddress) - NativeUInt(PInst^.Address);
  PInst^.Offset.Size := OffsetSize;
  case OffsetSize of
    SizeOfByte: PInst^.Offset.Value := PShortInt(PInst^.NextAddress)^;
    SizeOfWord: PInst^.Offset.Value := PSmallInt(PInst^.NextAddress)^;
    SizeOfDword: PInst^.Offset.Value := PInteger(PInst^.NextAddress)^;
    SizeOfQword: PInst^.Offset.Value := PInt64(PInst^.NextAddress)^;
  end;
  Inc(PInst^.NextAddress, OffsetSize);
end;

procedure DecodeFarAddress(PInst: PInstruction);
var
  OffsetSize: ShortInt;
begin
  { Address = [WORD:WORD|DWORD] }
  PInst^.FarAddress.Pos := NativeUInt(PInst^.NextAddress) - NativeUInt(PInst^.Address);
  OffsetSize := PInst^.InternalData^.zSize;
  with PInst^.FarAddress do
  begin
    Size := SizeOfWord + OffsetSize;
    Seg := PSmallInt(PInst^.NextAddress)^;
    Inc(PInst^.NextAddress, SizeOfWord);
    case OffsetSize of
      SizeOfWord: Value := PSmallInt(PInst^.NextAddress)^;
      SizeOfDword: Value := PInteger(PInst^.NextAddress)^;
    end;
  end;
  Inc(PInst^.NextAddress, OffsetSize);
end;

procedure DecodePrefix(PInst: PInstruction);
begin
  case PInst^.InstId of
    INST_ID_PREFIX_ADDRESS_SIZE:
      begin
        if PInst^.Prefixes and PF_ADDRESS_SIZE = 0 then
        begin
          Dec(PInst^.AddressMode);
          PInst^.Prefixes := PInst^.Prefixes or PF_ADDRESS_SIZE;
        end;
        Inc(PInst^.NextAddress); // Skip prefix.
      end;
    INST_ID_PREFIX_OPERAND_SIZE:
      begin
        with PInst^.InternalData^ do
        begin
          vSize := SizeOfWord;
          zSize := SizeOfWord;
        end;
        PInst^.Prefixes := PInst^.Prefixes or PF_OPSIZE;
        Inc(PInst^.NextAddress); // Skip prefix.
      end;
    INST_ID_PREFIX_REX:
      begin
        if PInst^.Arch = CPUX64 then
        begin
          with PInst^.Rex do
          begin
            Value := PInst^.NextAddress^;
            B := (Value and $01 <> $00);
            X := (Value and $02 <> $00);
            R := (Value and $04 <> $00);
            W := (Value and $08 <> $00);
            if W then
              PInst^.InternalData^.vSize := SizeOfQword;
          end;
          PInst^.Prefixes := PInst^.Prefixes or PF_REX;
        end
        else { CPUX86 }
        begin
          if PInst^.NextAddress^ < $48 then
            PInst^.InstId := INST_ID_INC
          else
            PInst^.InstId := INST_ID_DEC;
        end;
        Inc(PInst^.NextAddress); // Skip prefix.
      end;
    INST_ID_PREFIX_SEG_CS:
      begin
        PInst^.Prefixes := PInst^.Prefixes or PF_SEG;
        PInst^.Seg := SEG_CS;
        Inc(PInst^.NextAddress); // Skip prefix.
      end;
    INST_ID_PREFIX_SEG_DS:
      begin
        PInst^.Prefixes := PInst^.Prefixes or PF_SEG;
        PInst^.Seg := SEG_DS;
        Inc(PInst^.NextAddress); // Skip prefix.
      end;
    INST_ID_PREFIX_SEG_GS:
      begin
        PInst^.Prefixes := PInst^.Prefixes or PF_SEG;
        PInst^.Seg := SEG_GS;
        Inc(PInst^.NextAddress); // Skip prefix.
      end;
    INST_ID_PREFIX_SEG_FS:
      begin
        PInst^.Prefixes := PInst^.Prefixes or PF_SEG;
        PInst^.Seg := SEG_FS;
        Inc(PInst^.NextAddress); // Skip prefix.
      end;
    INST_ID_PREFIX_SEG_SS:
      begin
        PInst^.Prefixes := PInst^.Prefixes or PF_SEG;
        PInst^.Seg := SEG_SS;
        Inc(PInst^.NextAddress); // Skip prefix.
      end;
    INST_ID_PREFIX_SEG_ES:
      begin
        PInst^.Prefixes := PInst^.Prefixes or PF_SEG;
        PInst^.Seg := SEG_ES;
        Inc(PInst^.NextAddress); // Skip prefix.
      end;
    INST_ID_PREFIX_LOCK:
      begin
        PInst^.Prefixes := PInst^.Prefixes or PF_LOCK;
        Inc(PInst^.NextAddress); // Skip prefix.
      end;
    INST_ID_PREFIX_REP:
      begin
        PInst^.Prefixes := PInst^.Prefixes or PF_REP;
        Inc(PInst^.NextAddress); // Skip prefix.
      end;
    INST_ID_PREFIX_REPNE:
      begin
        PInst^.Prefixes := PInst^.Prefixes or PF_REPNE;
        Inc(PInst^.NextAddress); // Skip prefix.
      end;
  else SetError(PInst, ERROR_UNKNOWN_PREFIX);
  end;
end;

function DecodeGroup(PInst: PInstruction; Group: ShortInt): Byte;
var
  PGrpArray: PGroupArray;
  Pair: PPairFlagsInstId;
begin
  DecodeModRm(PInst);
  PGrpArray := nil;
  Result := Byte(-1);
  case Group of
    INST_ID_Grp1: PGrpArray := @Grp1;
    INST_ID_Grp1A: PGrpArray := @Grp1A;
    INST_ID_Grp2: PGrpArray := @Grp2;
    INST_ID_Grp3_1: PGrpArray := @Grp3_1;
    INST_ID_Grp3_2: PGrpArray := @Grp3_2;
    INST_ID_Grp4: PGrpArray := @Grp4;
    INST_ID_Grp5: PGrpArray := @Grp5;
    INST_ID_Grp6: PGrpArray := @Grp6;
    INST_ID_Grp7: PGrpArray := @Grp7;
    INST_ID_Grp8: PGrpArray := @Grp8;
    INST_ID_Grp9: PGrpArray := @Grp9;
    INST_ID_Grp10: PGrpArray := @Grp10;
    INST_ID_Grp11A: PGrpArray := @Grp11A;
    INST_ID_Grp11B: PGrpArray := @Grp11B;
    INST_ID_Grp12: PGrpArray := @Grp12;
    INST_ID_Grp13: PGrpArray := @Grp13;
    INST_ID_Grp14: PGrpArray := @Grp14;
    INST_ID_Grp15: PGrpArray := @Grp15;
    INST_ID_Grp16: PGrpArray := @Grp16;
    INST_ID_Grp17: PGrpArray := @Grp17;
    INST_ID_Grp18: PGrpArray := @Grp18;
    INST_ID_Grp19: PGrpArray := @Grp19;
    INST_ID_GrpP: PGrpArray := @GrpP;
    INST_ID_GrpPDLK: PGrpArray := @GrpPDLK;
    INST_ID_GrpRNG: PGrpArray := @GrpRNG;
  else SetError(PInst, ERROR_UNKNOWN_GROUP);
  end;
  if Assigned(PGrpArray) then
  begin
    Pair := @PGrpArray^[PInst^.ModRm.Reg];
    PInst^.InstId := Pair^.InstId;
    Result := Pair^.Flags;
  end;
end;


procedure CalculateBranchTarget(PInst: PInstruction);
var
  P: Pointer;
begin
  P := nil;
  if not Assigned(PInst^.VirtualAddress) then
    PInst^.VirtualAddress := PInst^.NextAddress;
  if (PInst^.Offset.Pos > 0) and (PInst^.InstId <> INST_ID_MOV) then
  begin
    if (PInst^.InstId <> INST_ID_JMP) and (PInst^.InstId <> INST_ID_CALL) then
      PInst^.Branch.Flags := BF_BRANCH or BF_REL or BF_JCC
    else
      PInst^.Branch.Flags := BF_BRANCH or BF_REL;
     P := Pointer(IntPtr(PInst^.VirtualAddress) + PInst^.Offset.Value);
  end // Offset.
  else // Memory.
    if (PInst^.Displacement.Pos > 0) and (PInst^.ModRm.Flags and MF_DISP_ONLY <> $00) and ((PInst^.InstId = INST_ID_JMP) or (PInst^.InstId = INST_ID_CALL)) then
    begin
      PInst^.Branch.Flags := BF_BRANCH or BF_MEM;
      if PInst^.Arch = CPUX86 then
      begin
        try
          P := PPointer(PInst^.Displacement.Value)^;
        except
          SetError(PInst, ERROR_BRANCH_MEMORY);
        end;
      end else begin
        P := PInst^.VirtualAddress;
        if PInst^.AddressMode = am32 then
          P := Pointer(NativeUInt(P) and $FFFFFFFF);
        P := Pointer(IntPtr(P) + PInst^.Displacement.Value);
        try
          P := PPointer(P)^;
        except
          SetError(PInst, ERROR_BRANCH_MEMORY);
        end;
      end;
    end;
  PInst^.Branch.Target := P;
end;


function DecodeInst(PInst: PInstruction): ShortInt;
var
  Pair: PPairFlagsInstId;
  Flags: Byte;
  GrpFlags: Byte;
  InstId: Byte;
  Size: ShortInt;
  InternalData: TInternalData;
  MaxInstLength: ShortInt;
begin
  { Initialization }
  Pair := nil;
  PInst^.NextAddress := PInst^.Address;
  PInst^.AddressMode := CPUX2AddressMode[PInst^.Arch];
  PInst^.OpTable := TABLE_ONE_BYTE;
  PInst^.InternalData := @InternalData;
  InternalData.zSize := SizeOfDword;
  InternalData.vSize := SizeOfDword;
  MaxInstLength := CPUX2MaxInstLength[PInst^.Arch];
  SetError(PInst, NO_ERROR);
  while True do
  begin
    if (PInst^.NextAddress^ = $0F) and (PInst^.OpTable = TABLE_ONE_BYTE) then
    begin
      PInst^.OpTable := TABLE_TWO_BYTE;
      Inc(PInst^.NextAddress); // Skip escape.
      Continue;
    end else if (PInst^.NextAddress^ = $38) and (PInst^.OpTable = TABLE_TWO_BYTE) then
    begin
      PInst^.OpTable := TABLE_THREE_BYTE_38;
      Inc(PInst^.NextAddress); // Skip escape.
      Continue;
    end else if (PInst^.NextAddress^ = $3A) and (PInst^.OpTable = TABLE_TWO_BYTE) then
    begin
      PInst^.OpTable := TABLE_THREE_BYTE_3A;
      Inc(PInst^.NextAddress); // Skip escape.
      Continue;
    end else begin
      case PInst^.OpTable of
        TABLE_ONE_BYTE: Pair := @Table1[PInst^.NextAddress^];
        TABLE_TWO_BYTE: Pair := @Table2[PInst^.NextAddress^];
        TABLE_THREE_BYTE_38: Pair := @Table38[PInst^.NextAddress^];
        TABLE_THREE_BYTE_3A: Pair := @Table3A[PInst^.NextAddress^];
      end;
      Flags := Pair^.Flags;
      InstId := Pair^.InstId;
      PInst^.InstId := InstId;
      if (PInst^.Arch = CPUX64) and (Flags and OPF_I64 <> $00) then
        Flags := OPF_INVALID;

      if (PInst^.OpTable = TABLE_ONE_BYTE) and //
        (PInst^.NextAddress^ >= $D8) and (PInst^.NextAddress^ <= $DF) then
      begin
        { FPU instructions. }
        Inc(PInst^.NextAddress); // Skip fpu esc.
        DecodeModRm(PInst);
        Break;
      end;

      Inc(PInst^.NextAddress); // Skip opcode.

      if (Flags = OPF_INVALID) or (InstId = INST_ID_INVALID) then
      begin
        { Invalid opcode }
        PInst^.InstId := INST_ID_INVALID;
        Break;
      end else if Flags and OPF_PREFIX <> $00 then
      begin
        { Prefixes }
        Dec(PInst^.NextAddress); // Need prefix opcode !
        DecodePrefix(PInst);
        Continue;
      end else if Flags and OPF_GRP <> $00 then
      begin
        { Groups }
        GrpFlags := DecodeGroup(PInst, PInst^.InstId);
        if GrpFlags <> OPF_NONE then
          Flags := GrpFlags; // Inherits from group.
      end;
      if Flags and OPF_MODRM <> $00 then
      begin
        { ModRm }
        DecodeModRm(PInst);
      end;
      if Flags and OPF_TYPE_MASK = OPF_IMM then
      begin
        { Immediate }
        if InstId = INST_ID_ENTER then
        begin
          { ENTER Iw,Ib }
          { Enter instruction uses two immediate !!! }
          DecodeImmediate(PInst, SizeOfWord);
          PInst^.Immediate.ExtraImm16 := PShortInt(PInst^.NextAddress)^;
          Inc(PInst^.NextAddress);
        end else begin
          Size := GetSizeFromFlags(PInst, Flags);
          DecodeImmediate(PInst, Size);
        end;
      end else if Flags and OPF_TYPE_MASK = OPF_OFFSET then
      begin
        { Offset }
        Size := GetSizeFromFlags(PInst, Flags);
        DecodeOffset(PInst, Size);
      end else if Flags and OPF_TYPE_MASK = OPF_ADDRESS then
      begin
        { Address }
        DecodeFarAddress(PInst);
      end;
    end;
    Break;
  end;
  CalculateBranchTarget(PInst);
  PInst^.InternalData := nil;
  Result := NativeUInt(PInst^.NextAddress) - NativeUInt(PInst^.Address);
  if Result > MaxInstLength then
    SetError(PInst, ERROR_INSTRUCTION_LENGTH);
end;

end.
