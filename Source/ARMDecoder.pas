unit ARMDecoder;

// https://github.com/MahdiSafsafi/delphi-detours-library

{ ============================== not ready yet =============================== }
{$DEFINE NOT_READY_YET}
{$DEFINE EXPERIMENTAL}

// Bugs may appear. Architecture may change, ...
{ ============================================================================ }
interface

const
  { InstId }
  INST_ID_AND = 1; { Logical AND Rd := Rn AND shifter_operand. }
  INST_ID_EOR = 2; { Logical Exclusive OR Rd := Rn EOR shifter_operand. }
  INST_ID_SUB = 3; { Subtract Rd := Rn - shifter_operand. }
  INST_ID_RSB = 4; { Reverse Subtract Rd := shifter_operand - Rn. }
  INST_ID_ADD = 5; { Add Rd := Rn + shifter_operand. }
  INST_ID_ADC = 6; { Add with Carry Rd := Rn + shifter_operand + Carry Flag. }
  INST_ID_SBC = 7; { Subtract with Carry Rd := Rn - shifter_operand - NOT(Carry Flag). }
  INST_ID_RSC = 8; { Reverse Subtract with Carry Rd := shifter_operand - Rn - NOT(Carry Flag). }
  INST_ID_TST = 9; { Test Update flags after Rn AND shifter_operand. }
  INST_ID_TEQ = 10; { Test Equivalence Update flags after Rn EOR shifter_operand. }
  INST_ID_CMP = 11; { Compare Update flags after Rn - shifter_operand. }
  INST_ID_CMN = 12; { Compare Negated Update flags after Rn + shifter_operand. }
  INST_ID_ORR = 13; { Logical (inclusive) OR Rd := Rn OR shifter_operand. }
  INST_ID_MOV = 14; { Move Rd := shifter_operand (no first operand). }
  INST_ID_BIC = 15; { Bit Clear Rd := Rn AND NOT(shifter_operand). }
  INST_ID_MVN = 16; { Move Not Rd := NOT shifter_operand (no first operand). }

  { Condition code }
  CONDITION_EQ = $00; { Equal Z set. }
  CONDITION_NE = $01; { Not equal Z clear. }
  CONDITION_CS = $02; { Carry set/unsigned higher or same C set. }
  CONDITION_HS = CONDITION_CS;
  CONDITION_CC = $03; { Carry clear/unsigned lower C clear. }
  CONDITION_LO = CONDITION_CC;
  CONDITION_MI = $04; { Minus/negative N set. }
  CONDITION_PL = $05; { Plus/positive or zero N clear. }
  CONDITION_VS = $06; { Overflow V set. }
  CONDITION_VC = $07; { No overflow V clear. }
  CONDITION_HI = $08; { Unsigned higher C set and Z clear. }
  CONDITION_LS = $09; { Unsigned lower or same C clear or Z set. }
  CONDITION_GE = $0A; { Signed greater than or equal N set and V set, or N clear and V clear (N == V). }
  CONDITION_LT = $0B; { Signed less than N set and V clear, or N clear and V set (N != V). }
  CONDITION_GT = $0C; { Signed greater than Z clear, and either N set and V set, or N clear and V clear (Z == 0,N == V). }
  CONDITION_LE = $0D; { Signed less than or equal Z set, or N set and V clear, or N clear and V set (Z == 1 or N != V). }
  CONDITION_AL = $0E; { Always (unconditional) . }
  CONDITION_NONE = $0F;

  { ARM Registers }
  REG_R0 = 0;
  REG_R1 = 1;
  REG_R2 = 2;
  REG_R3 = 3;
  REG_R4 = 4;
  REG_R5 = 5;
  REG_R6 = 6;
  REG_R7 = 7;
  REG_R8 = 8;
  REG_R9 = 9;
  REG_R10 = 10;
  REG_R11 = 11;
  REG_R12 = 12;
  REG_R13 = 13;
  REG_R14 = 14;
  REG_R15 = 15;
  REG_IP = REG_R12; // Intra-procedure scratch register.
  REG_SP = REG_R13; // Stack pointer register.
  REG_LR = REG_R14; // Link register.
  REG_PC = REG_R15; // Program counter register.

type
  { ARM world defines WORD type as it was 4 byte ! }
  TARMWord = Cardinal;
  PARMWord = ^TARMWord;
  TARMHalfWord = UInt16;
  TRegister = ShortInt;
  TInstId = SmallInt;
  TCondition = CONDITION_EQ .. CONDITION_NONE;

  TShiftKind = ( //
    skReg, // Rm shift Rs.
    skImm, // Rm shift Imm.
    skLeft, // Shift left operation.
    skRight, // Shift right operation.
    skLogical, // Logical shift operation.
    skArithmetic, // Arithmetic shift operation.
    skRotate, // Rotate operation.
    skExtend // Only with rotate.
    );

  TShiftKinds = set of TShiftKind;

  TImmediate = record
    Value: ShortInt;
    Rotate: ShortInt;
  end;

  TShiftOperand = record
    Kinds: TShiftKinds;
    Rm: TRegister;
    case TShiftKind of
      skImm: (Imm: ShortInt);
      skReg: (Rs: TRegister);
  end;

  TInstruction = record
    Address: Pointer;
    FullOpCode: TARMWord;
    Condition: TCondition;
    InstId: TInstId;
    Rn: TRegister;
    Rd: TRegister;
    Imm: TImmediate;
    Shift: TShiftOperand;
  end;

function DecodeARM(var Ins: TInstruction): ShortInt;

implementation

const
  CONDITION_MASK = $F0000000;

  { Bits }
  BIT_0 = $00000001;
  BIT_1 = $00000002;
  BIT_2 = $00000004;
  BIT_3 = $00000008;
  BIT_4 = $00000010;
  BIT_5 = $00000020;
  BIT_6 = $00000040;
  BIT_7 = $00000080;
  BIT_8 = $00000100;
  BIT_9 = $00000200;
  BIT_10 = $00000400;
  BIT_11 = $00000800;
  BIT_12 = $00001000;
  BIT_13 = $00002000;
  BIT_14 = $00004000;
  BIT_15 = $00008000;
  BIT_16 = $00010000;
  BIT_17 = $00020000;
  BIT_18 = $00040000;
  BIT_19 = $00080000;
  BIT_20 = $00100000;
  BIT_21 = $00200000;
  BIT_22 = $00400000;
  BIT_23 = $00800000;
  BIT_24 = $01000000;
  BIT_25 = $02000000;
  BIT_26 = $04000000;
  BIT_27 = $08000000;
  BIT_28 = $10000000;
  BIT_29 = $20000000;
  BIT_30 = $40000000;
  BIT_31 = $80000000;

function GetRn(Value: TARMWord): TRegister;
begin
  Result := TRegister((Value and $F0000) shr 16);
end;

function GetRd(Value: TARMWord): TRegister;
begin
  Result := TRegister((Value and $F000) shr 12);
end;

function GetRs(Value: TARMWord): TRegister;
begin
  Result := TRegister((Value and $F00) shr 8);
end;

function GetRm(Value: TARMWord): TRegister;
begin
  Result := Value and $0F;
end;

function GetImm(Value: TARMWord): ShortInt;
begin
  Result := Value and $FF;
end;

function GetRotate(Value: TARMWord): ShortInt;
begin
  Result := (Value and $F0) shr 4;
end;

function GetShiftImm(Value: TARMWord): ShortInt;
begin
  Result := (Value and $F80) shr 7;
end;

procedure DecodeImm(var Ins: TInstruction);
begin
  Ins.Imm.Value := GetImm(Ins.FullOpCode);
  Ins.Imm.Rotate := GetRotate(Ins.FullOpCode);
end;

procedure DecodeShifterOperand(var Ins: TInstruction);
var
  W: TARMWord;
  ShiftMethod: ShortInt;
begin
  W := Ins.FullOpCode;
  Ins.Shift.Rm := GetRm(W);
  ShiftMethod := (W and $70) shr 4;
  case ShiftMethod of
    0:
      begin
        { Logical shift left by immediate }
        Ins.Shift.Kinds := [skLogical, skLeft, skImm];
        Ins.Shift.Imm := GetShiftImm(W);
      end;
    1:
      begin
        { Logical shift left by register }
        Ins.Shift.Kinds := [skLogical, skLeft, skReg];
        Ins.Shift.Rs := GetRs(W);
      end;
    2:
      begin
        { Logical shift right by immediate }
        Ins.Shift.Kinds := [skLogical, skRight, skImm];
        Ins.Shift.Imm := GetShiftImm(W);
      end;
    3:
      begin
        { Logical shift right by register }
        Ins.Shift.Kinds := [skLogical, skRight, skReg];
        Ins.Shift.Rs := GetRs(W);
      end;
    4:
      begin
        { Arithmetic shift right by immediate }
        Ins.Shift.Kinds := [skArithmetic, skRight, skImm];
        Ins.Shift.Imm := GetShiftImm(W);
      end;
    5:
      begin
        { Arithmetic shift right by register }
        Ins.Shift.Kinds := [skArithmetic, skRight, skReg];
        Ins.Shift.Rs := GetRs(W);
      end;
    6:
      begin
        if (W and $FF0 = $60) then
        begin
          { Rotate right with extend }
          { This data-processing operand can be used to perform a 33-bit rotate right using the Carry Flag as the 33rd bit.
            This instruction operand is the value of register Rm shifted right by one bit, with the Carry Flag replacing
            the vacated bit position. The carry-out from the shifter is the bit shifted off the right end. }
          Ins.Shift.Kinds := [skRotate, skRight, skExtend];
        end else begin
          { Rotate right by immediate }
          Ins.Shift.Kinds := [skRotate, skRight, skImm];
          Ins.Shift.Imm := GetShiftImm(W);
        end;
      end;
    7:
      begin
        { Rotate right by register }
        Ins.Shift.Kinds := [skRotate, skRight, skReg];
        Ins.Shift.Rs := GetRs(W);
      end;
  end;
end;

procedure DecodeDataProcessingIns(var Ins: TInstruction);
const
  OPCODE_MASK = $1E00000;
  InsArray: array [$00 .. $0F] of TInstId = ( //
    INST_ID_AND, { $00 }
    INST_ID_EOR, { $01 }
    INST_ID_SUB, { $02 }
    INST_ID_RSB, { $03 }
    INST_ID_ADD, { $04 }
    INST_ID_ADC, { $05 }
    INST_ID_SBC, { $06 }
    INST_ID_RSC, { $07 }
    INST_ID_TST, { $08 }
    INST_ID_TEQ, { $09 }
    INST_ID_CMP, { $0A }
    INST_ID_CMN, { $0B }
    INST_ID_ORR, { $0C }
    INST_ID_MOV, { $0D }
    INST_ID_BIC, { $0E }
    INST_ID_MVN { $0F }
    );
var
  W: TARMWord;
  OpCode: Cardinal;
begin
  { Data-processing instructions }
  W := Ins.FullOpCode;
  OpCode := (W and OPCODE_MASK) shr 21;
  Ins.InstId := InsArray[OpCode];
  Ins.Rn := GetRn(W);
  Ins.Rd := GetRd(W);
  if W and BIT_25 <> $00 then
  begin
    { Immediate }
    DecodeImm(Ins);
  end else begin
    { ShifterOperand }
    DecodeShifterOperand(Ins);
  end;
end;

procedure DecodeMiscIns(var Ins: TInstruction);
begin

end;

procedure DecodeMultipliesIns(var Ins: TInstruction);
begin

end;

procedure DecodeUndefinedIns(var Ins: TInstruction);
begin

end;

procedure DecodeMoveImmToStatusRegIns(var Ins: TInstruction);
begin

end;

procedure DecodeLoadStoreImmOffsetIns(var Ins: TInstruction);
begin

end;

procedure DecodeArchitecturallyUndefinedIns(var Ins: TInstruction);
begin

end;

procedure DecodeMediaIns(var Ins: TInstruction);
begin

end;

procedure DecodeLoadStoreRegOffsetIns(var Ins: TInstruction);
begin

end;

procedure DecodeLoadStoreMultipleIns(var Ins: TInstruction);
begin

end;

procedure DecodeBranchIns(var Ins: TInstruction);
begin

end;

procedure DecodeCoprocessorLoadStoreIns(var Ins: TInstruction);
begin

end;

procedure DecodeCoprocessorDataIns(var Ins: TInstruction);
begin

end;

procedure DecodeCoprocessorRegTransferIns(var Ins: TInstruction);
begin

end;

procedure DecodeSoftwareInterruptIns(var Ins: TInstruction);
begin

end;

procedure Error;
begin

end;

procedure DecodeConditionalIns(var Ins: TInstruction);
var
  W: TARMWord;
  FirstSel: Cardinal;
begin
  W := Ins.FullOpCode;
  FirstSel := (W and (BIT_25 or BIT_26 or BIT_27)) shr 25; // Bits [25..27]
  if FirstSel = $00 then
  begin
    if (W and BIT_4 <> $00) then
    begin
      if (W and BIT_24) <> $00 then
        DecodeMiscIns(Ins)
      else
        if (W and BIT_7) <> $00 then
          DecodeMultipliesIns(Ins)
        else
          DecodeDataProcessingIns(Ins);
    end else begin // Not Bit4
      if (W and BIT_24) <> $00 then
        DecodeMiscIns(Ins)
      else
        DecodeDataProcessingIns(Ins);
    end;
  end else begin // FirstSel != 0.
    case FirstSel of
      1:
        begin
          { (Data-processing || Undefined || Move Imm to status reg) instructions }
          { Check bits [20 & 21] and [23 & 24] }
          if (W and (BIT_24 or BIT_23) = BIT_24) and (W and (BIT_20 or BIT_21) = BIT_21) then
            DecodeMoveImmToStatusRegIns(Ins)
          else
            if (W and (BIT_24 or BIT_23) = BIT_24) and (W and (BIT_20 or BIT_21) = $00) then
              DecodeUndefinedIns(Ins)
            else
              DecodeDataProcessingIns(Ins);
        end;
      2:
        begin
          { Load/Store imm offset }
          DecodeLoadStoreImmOffsetIns(Ins);
        end;
      3:
        begin
          if (W and (BIT_20 or BIT_21 or BIT_22 or BIT_23 or BIT_24) = (BIT_20 or BIT_21 or BIT_22 or BIT_23 or BIT_24)) and
            (W and (BIT_4 or BIT_5 or BIT_6 or BIT_7) = (BIT_4 or BIT_5 or BIT_6 or BIT_7)) then
          begin
            { Architecturally undefined }
            DecodeArchitecturallyUndefinedIns(Ins);
          end else if (W and BIT_4 <> $00) then
            DecodeMediaIns(Ins)
          else
            DecodeLoadStoreRegOffsetIns(Ins);
        end;
      4:
        begin
          DecodeLoadStoreMultipleIns(Ins);
        end;
      5:
        begin
          DecodeBranchIns(Ins);
        end;
      6:
        begin
          DecodeCoprocessorLoadStoreIns(Ins);
        end;
      7:
        begin
          if (W and BIT_24) <> $00 then
            DecodeSoftwareInterruptIns(Ins)
          else
            if (W and BIT_4) <> $00 then
              DecodeCoprocessorRegTransferIns(Ins)
            else
              DecodeCoprocessorDataIns(Ins);
        end;
    end;
  end;
end;

function DecodeARM(var Ins: TInstruction): ShortInt;
var
  Cond: Cardinal;
begin
  Result := SizeOf(TARMWord);
  Ins.FullOpCode := PARMWord(Ins.Address)^;
  Cond := (Ins.FullOpCode and CONDITION_MASK) shr 28;
  Ins.Condition := ShortInt(Cond);
  if Cond = CONDITION_NONE then
  begin

  end else begin
    DecodeConditionalIns(Ins);
  end;
end;

end.
