# Introduction #

The **InstDecode Library** is a library allowing decoding intel instructions . It support both x86-32bit and x86-64bit architectures and provide an easy way to  disassembler opcodes (without displaying opcodes mnemonics).

## Features : ##
  * 32bit & 64bit architectures support.
  * GP,FPU,MMX,SSE,SSE2,SSE3,SSSE3,SSE4.1,SSE4.2,SHA,AES,BMI,BMI2,AVX,FMA, instructions support.
  * Faster instructions decoding.
  * Small in size (the size of the compiled "InstDecode.dcu" is about 30 kb) .
  * One,Two,Three bytes opcodes escape support.
  * VEX (Two and Three bytes) decoding support.
  * Calculating Branch destination address.
  * ...

## DecodeInst function : ##
```
function DecodeInst(PInst: PInstruction): ShortInt;
```
This is the main function for this library .It can decode instructions , and retrieve useful information about the instruction that was decoded such as :
  * Instruction length.
  * OpCode value .
  * Displacement value .
  * Immediate value .
  * Branch destination.
  * Prefixes.
  * ...

### Params: ###
Only one parameter **PInst.**

**PInst :** Pointer to TInstruction structure.It contains the information returned by the **DecodeInst function** .

### TInstruction Structure : ###
```
TModRM = record
    iMod: Byte; { ModRm.Mod Field }
    Reg: Byte; { ModRm.Reg Field }
    Rm: Byte; { ModRm.Rm Field }
    Value: Byte; { ModRm Value }
    { ModRm.Flags => See ModRmFlagsTable.inc }
    Flags: Byte;
  end;

  PModRM = ^TModRM;
  LPModRM = PModRM;

  TSib = record
    Scale: Byte; { SIB.Scale Field }
    Index: Byte; { Register Index }
    Base: Byte; { Register Base }
    Value: Byte; { SIB Value }
    Flags: Byte; { SIB Flags }
  end;

  PSib = ^TSib;
  LPSib = PSib;

  TImmediat = record
    Size: Byte; { Size of Immediat => opsxxxbits }
    Value: Int64; { Immediat Value }
    Flags: Byte; { Sets of imfxxx }
  end;

  PImmediat = ^TImmediat;

  TDisplacement = record
    Size: Byte; { Size of Displacement => opsxxxbits }
    Value: Int64; { Displacement Value }
    Flags: Byte; { Sets of dfxxx }
  end;

  PDisplacement = ^TDisplacement;

  TBranch = record
    Size: Byte; { Branch size/length. }
    Value: Int64;
    Target: PByte; { Destination address }
    Falgs: Byte; { Sets of bfxxx }
  end;

  PBranch = ^TBranch;

  TRex = record
    R: Boolean; { REX.R Field }
    X: Boolean; { REX.X Field }
    B: Boolean; { REX.B Field }
    W: Boolean; { REX.W Field }
    Value: Byte; { REX value = [$40..$4F] }
  end;

  PRex = ^TRex;

  TVex = record
    {
      ==================> N.B <==================
      1 => ALL FIELD ARE IN NO INVERTED FORM !
      2 => VEX.[R,X,B & W] ARE ACCESSIBLE THROUGH REX FIELD !

    }
    vvvv: Byte; { VEX.vvvv ==> Vector Register }
    L: Boolean; { VEX.L ==> You should use VL instead ! }
    PP: Byte; { VEX.PP ==> Implied Mandatory Prefixes }
    mmmmm: Byte; { VEX.mmmmm ==> Implied Escape }
    VL: Byte; { Vector Length }
  end;

  PVex = ^TVex;

  TInternalData = record
    MndPrf: Byte; { Mandatory Prefix }
    zOpSize: Byte; { word or dword depending on opsize prefix ! }
    vOpSize: Byte; { word or dword or qword depending on opsize & REX prefix ! }
  end;

  PInternalData = ^TInternalData;

  TInstruction = record
    Archi: Byte; { CPUX32 or CPUX64 ! }
    AddrMode: Byte; { Address Mode }
    Addr: PByte; { The address where the library will start the decoding. }
    VirtualAddr: PByte; { VA to calculate branch destination. }
    NextInst: PByte; { Pointer to the Next Instruction }
    OpCode: Byte; { OpCode Value }
    OpType: Byte; { otCALL,otRET,otJMP,... }
    OpKind: Byte; { kGrp if inst escaped from group. }
    OpTable: Byte; { tbOneByte,tbTwoByte,... }
    OperandFlags: Byte; { one of this : opdxxx }
    Prefixes: Word; { Sets of Prf_xxx }
    ModRm: TModRM;
    Sib: TSib;
    Disp: TDisplacement;
    Imm: TImmediat; { Primary Immediat }
    ImmEx: TImmediat; { Secondary Immediat if used ! }
    Branch: TBranch; { JMP & CALL }
    SegReg: Byte; { Segment Register }
    Rex: TRex; { Rex Structure }
    Vex: TVex; { Vex Structure }
    LID: TInternalData; { Internal Data }
    Errors: Byte; { Decoding Errors }
    InstSize: Byte; { Instruction Length }
    Options: Byte;
    UserTag: UInt64; { User Data }
  end;

  PInstruction = ^TInstruction;

```
### Example : ###
Here is a sample example showing how to use this library to decode instructions.
This example will illustrates all instructions used in the "Foo function".
```
uses
  System.SysUtils,
  InstDecode;

procedure Foo;
asm
  {$IFDEF CPUX64}
  PUSH RAX
  XOR EAX,EAX
  MOV EAX,5
  ADD EAX,EDX
  POP RAX
  NOP
  NOP
  NOP
  MOV RAX,1
  {$ELSE !CPUX64}
  PUSH EAX
  XOR EAX,EAX
  MOV EAX,5
  ADD EAX,EDX
  POP EAX
  NOP
  NOP
  NOP
  MOV EAX,1
  {$ENDIF CPUX64}
end;

var
  Inst: TInstruction;
  nInst: Integer;

begin
  //Foo;
  Inst := Default (TInstruction);
  Inst.Archi := CPUX;
  Inst.NextInst := @Foo;
  nInst := 0;
  while (Inst.OpType <> otRET) do
  begin
    inc(nInst);
    Inst.Addr := Inst.NextInst;
    DecodeInst(@Inst);
    Writeln(Format('OpCode : 0x%.2x | Length : %d', [Inst.OpCode, Inst.InstSize]));
  end;
  Writeln('-------------------------------');
  Writeln(Format('Total instructions : %d', [nInst]));
  ReadLn;

end.
```