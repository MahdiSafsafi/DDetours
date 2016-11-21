unit InstDecode;
{ Not yet =>  Use x86Decoder unit. }
{ ===============================> CHANGE LOG <======================================================
  ==>  Version



  ==> Version 3 (Nov 13,2016):
  +Updated opcodes map.
  +Added legacy instructions identifiers.
  +Added address relative position to disp,imm,... fields.
  +BugFix: fixed many bugs when opcode is two byte.
  +BugFix: some instructions that use mandatory prefixes.
  +BugFix: when decoding invalid opcode from group.
  +BugFix: when decoding invalid FPU instructions.
  +Improved FPU instructions decoding.
  +Improved branch target calculation when offset is used.
  +Improved ModRm decoding.
  +Handled error when calculating branch target address.
  +Removed mandatory prefixes check.
  +Removed unnecessary fields.

  ==> Dec 27,2014 , Mahdi Safsafi :
  +BugFix : IN/INS/OUT/OUTS instructions decoding.
  +BugFix : MOV with offset instructions decoding.

  ==> Version 2 (Nov,22,2014):
  +Updated opcodes map .
  +Added support to three byte escape Table
  +Added support to vex decoding (vex three & two byte).
  +Added support to groups opcodes instructions.
  +Added support to decode invalid opcode .
  +Added support to 16-bits ModRm .
  +Added support to handling errors.
  +Added support for mandatory prefixes.
  +Improve Decoding Process .=> Very faster than the old one !
  +Reduce memory usage .
  +Removing inused fields.
  +Better support for REX prefix.
  +Reduce OpCodesTable data size (the old : 8670 bytes => the new one : 1020 bytes !)
  +BugFix : FPU instructions length.
  +BugFix : Instructions that use two immediat .
  +BugFix : Invalid instructions .
  +BugFix : Invalid instructions for some mandatory prefixes.
  +Many Bug Fix.
}

interface

implementation

end.
