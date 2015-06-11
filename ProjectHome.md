The **Delphi Detours Library** is a library allowing you to hook delphi and windows API functions .It provides an easy way to insert and remove hook .



## What's new in Version 2: ##
Please check [Version2](Version2.md) wiki page
## Features : ##
  * Support **x86** and **x64** architecture .
  * Allow calling the original function via <u><b>Trampoline</b></u> function.
  * Support for <u><b>Multi Hook</b></u>.
  * **COM**/**Interfaces**/**win32api** support.
  * Support COM **vtable** patching.
  * Fully <u><b>thread-safe</b></u> code hooking and unhooking .
  * Support hooking Object Method .
  * Supported Delphi 7/2005-2010/XE-XE8 .
  * Support Lazarus/FPC .
  * 64 bit address is supported .
  * The library does not use any external library .
  * The library can insert and remove the hook at any time .
  * The library contains InstDecode library , that allow to you to decode asm instructions (x86 & x64).

This project contians two sub projects : **The Detours Library** and **the InstDecode library** .

The InstDecode Library is a library that can decode both (x86-x64) instructions .You can consider it as a small disassembler routine .
It can decode instruction and getting information about the instruction (size of instruction,displacement,immediate data, jump address,..) without displaying mnemonics making it very faster and small in compiled size.

This two library was coded in pure pascal language with delphi XE7.

See the [Wiki](http://code.google.com/p/delphi-detours-library/w/list) page for more information about how to use the library .

Please , if you find any bug , feel free to report it .