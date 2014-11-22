unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, Vcl.StdCtrls,
  Generics.Collections, Vcl.Grids, Vcl.ValEdit;

type
  TMain = class(TForm)
    BtnViewMem: TButton;
    ValueListEditor1: TValueListEditor;
    LblTotalSize: TLabel;
    procedure BtnViewMemClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Main: TMain;

implementation

uses DDetours;
{$R *.dfm}

{ This is a simple example showing how to hook RTL
  function .
  ------------------------------------------------
  in this example , i hooked the GetMem function
  to detect memory that is used by the current process .
  However, i have not hooked the FreeMem and you should not
  try to hook it because , when calling the InterceptCreate method
  from DDetours , the function call indirectly FreeMem to free RTID
  object before the Trampoline is initialized .
  The solution for this issue is very simple ,
  rather than getting the trampoline pointer from result ,
  we can pass it as a variable !!
  eg :
  InterceptCreate(Target, @InterceptTarget,@TrampoLine) .

  But for the moment ,
  i am not going to change the declaration of InterceptCreate !!
}
var
  TrampoGetMem: function(Size: NativeInt): Pointer;
  P: Pointer;
  MemList: TDictionary<Pointer, Integer>;

function GetMemAddr: Pointer;
asm
  { The delphi compiler will not allow us to access
  System.GetMem pointer , so we need to use assembly
  to access it .
   }
  {$IFDEF CPUX64}
  MOV RAX,offset System.@GetMem
  {$ELSE !CPUX64}
  MOV EAX,offset System.@GetMem
  {$ENDIF !CPUX64}
end;

function InterceptGetMem(Size: NativeInt): Pointer;
begin
  Result := TrampoGetMem(Size);
  if Assigned(MemList) then
  begin
    if not MemList.ContainsKey(Result) then
      MemList.Add(Result, Size);
  end;
end;

procedure TMain.BtnViewMemClick(Sender: TObject);
var
  Key: Pointer;
  Size, TotalSize: Integer;
  mbi: TMemoryBasicInformation;
  s: String;
begin
  BtnViewMem.Enabled := False;
  Sleep(50);
  ValueListEditor1.Strings.Clear;
  TotalSize := 0;
  for Key in MemList.Keys do
  begin
    Size := MemList[Key];
    if VirtualQuery(Key, mbi, Size) <> 0 then
      if (mbi.State and MEM_COMMIT = MEM_COMMIT) then
      begin
        Inc(TotalSize, Size);
        s := IntToHex(NativeUInt(Key), 8) + '=' + IntToStr(Size);
        ValueListEditor1.Strings.Add(s);
      end;
  end;
  LblTotalSize.Caption := Format('Total Memory Used : %d bytes.', [TotalSize]);
  BtnViewMem.Enabled := True;
end;

initialization

MemList := TDictionary<Pointer, Integer>.Create;
P := GetMemAddr;
@TrampoGetMem := InterceptCreate(P, @InterceptGetMem);

finalization

FreeAndNil(MemList);
InterceptRemove(P);

end.
