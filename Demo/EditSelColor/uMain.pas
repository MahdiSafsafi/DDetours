{
  EditSelColor Demo by Mahdi Safsafi [SMP3]
  http://code.google.com/p/delphi-detours-library/
}
unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, DDetours, CommCtrl;

type
  TEdit = class(Vcl.StdCtrls.TEdit)
  private
    FSelColor: TColor;
  protected
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    property SelColor: TColor read FSelColor write FSelColor;
  end;

  TForm2 = class(TForm)
    ListBox1: TListBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

const
  CM_GETSELCOLOR = WM_USER + $199;

var
  TrampolineGetSysColor: function(nIndex: Integer): DWORD;
stdcall = nil;
EditHandle: HWND = 0;

function InterceptGetSysColor(nIndex: Integer): DWORD; stdcall;
begin
  if (nIndex = COLOR_HIGHLIGHT) and (EditHandle > 0) then
    if GetFocus = EditHandle then
    begin
      Result := SendMessage(EditHandle, CM_GETSELCOLOR, 0, 0);
      EditHandle := 0;
      Exit;
    end;
  Result := TrampolineGetSysColor(nIndex);
end;

{ TEdit }

constructor TEdit.Create(AOwner: TComponent);
begin
  inherited;
  FSelColor := TrampolineGetSysColor(COLOR_HIGHLIGHT);
end;

procedure TEdit.WndProc(var Message: TMessage);
begin
  case Message.Msg of

    CM_GETSELCOLOR:
      begin
        Message.Result := FSelColor;
        Exit;
      end;

    WM_MOUSEMOVE, WM_NCHITTEST:
      if Focused then
        EditHandle := Handle;

    WM_SETFOCUS, WM_KILLFOCUS: EditHandle := 0;
    EM_SETSEL, WM_LBUTTONDOWN .. WM_RBUTTONDBLCLK: EditHandle := Handle;
  end;

  inherited;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  Color := $00635E56;

  Edit1.SelColor := $001A79FB;
  Edit1.Color := $007F7E35;
  Edit1.Font.Color := clWhite;

  Edit2.SelColor := $0010C2FF;
  Edit2.Color := $00656565;
  Edit2.Font.Color := clWhite;

  Edit3.SelColor := clWebViolet;

  Edit4.SelColor := clRed;

end;

initialization

@TrampolineGetSysColor := InterceptCreate(@GetSysColor, @InterceptGetSysColor);

finalization

InterceptRemove(@TrampolineGetSysColor);

end.
