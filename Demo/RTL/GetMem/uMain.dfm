object Main: TMain
  Left = 0
  Top = 0
  Caption = 'Main'
  ClientHeight = 372
  ClientWidth = 351
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object LblTotalSize: TLabel
    Left = 71
    Top = 301
    Width = 99
    Height = 13
    Caption = 'Total Memory Used :'
  end
  object BtnViewMem: TButton
    Left = 29
    Top = 335
    Width = 306
    Height = 25
    Caption = 'View memory used by this App'
    TabOrder = 0
    OnClick = BtnViewMemClick
  end
  object ValueListEditor1: TValueListEditor
    Left = 29
    Top = 16
    Width = 306
    Height = 257
    TabOrder = 1
  end
end
