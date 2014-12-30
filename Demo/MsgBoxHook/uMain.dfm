object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Main'
  ClientHeight = 137
  ClientWidth = 289
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object SpeedButton1: TSpeedButton
    Left = 80
    Top = 32
    Width = 129
    Height = 22
    Caption = 'MessageBox'
    OnClick = SpeedButton1Click
  end
  object SpeedButton2: TSpeedButton
    Left = 80
    Top = 60
    Width = 129
    Height = 22
    Caption = 'Insert Hook'
    OnClick = SpeedButton2Click
  end
  object SpeedButton3: TSpeedButton
    Left = 80
    Top = 88
    Width = 129
    Height = 22
    Caption = 'Remove Hook'
    OnClick = SpeedButton3Click
  end
end
