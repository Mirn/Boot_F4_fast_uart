object Form1: TForm1
  Left = 192
  Top = 118
  Width = 1710
  Height = 831
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    1700
    799)
  PixelsPerInch = 96
  TextHeight = 13
  object StatLabel: TLabel
    Left = 8
    Top = 744
    Width = 90
    Height = 18
    Caption = 'StatLabel'
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Courier New'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object MemoDevice: TMemo
    Left = 8
    Top = 8
    Width = 833
    Height = 725
    Anchors = [akLeft, akTop, akBottom]
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Courier New'
    Font.Style = [fsBold]
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object MemoCMD: TMemo
    Left = 848
    Top = 8
    Width = 842
    Height = 725
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Courier New'
    Font.Style = [fsBold]
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object Timer1mS: TTimer
    Interval = 1
    OnTimer = Timer1mSTimer
    Left = 40
    Top = 24
  end
end
