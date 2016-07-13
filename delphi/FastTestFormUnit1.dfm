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
    Width = 360
    Height = 18
    Caption = 'StatLabelStatLabelStatLabelStatLabel'
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
    Width = 865
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
    Left = 880
    Top = 8
    Width = 810
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
  object ProgressBar1: TProgressBar
    Left = 8
    Top = 768
    Width = 353
    Height = 17
    Min = 0
    Max = 100
    Smooth = True
    TabOrder = 2
  end
  object StopCheckBox: TCheckBox
    Left = 1592
    Top = 736
    Width = 97
    Height = 17
    Caption = 'StopCheckBox'
    TabOrder = 3
  end
  object Button1: TButton
    Left = 1504
    Top = 736
    Width = 75
    Height = 25
    Caption = 'START'
    TabOrder = 4
    OnClick = Button1Click
  end
  object Timer1mS: TTimer
    Interval = 1
    OnTimer = Timer1mSTimer
    Left = 40
    Top = 24
  end
  object Timer100ms: TTimer
    Interval = 100
    OnTimer = Timer100msTimer
    Left = 80
    Top = 64
  end
end
