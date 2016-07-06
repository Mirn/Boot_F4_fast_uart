object Form1: TForm1
  Left = 192
  Top = 118
  Width = 1661
  Height = 835
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
    1651
    803)
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 8
    Top = 8
    Width = 1621
    Height = 743
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Courier New'
    Font.Style = [fsBold]
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object Timer1mS: TTimer
    Interval = 1
    OnTimer = Timer1mSTimer
    Left = 40
    Top = 24
  end
end
