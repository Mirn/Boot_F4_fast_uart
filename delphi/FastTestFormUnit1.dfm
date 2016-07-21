object Form1: TForm1
  Left = 192
  Top = 118
  BorderStyle = bsDialog
  Caption = 'SFU uploader'
  ClientHeight = 807
  ClientWidth = 1525
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    1525
    807)
  PixelsPerInch = 96
  TextHeight = 13
  object StatLabel: TLabel
    Left = 8
    Top = 727
    Width = 360
    Height = 18
    Anchors = [akLeft, akBottom]
    Caption = 'StatLabelStatLabelStatLabelStatLabel'
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Courier New'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object SFUboot_StatusLabel: TLabel
    Left = 8
    Top = 783
    Width = 360
    Height = 18
    Anchors = [akLeft, akBottom]
    Caption = 'StatLabelStatLabelStatLabelStatLabel'
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Courier New'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object LabelDev: TLabel
    Left = 544
    Top = 749
    Width = 80
    Height = 18
    Anchors = [akLeft, akBottom]
    Caption = ' Device:'
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Courier New'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object LabelBin: TLabel
    Left = 544
    Top = 781
    Width = 80
    Height = 18
    Anchors = [akLeft, akBottom]
    Caption = 'F/W bin:'
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Courier New'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Cap1Label: TLabel
    Left = 8
    Top = 2
    Width = 405
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'mSec     Com Device and Boot packet level log'
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Courier New'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Cap2Label: TLabel
    Left = 880
    Top = 2
    Width = 360
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'mSec    hstFifo BootCommnands    devFIFO'
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Courier New'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object ErrorsKeepCheckBox: TCheckBox
    Left = 1056
    Top = 759
    Width = 129
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'ErrorsKeep'
    Checked = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Courier New'
    Font.Style = [fsBold]
    ParentFont = False
    State = cbChecked
    TabOrder = 13
  end
  object MemoDevice: TMemo
    Left = 8
    Top = 16
    Width = 865
    Height = 709
    Anchors = [akLeft, akTop, akBottom]
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Courier New'
    Font.Style = [fsBold]
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object MemoCMD: TMemo
    Left = 880
    Top = 16
    Width = 642
    Height = 709
    Anchors = [akLeft, akTop, akBottom]
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Courier New'
    Font.Style = [fsBold]
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object ProgressBar: TProgressBar
    Left = 8
    Top = 759
    Width = 353
    Height = 17
    Anchors = [akLeft, akBottom]
    Min = 0
    Max = 100
    Smooth = True
    TabOrder = 2
  end
  object StopCheckBox: TCheckBox
    Left = 1472
    Top = 727
    Width = 49
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Stop'
    TabOrder = 3
  end
  object GoButton: TButton
    Left = 376
    Top = 751
    Width = 75
    Height = 49
    Anchors = [akLeft, akBottom]
    Caption = 'Run'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -32
    Font.Name = 'Courier New'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 4
    OnClick = GoButtonClick
  end
  object FastCheckBox: TCheckBox
    Left = 456
    Top = 751
    Width = 73
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Fast'
    Checked = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Courier New'
    Font.Style = [fsBold]
    ParentFont = False
    State = cbChecked
    TabOrder = 5
  end
  object DeviceEdit: TEdit
    Left = 624
    Top = 743
    Width = 425
    Height = 30
    Anchors = [akLeft, akBottom]
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Courier New'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 6
  end
  object ResetCheckBox: TCheckBox
    Left = 456
    Top = 767
    Width = 73
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Reset'
    Checked = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Courier New'
    Font.Style = [fsBold]
    ParentFont = False
    State = cbChecked
    TabOrder = 7
  end
  object FirmwareEdit: TEdit
    Left = 624
    Top = 775
    Width = 865
    Height = 30
    Anchors = [akLeft, akBottom]
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Courier New'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 8
  end
  object StopButton: TButton
    Left = 376
    Top = 654
    Width = 73
    Height = 49
    Anchors = [akLeft, akBottom]
    Caption = 'Stop'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clMaroon
    Font.Height = -24
    Font.Name = 'Courier New'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 9
    Visible = False
    OnClick = StopButtonClick
  end
  object ExitCheckBox: TCheckBox
    Left = 456
    Top = 783
    Width = 73
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Exit'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Courier New'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 10
  end
  object OpenFWButton: TButton
    Left = 1496
    Top = 776
    Width = 25
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '...'
    TabOrder = 11
    OnClick = OpenFWButtonClick
  end
  object PreWriteCheckBox: TCheckBox
    Left = 1056
    Top = 743
    Width = 105
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'PreWrite'
    Checked = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Courier New'
    Font.Style = [fsBold]
    ParentFont = False
    State = cbChecked
    TabOrder = 12
  end
  object Timer1mS: TTimer
    Interval = 1
    OnTimer = Timer1mSTimer
    Left = 40
    Top = 24
  end
  object Timer32ms: TTimer
    Interval = 32
    OnTimer = Timer32msTimer
    Left = 80
    Top = 64
  end
end
