object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'QuickJS Runner'
  ClientHeight = 480
  ClientWidth = 720
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 15
  object PanelTop: TPanel
    Left = 0
    Top = 0
    Width = 720
    Height = 48
    Align = alTop
    TabOrder = 0
    object lblInfo: TLabel
      Left = 96
      Top = 14
      Width = 500
      Height = 15
      Caption = 'Running file: main.js next to executable (also place fetch.js and quickjs64.dll)'
    end
    object btnRun: TButton
      Left = 8
      Top = 10
      Width = 75
      Height = 27
      Caption = 'Run'
      TabOrder = 0
      OnClick = btnRunClick
    end
  end
  object MemoConsole: TMemo
    Left = 0
    Top = 48
    Width = 720
    Height = 432
    Align = alClient
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
  end
end
