object MainForm: TMainForm
  Left = 342
  Top = 333
  Width = 724
  Height = 490
  Caption = 'Find the Border'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = mainMenu
  OldCreateOrder = False
  WindowState = wsMaximized
  OnKeyPress = FormKeyPress
  OnMouseDown = FormMouseDown
  OnMouseMove = FormMouseMove
  OnMouseUp = FormMouseUp
  OnPaint = FormPaint
  OnResize = FormPaint
  PixelsPerInch = 96
  TextHeight = 13
  object mainMenu: TMainMenu
    Left = 16
    Top = 48
    object File1: TMenuItem
      Caption = 'File'
      object Openinput1: TMenuItem
        Caption = 'Open Input'
        ShortCut = 114
        OnClick = Openinput1Click
      end
      object Openoutput1: TMenuItem
        Caption = 'Save Input'
        ShortCut = 113
        OnClick = Openoutput1Click
      end
      object OpenOutput2: TMenuItem
        Caption = 'Open Output'
        ShortCut = 8306
        OnClick = OpenOutput2Click
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object ClearInput1: TMenuItem
        Caption = 'Clear Input'
        ShortCut = 46
        OnClick = ClearInput1Click
      end
      object ClearOutput1: TMenuItem
        Caption = 'Clear Output'
        ShortCut = 8238
        OnClick = ClearOutput1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'Exit'
        ShortCut = 32856
        OnClick = Exit1Click
      end
    end
    object View1: TMenuItem
      Caption = 'View'
      object labelsMenuItem: TMenuItem
        Caption = 'Labels'
        ShortCut = 76
        OnClick = labelsMenuItemClick
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object NextTest1: TMenuItem
        Caption = 'Next Test'
        ShortCut = 78
        OnClick = NextTest1Click
      end
      object PreviousTest1: TMenuItem
        Caption = 'Previous Test'
        ShortCut = 80
        OnClick = PreviousTest1Click
      end
    end
  end
  object openDialog: TOpenDialog
    Left = 48
    Top = 48
  end
  object saveDialog: TSaveDialog
    Left = 80
    Top = 48
  end
end
