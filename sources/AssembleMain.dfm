object frmAssemble: TfrmAssemble
  Left = 352
  Top = 246
  Caption = 'Embed translations in executable'
  ClientHeight = 288
  ClientWidth = 491
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    491
    288)
  PixelsPerInch = 96
  TextHeight = 13
  object LabelExplanation: TLabel
    Left = 10
    Top = 50
    Width = 469
    Height = 13
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 
      'The following translations will be embedded into the executable ' +
      'file:'
    WordWrap = True
    ExplicitWidth = 461
  end
  object LabelExeHeading: TLabel
    Left = 10
    Top = 10
    Width = 74
    Height = 13
    Caption = 'Executable file:'
  end
  object LabelExeName: TLabel
    Left = 10
    Top = 30
    Width = 469
    Height = 13
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    ExplicitWidth = 461
  end
  object laVersion: TLabel
    Left = 80
    Top = 259
    Width = 3
    Height = 13
    Anchors = [akLeft, akBottom]
  end
  object ListBoxTranslations: TCheckListBox
    Left = 10
    Top = 70
    Width = 471
    Height = 171
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 0
  end
  object bbAll: TBitBtn
    Left = 10
    Top = 250
    Width = 31
    Height = 31
    Anchors = [akLeft, akBottom]
    Glyph.Data = {
      96090000424D9609000000000000360000002800000028000000140000000100
      18000000000060090000120B0000120B00000000000000000000C0DCC0C0DCC0
      C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DC
      C0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0
      DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0
      C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DC
      C0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0
      DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0
      C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DC
      C0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC07A3F05C0DCC0C0DCC0C0
      DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0
      C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0404040C0DCC0C0DCC0C0DCC0C0DC
      C0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0
      DCC0C0DCC0C0DCC0C0DCC07A3F057A3F05C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0
      C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DC
      C0C0DCC0C0DCC0404040404040C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0
      DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0
      7A3F057A3F057A3F057A3F05C0DCC0C0DCC0C0DCC09966339966339966339966
      33996633996633996633996633996633C0DCC0C0DCC0C0DCC0C0DCC040404040
      4040404040404040C0DCC0C0DCC0C0DCC0666666666666666666666666666666
      666666666666666666666666C0DCC0C0DCC0C0DCC0C0DCC07A3F05C0DCC0C0DC
      C07A3F057A3F05C0DCC0C0DCC099663399663399663399663399663399663399
      6633996633996633C0DCC0C0DCC0C0DCC0C0DCC0404040C0DCC0C0DCC0404040
      404040C0DCC0C0DCC06666666666666666666666666666666666666666666666
      66666666C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC07A3F05C0
      DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0
      C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0404040C0DCC0C0DC
      C0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0
      DCC0C0DCC0C0DCC0C0DCC0C0DCC07A3F05C0DCC0C0DCC07A3F05C0DCC0C0DCC0
      C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DC
      C0C0DCC0C0DCC0C0DCC0404040C0DCC0C0DCC0404040C0DCC0C0DCC0C0DCC0C0
      DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0
      C0DCC07A3F057A3F05C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DC
      C0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC040
      4040404040C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0
      C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC07A3F057A3F057A3F
      057A3F05C0DCC0C0DCC0C0DCC099663399663399663399663399663399663399
      6633996633996633C0DCC0C0DCC0C0DCC0C0DCC0404040404040404040404040
      C0DCC0C0DCC0C0DCC06666666666666666666666666666666666666666666666
      66666666C0DCC0C0DCC0C0DCC0C0DCC07A3F05C0DCC0C0DCC07A3F057A3F05C0
      DCC0C0DCC0996633996633996633996633996633996633996633996633996633
      C0DCC0C0DCC0C0DCC0C0DCC0404040C0DCC0C0DCC0404040404040C0DCC0C0DC
      C0666666666666666666666666666666666666666666666666666666C0DCC0C0
      DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC07A3F05C0DCC0C0DCC0C0DCC0
      C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DC
      C0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0404040C0DCC0C0DCC0C0DCC0C0DCC0C0
      DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0
      C0DCC0C0DCC07A3F05C0DCC0C0DCC07A3F05C0DCC0C0DCC0C0DCC0C0DCC0C0DC
      C0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0
      DCC0404040C0DCC0C0DCC0404040C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0
      C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC07A3F057A3F
      05C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0
      DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0404040404040C0DCC0
      C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DC
      C0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC07A3F057A3F057A3F057A3F05C0DCC0C0
      DCC0C0DCC0996633996633996633996633996633996633996633996633996633
      C0DCC0C0DCC0C0DCC0C0DCC0404040404040404040404040C0DCC0C0DCC0C0DC
      C0666666666666666666666666666666666666666666666666666666C0DCC0C0
      DCC0C0DCC0C0DCC07A3F05C0DCC0C0DCC07A3F057A3F05C0DCC0C0DCC0996633
      996633996633996633996633996633996633996633996633C0DCC0C0DCC0C0DC
      C0C0DCC0404040C0DCC0C0DCC0404040404040C0DCC0C0DCC066666666666666
      6666666666666666666666666666666666666666C0DCC0C0DCC0C0DCC0C0DCC0
      C0DCC0C0DCC0C0DCC0C0DCC07A3F05C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DC
      C0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0
      DCC0C0DCC0C0DCC0404040C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0
      C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DC
      C0C0DCC0C0DCC07A3F05C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0
      DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0
      C0DCC0404040C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DC
      C0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0
      DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0
      C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DC
      C0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0
      DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0
      C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DC
      C0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0
      DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0}
    NumGlyphs = 2
    TabOrder = 1
    OnClick = bbAllClick
  end
  object bbNone: TBitBtn
    Left = 45
    Top = 250
    Width = 31
    Height = 31
    Anchors = [akLeft, akBottom]
    Glyph.Data = {
      96090000424D9609000000000000360000002800000028000000140000000100
      18000000000060090000120B0000120B00000000000000000000C0DCC0C0DCC0
      C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DC
      C0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0
      DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0
      C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DC
      C0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0
      DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0
      C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DC
      C0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0
      DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0
      C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DC
      C0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0
      DCC0C0DCC0C0DCC0C0DCC0000000000000000000000000C0DCC0C0DCC0C0DCC0
      C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DC
      C0C0DCC0C0DCC04F4F4F4F4F4F4F4F4F4F4F4FC0DCC0C0DCC0C0DCC0C0DCC0C0
      DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0
      C0DCC0000000F0F0F0F0F0F0000000C0DCC0C0DCC09966339966339966339966
      33996633996633996633996633996633C0DCC0C0DCC0C0DCC0C0DCC0C0DCC04F
      4F4FECECECECECEC4F4F4FC0DCC0C0DCC0707070707070707070707070707070
      707070707070707070707070C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0000000F0F0
      F0F0F0F0000000C0DCC0C0DCC099663399663399663399663399663399663399
      6633996633996633C0DCC0C0DCC0C0DCC0C0DCC0C0DCC04F4F4FECECECECECEC
      4F4F4FC0DCC0C0DCC07070707070707070707070707070707070707070707070
      70707070C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0000000000000000000000000C0
      DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0
      C0DCC0C0DCC0C0DCC0C0DCC0C0DCC04F4F4F4F4F4F4F4F4F4F4F4FC0DCC0C0DC
      C0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0
      DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0
      C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DC
      C0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0
      DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0
      C0DCC0000000000000000000000000C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DC
      C0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC04F
      4F4F4F4F4F4F4F4F4F4F4FC0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0
      C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0000000F0F0
      F0F0F0F0000000C0DCC0C0DCC099663399663399663399663399663399663399
      6633996633996633C0DCC0C0DCC0C0DCC0C0DCC0C0DCC04F4F4FECECECECECEC
      4F4F4FC0DCC0C0DCC07070707070707070707070707070707070707070707070
      70707070C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0000000F0F0F0F0F0F0000000C0
      DCC0C0DCC0996633996633996633996633996633996633996633996633996633
      C0DCC0C0DCC0C0DCC0C0DCC0C0DCC04F4F4FECECECECECEC4F4F4FC0DCC0C0DC
      C0707070707070707070707070707070707070707070707070707070C0DCC0C0
      DCC0C0DCC0C0DCC0C0DCC0000000000000000000000000C0DCC0C0DCC0C0DCC0
      C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DC
      C0C0DCC0C0DCC04F4F4F4F4F4F4F4F4F4F4F4FC0DCC0C0DCC0C0DCC0C0DCC0C0
      DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0
      C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DC
      C0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0
      DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0
      C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC00000000000
      00000000000000C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0
      DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC04F4F4F4F4F4F4F4F4F
      4F4F4FC0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DC
      C0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0000000F0F0F0F0F0F0000000C0
      DCC0C0DCC0996633996633996633996633996633996633996633996633996633
      C0DCC0C0DCC0C0DCC0C0DCC0C0DCC04F4F4FECECECECECEC4F4F4FC0DCC0C0DC
      C0707070707070707070707070707070707070707070707070707070C0DCC0C0
      DCC0C0DCC0C0DCC0C0DCC0000000F0F0F0F0F0F0000000C0DCC0C0DCC0996633
      996633996633996633996633996633996633996633996633C0DCC0C0DCC0C0DC
      C0C0DCC0C0DCC04F4F4FECECECECECEC4F4F4FC0DCC0C0DCC070707070707070
      7070707070707070707070707070707070707070C0DCC0C0DCC0C0DCC0C0DCC0
      C0DCC0000000000000000000000000C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DC
      C0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC04F
      4F4F4F4F4F4F4F4F4F4F4FC0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0
      C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DC
      C0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0
      DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0
      C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DC
      C0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0
      DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0
      C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DC
      C0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0
      DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0
      C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DC
      C0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0
      DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0}
    NumGlyphs = 2
    TabOrder = 2
    OnClick = bbNoneClick
  end
  object ButtonCancel: TBitBtn
    Left = 390
    Top = 251
    Width = 91
    Height = 31
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    Glyph.Data = {
      36060000424D3606000000000000360000002800000020000000100000000100
      18000000000000060000120B0000120B00000000000000000000C0DCC0C0DCC0
      C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DC
      C0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0
      DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0
      C0DCC00D15AB0D15ABC0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC00D15AB0D15
      ABC0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC05E5E5E5E5E5EC0DCC0C0DCC0C0
      DCC0C0DCC0C0DCC0C0DCC05E5E5E5E5E5EC0DCC0C0DCC0C0DCC0C0DCC0C0DCC0
      1018AB171FB0171FB01018ABC0DCC0C0DCC0C0DCC0C0DCC01018AB171FB0171F
      B01018ABC0DCC0C0DCC0C0DCC0C0DCC0606060666666666666606060C0DCC0C0
      DCC0C0DCC0C0DCC0606060666666666666606060C0DCC0C0DCC0C0DCC0131BAC
      1A22B12B33C12B33C11A22B2121AADC0DCC0C0DCC0121AAD1A22B22B33C22B33
      C11A22B2131BACC0DCC0C0DCC06262626868687B7B7B7B7B7B696969626262C0
      DCC0C0DCC06262626969697C7C7C7B7B7B696969626262C0DCC0C0DCC0141CAE
      1D25B32B33C92B33C72C34CA1D25B3141CAE141CAE1D25B32C34CA2B33C72B33
      C91D25B3141CAEC0DCC0C0DCC06464646B6B6B8080807F7F7F8181816B6B6B64
      64646464646B6B6B8181817F7F7F8080806B6B6B646464C0DCC0C0DCC0C0DCC0
      161EAF1F27B62C35D12C34CE2C35D12129B62129B62C35D12C34CE2C35D11F27
      B6161EAFC0DCC0C0DCC0C0DCC0C0DCC06666666E6E6E8585858383838585856F
      6F6F6F6F6F8585858383838585856E6E6E666666C0DCC0C0DCC0C0DCC0C0DCC0
      C0DCC01820B1232BB72C35DB2B34D52C35DC2C35DC2B34D62C35DB232BB71820
      B1C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC06767677171718B8B8B8787878C
      8C8C8C8C8C8888888B8B8B717171676767C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0
      C0DCC0C0DCC01A22B2272FB92B35E42B34E32B34E32B35E4272FB91A22B2C0DC
      C0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC06969697575759090908F
      8F8F8F8F8F909090757575696969C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0
      C0DCC0C0DCC01C24B42A32BD2C36EB2A34EB2A34EB2C36EB2B32BD1C24B4C0DC
      C0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC06B6B6B78787894949493
      93939393939494947979796B6B6BC0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0
      C0DCC01E26B52B32BD303AEE2833EE303AEF303AEF2933EC303AEE2B32BD1E26
      B5C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC06D6D6D79797998989894949499
      99999999999393939898987979796D6D6DC0DCC0C0DCC0C0DCC0C0DCC0C0DCC0
      2028B62C33C0343DF02C36F0353FF02C34C42C34C4353FF02D37EF343DF02B33
      C22028B6C0DCC0C0DCC0C0DCC0C0DCC06E6E6E7B7B7B9C9C9C9797979C9C9C7D
      7D7D7D7D7D9C9C9C9797979C9C9C7C7C7C6E6E6EC0DCC0C0DCC0C0DCC0222AB7
      2C34C53943F0313BF03B44F02D35C5222AB7222AB72D35C53B44F0313BF03943
      F02C34C5232BB7C0DCC0C0DCC07070707E7E7E9F9F9F9A9A9AA0A0A07E7E7E70
      70707070707E7E7EA0A0A09A9A9A9F9F9F7E7E7E717171C0DCC0C0DCC0252CB9
      2D35C84049F14049F12E36C9242BB9C0DCC0C0DCC0242BB92E36C94049F14049
      F12D35C8252CB9C0DCC0C0DCC0737373808080A3A3A3A3A3A3818181727272C0
      DCC0C0DCC0727272818181A3A3A3A3A3A3808080737373C0DCC0C0DCC0C0DCC0
      262DBA2E36CB2E36CB262DBAC0DCC0C0DCC0C0DCC0C0DCC0262DBA2E36CC2E36
      CB262DBAC0DCC0C0DCC0C0DCC0C0DCC0747474828282828282747474C0DCC0C0
      DCC0C0DCC0C0DCC0747474838383838383747474C0DCC0C0DCC0C0DCC0C0DCC0
      C0DCC0272FBB2830BCC0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC02830BC2830
      BCC0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0767676767676C0DCC0C0DCC0C0
      DCC0C0DCC0C0DCC0C0DCC0767676767676C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0
      C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DC
      C0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0
      DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0}
    NumGlyphs = 2
    ParentDoubleBuffered = True
    TabOrder = 3
    OnClick = ButtonCancelClick
  end
  object ButtonOK: TBitBtn
    Left = 300
    Top = 251
    Width = 83
    Height = 31
    Anchors = [akRight, akBottom]
    Caption = 'Start'
    Default = True
    Glyph.Data = {
      36060000424D3606000000000000360000002800000020000000100000000100
      18000000000000060000120B0000120B00000000000000000000C0DCC0C0DCC0
      C0DCC0C0DCC0C0DCC000B60000B80000AE0000AF0000B80000BA00C0DCC0C0DC
      C0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC07979797A7A7A75
      75757575757A7A7A7B7B7BC0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0
      C0DCC000BC0000BC0000B20000AA0000A80000A80000AB0000B30000BD0000B9
      00C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC07C7C7C7C7C7C77777773737372
      72727272727373737777777C7C7C7A7A7AC0DCC0C0DCC0C0DCC0C0DCC0C0DCC0
      00BA0000B80000AD0000A50000A40700A60E00A60E00A40700A40000AD0000B7
      0000B800C0DCC0C0DCC0C0DCC0C0DCC07B7B7B7A7A7A74747470707070707071
      71717171717070707070707474747979797A7A7AC0DCC0C0DCC0C0DCC000B300
      00B40000AD0072CF767AD5880AB53000BA3100BA3100B62900B31C00AB0700AE
      0000B40000B300C0DCC0C0DCC0777777787878747474BFBFBFC6C6C67D7D7D7B
      7B7B7B7B7B797979777777737373757575787878777777C0DCC0C0DCC000B200
      00B00000B00780D18DFFFFFFDCF4E455CE8301C55100C84A00C23900B92200B0
      0700B00000B200C0DCC0C0DCC0777777767676767676C7C7C7FFFFFFFFFFFFAF
      AFAF8181818282827F7F7F7A7A7A767676767676777777C0DCC000AE0000B100
      00B50000B91B80CC91FFFFFFFFFFFFFFFFFFC4EBD638C07400C84F00C83900BF
      1C00B50000B00000AF007575757676767878787A7A7AC4C4C4FFFFFFFFFFFFFF
      FFFFF5F5F59A9A9A8282828282827D7D7D78787876767675757500AA0000B300
      00BC0700BF2680C490FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFAAD9BA26B04E00BF
      2600BC0700B30000AD007373737777777C7C7C7D7D7DC0C0C0FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFDFDFDF8989897D7D7D7C7C7C77777774747400AC0000B400
      00C00E00C12D80BA8CFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFDFEFD9FC8
      A41C9B2300B60000A7007474747878787E7E7E7F7F7FBBBBBBFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFD2D2D279797979797971717100A60000B100
      00C00E00C02C80AD84FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFDFEFDA2C2
      A418931E00B20000A4007171717676767E7E7E7E7E7EB4B4B4FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFD0D0D0737373777777707070009D0000AD00
      02BE090FC23183B68BFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBDDAC437994C0EBA
      2E02BD0900AC0000A3006C6C6C7474747E7E7E878787BABABAFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFE9E9E98686868282827D7D7D7474746F6F6F009F0000A400
      28C2283AC94D8CC295FFFFFFFFFFFFFFFFFFDBEFE261B47C34C8693EDB693ED3
      5328C12800A300009E006D6D6D7070709393939F9F9FC5C5C5FFFFFFFFFFFFFF
      FFFFFFFFFFA8A8A89C9C9CAAAAAAA6A6A69393936F6F6F6D6D6DC0DCC0009D00
      22B8225ACF5E9BD0A1FFFFFFF0F9F38ECDA04FCA7B5EE28C5EE1825EDC735ED5
      6220B720009600C0DCC0C0DCC06C6C6C8B8B8BB2B2B2D4D4D4FFFFFFFFFFFFCC
      CCCCAAAAAABEBEBEBEBEBEBBBBBBB7B7B78A8A8A696969C0DCC0C0DCC0009B00
      02A1025FCA5FA7D8A9B4DFB978D5897EE5967EE6977EE4937EE28C7EDE8261D0
      6102A002009D00C0DCC0C0DCC06B6B6B6F6F6FB2B2B2DDDDDDE7E7E7C5C5C5D0
      D0D0D0D0D0CFCFCFCECECECCCCCCB6B6B66F6F6F6C6C6CC0DCC0C0DCC0C0DCC0
      0097000DA60D76D1769DE29D9EE6A19EE7A39EE6A49EE5A19EE39E79D5790CA5
      0C009B00C0DCC0C0DCC0C0DCC0C0DCC0696969777777C2C2C2DDDDDDE0E0E0E0
      E0E0E0E0E0DFDFDFDEDEDEC5C5C57676766B6B6BC0DCC0C0DCC0C0DCC0C0DCC0
      C0DCC0009A000298024ABE4A93DB93B1E7B1B1E7B192DB9248BC48029702009A
      00C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC06B6B6B6B6B6BA2A2A2D5D5D5EA
      EAEAEAEAEAD5D5D5A0A0A06A6A6A6B6B6BC0DCC0C0DCC0C0DCC0C0DCC0C0DCC0
      C0DCC0C0DCC0C0DCC0009B00009800009500009600009A00009D00C0DCC0C0DC
      C0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC06B6B6B6A6A6A68
      68686969696B6B6B6C6C6CC0DCC0C0DCC0C0DCC0C0DCC0C0DCC0}
    NumGlyphs = 2
    ParentDoubleBuffered = True
    TabOrder = 4
    OnClick = ButtonOKClick
  end
  object btnHelp: TBitBtn
    Left = 265
    Top = 252
    Width = 31
    Height = 31
    Hint = 'Show program help'
    Anchors = [akRight, akBottom]
    Glyph.Data = {
      96090000424D9609000000000000360000002800000028000000140000000100
      18000000000060090000120B0000120B00000000000000000000C0DCC0C0DCC0
      C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DC
      C0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0
      DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0
      C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DC
      C0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0
      DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0
      C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DC
      C0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0
      DCC0C0DCC0422104422104C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0
      C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DC
      C0404040404040C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0
      DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0422104422104808080
      804715422104C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DC
      C0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0404040404040BEBEBE7D7D7D40
      4040C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0
      C0DCC0C0DCC0C0DCC0422104422104808080C0C0C0FFFFFFC0C0C08047154221
      04C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0
      DCC0C0DCC0404040404040BEBEBEEEEEEEFFFFFFEEEEEE7D7D7D404040C0DCC0
      C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC04221044221
      04808080C0C0C0FFFFFFFFFFFF808080C0C0C0C0C0C0804715422104C0DCC0C0
      DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0404040404040BEBEBE
      EEEEEEFFFFFFFFFFFFBEBEBEEEEEEEEEEEEE7D7D7D404040C0DCC0C0DCC0C0DC
      C0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0422104808080C0C0C0FFFFFFFFFFFF80
      8080808080422104808080C0C0C0C0C0C0804715422104C0DCC0C0DCC0C0DCC0
      C0DCC0C0DCC0C0DCC0C0DCC0404040BEBEBEEEEEEEFFFFFFFFFFFFBEBEBEBEBE
      BE404040BEBEBEEEEEEEEEEEEE7D7D7D404040C0DCC0C0DCC0C0DCC0C0DCC0C0
      DCC0C0DCC0C0DCC0422104808080FFFFFF808080808080422104422104D4731E
      422104808080C0C0C0C0C0C0804715422104C0DCC0C0DCC0C0DCC0C0DCC0C0DC
      C0C0DCC0404040BEBEBEFFFFFFBEBEBEBEBEBE4040404040407D7D7D404040BE
      BEBEEEEEEEEEEEEE7D7D7D404040C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0
      422104808080808080422104422104D4731ED4731ED4731ED4731E4221048080
      80C0C0C0C0C0C0804715422104C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0404040BE
      BEBEBEBEBE4040404040407D7D7D7D7D7D7D7D7D7D7D7D404040BEBEBEEEEEEE
      EEEEEE7D7D7D404040C0DCC0C0DCC0C0DCC0C0DCC0C0DCC04221044221044221
      04D4731ED4731ED4731ED4731ED4731ED4731ED4731E422104808080C0C0C0C0
      C0C0804715422104C0DCC0C0DCC0C0DCC0C0DCC04040404040404040407D7D7D
      7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D404040BEBEBEEEEEEEEEEEEE7D7D
      7D404040C0DCC0C0DCC0C0DCC0C0DCC0422104808080D4731ED4731ED4731ED4
      731E00FFFF008080D4731ED4731ED4731E422104808080C0C0C0422104422104
      C0DCC0C0DCC0C0DCC0C0DCC0404040BEBEBE7D7D7D7D7D7D7D7D7D7D7D7DBEBE
      BE6F6F6F7D7D7D7D7D7D7D7D7D404040BEBEBEEEEEEE404040404040C0DCC0C0
      DCC0C0DCC0C0DCC0C0DCC0422104808080D4731ED4731ED4731ED4731EC0C0C0
      00FFFF00FFFFD4731ED4731E422104808080422104C0DCC0C0DCC0C0DCC0C0DC
      C0C0DCC0C0DCC0404040BEBEBE7D7D7D7D7D7D7D7D7D7D7D7DEEEEEEBEBEBEBE
      BEBE7D7D7D7D7D7D404040BEBEBE404040C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0
      C0DCC0C0DCC0422104808080D4731ED4731E00808000808080471500FFFF00FF
      FFD4731ED4731E422104422104C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0
      DCC0404040BEBEBE7D7D7D7D7D7D6F6F6F6F6F6F7D7D7DBEBEBEBEBEBE7D7D7D
      7D7D7D404040404040C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DC
      C0422104808080D4731ED4731E00FFFF00FFFF00FFFF008080D4731ED4731ED4
      731E422104C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0404040
      BEBEBE7D7D7D7D7D7DBEBEBEBEBEBEBEBEBE6F6F6F7D7D7D7D7D7D7D7D7D4040
      40C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC042210480
      8080D4731ED4731ED4731ED4731ED4731ED4731ED4731E422104422104C0DCC0
      C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0404040BEBEBE7D7D
      7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D404040404040C0DCC0C0DCC0C0
      DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0422104808080D4731E
      D4731ED4731ED4731E422104422104C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DC
      C0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0404040BEBEBE7D7D7D7D7D7D7D
      7D7D7D7D7D404040404040C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0
      C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0422104808080D4731E4221044221
      04C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0
      DCC0C0DCC0C0DCC0C0DCC0C0DCC0404040BEBEBE7D7D7D404040404040C0DCC0
      C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DC
      C0C0DCC0C0DCC0C0DCC0C0DCC0422104422104C0DCC0C0DCC0C0DCC0C0DCC0C0
      DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0
      C0DCC0C0DCC0C0DCC0404040404040C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DC
      C0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0
      DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0
      C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DC
      C0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0
      DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0
      C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DC
      C0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0
      DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0}
    NumGlyphs = 2
    ParentShowHint = False
    ShowHint = True
    TabOrder = 5
    OnClick = btnHelpClick
  end
  object btnManual: TBitBtn
    Left = 230
    Top = 252
    Width = 31
    Height = 31
    Hint = 'Show DxGetText manual'
    Anchors = [akRight, akBottom]
    Glyph.Data = {
      96090000424D9609000000000000360000002800000028000000140000000100
      18000000000060090000120B0000120B00000000000000000000C0DCC0C0DCC0
      C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DC
      C0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0
      DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0
      C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DC
      C0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0
      DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0
      C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DC
      C0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0
      DCC0C0DCC0010142010142C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0
      C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DC
      C0404040404040C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0
      DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0010142010142808080
      070788010142C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DC
      C0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0404040404040BEBEBE7D7D7D40
      4040C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0
      C0DCC0C0DCC0C0DCC0010142010142808080C0C0C0FFFFFFC0C0C00707880101
      42C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0
      DCC0C0DCC0404040404040BEBEBEEEEEEEFFFFFFEEEEEE7D7D7D404040C0DCC0
      C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC00101420101
      42808080C0C0C0FFFFFFFFFFFF808080C0C0C0C0C0C0070788010142C0DCC0C0
      DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0404040404040BEBEBE
      EEEEEEFFFFFFFFFFFFBEBEBEEEEEEEEEEEEE7D7D7D404040C0DCC0C0DCC0C0DC
      C0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0010142808080C0C0C0FFFFFFFFFFFF80
      8080808080010142808080C0C0C0C0C0C0070788010142C0DCC0C0DCC0C0DCC0
      C0DCC0C0DCC0C0DCC0C0DCC0404040BEBEBEEEEEEEFFFFFFFFFFFFBEBEBEBEBE
      BE404040BEBEBEEEEEEEEEEEEE7D7D7D404040C0DCC0C0DCC0C0DCC0C0DCC0C0
      DCC0C0DCC0C0DCC0010142808080FFFFFF8080808080800101420101420B0BDF
      010142808080C0C0C0C0C0C0070788010142C0DCC0C0DCC0C0DCC0C0DCC0C0DC
      C0C0DCC0404040BEBEBEFFFFFFBEBEBEBEBEBE4040404040407D7D7D404040BE
      BEBEEEEEEEEEEEEE7D7D7D404040C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0
      0101428080808080800101420101420B0BDF0B0BDF0B0BDF0B0BDF0101428080
      80C0C0C0C0C0C0070788010142C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0404040BE
      BEBEBEBEBE4040404040407D7D7D7D7D7D7D7D7D7D7D7D404040BEBEBEEEEEEE
      EEEEEE7D7D7D404040C0DCC0C0DCC0C0DCC0C0DCC0C0DCC00101420101420101
      420B0BDF0B0BDF0B0BDF0B0BDF0B0BDF0B0BDF0B0BDF010142808080C0C0C0C0
      C0C0070788010142C0DCC0C0DCC0C0DCC0C0DCC04040404040404040407D7D7D
      7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D404040BEBEBEEEEEEEEEEEEE7D7D
      7D404040C0DCC0C0DCC0C0DCC0C0DCC00101428080800B0BDF0B0BDF0B0BDF0B
      0BDF00FFFF0080800B0BDF0B0BDF0B0BDF010142808080C0C0C0010142010142
      C0DCC0C0DCC0C0DCC0C0DCC0404040BEBEBE7D7D7D7D7D7D7D7D7D7D7D7DBEBE
      BE6F6F6F7D7D7D7D7D7D7D7D7D404040BEBEBEEEEEEE404040404040C0DCC0C0
      DCC0C0DCC0C0DCC0C0DCC00101428080800B0BDF0B0BDF0B0BDF0B0BDFC0C0C0
      00FFFF00FFFF0B0BDF0B0BDF010142808080010142C0DCC0C0DCC0C0DCC0C0DC
      C0C0DCC0C0DCC0404040BEBEBE7D7D7D7D7D7D7D7D7D7D7D7DEEEEEEBEBEBEBE
      BEBE7D7D7D7D7D7D404040BEBEBE404040C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0
      C0DCC0C0DCC00101428080800B0BDF0B0BDF00808000808007078800FFFF00FF
      FF0B0BDF0B0BDF010142010142C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0
      DCC0404040BEBEBE7D7D7D7D7D7D6F6F6F6F6F6F7D7D7DBEBEBEBEBEBE7D7D7D
      7D7D7D404040404040C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DC
      C00101428080800B0BDF0B0BDF00FFFF00FFFF00FFFF0080800B0BDF0B0BDF0B
      0BDF010142C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0404040
      BEBEBE7D7D7D7D7D7DBEBEBEBEBEBEBEBEBE6F6F6F7D7D7D7D7D7D7D7D7D4040
      40C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC001014280
      80800B0BDF0B0BDF0B0BDF0B0BDF0B0BDF0B0BDF0B0BDF010142010142C0DCC0
      C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0404040BEBEBE7D7D
      7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D404040404040C0DCC0C0DCC0C0
      DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC00101428080800B0BDF
      0B0BDF0B0BDF0B0BDF010142010142C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DC
      C0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0404040BEBEBE7D7D7D7D7D7D7D
      7D7D7D7D7D404040404040C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0
      C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC00101428080800B0BDF0101420101
      42C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0
      DCC0C0DCC0C0DCC0C0DCC0C0DCC0404040BEBEBE7D7D7D404040404040C0DCC0
      C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DC
      C0C0DCC0C0DCC0C0DCC0C0DCC0010142010142C0DCC0C0DCC0C0DCC0C0DCC0C0
      DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0
      C0DCC0C0DCC0C0DCC0404040404040C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DC
      C0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0
      DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0
      C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DC
      C0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0
      DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0
      C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DC
      C0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0
      DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0C0DCC0}
    NumGlyphs = 2
    ParentShowHint = False
    ShowHint = True
    TabOrder = 6
    OnClick = btnManualClick
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'exe'
    Filter = 'exe files|*.exe'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofNoReadOnlyReturn, ofEnableSizing]
    Left = 100
    Top = 215
  end
end
