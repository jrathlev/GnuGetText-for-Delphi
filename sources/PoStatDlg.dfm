object PoStatDialog: TPoStatDialog
  Left = 0
  Top = 0
  Caption = 'Show statistics of po file'
  ClientHeight = 471
  ClientWidth = 441
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnAfterMonitorDpiChanged = FormAfterMonitorDpiChanged
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  DesignSize = (
    441
    471)
  PixelsPerInch = 96
  TextHeight = 13
  object Label6: TLabel
    Left = 5
    Top = 30
    Width = 39
    Height = 13
    Caption = 'Header:'
  end
  object laPoFile: TLabel
    Left = 5
    Top = 10
    Width = 21
    Height = 13
    Caption = 'xxx'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
  end
  object gbStat: TGroupBox
    Left = 5
    Top = 290
    Width = 431
    Height = 136
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Statistics'
    TabOrder = 1
    object Label1: TLabel
      Left = 15
      Top = 20
      Width = 90
      Height = 13
      Caption = 'Number of entries:'
    end
    object Label3: TLabel
      Left = 15
      Top = 40
      Width = 55
      Height = 13
      Caption = 'Translated:'
    end
    object Label4: TLabel
      Left = 15
      Top = 60
      Width = 73
      Height = 13
      Caption = 'Not translated:'
    end
    object Label5: TLabel
      Left = 15
      Top = 80
      Width = 74
      Height = 13
      Caption = 'Needs revision:'
    end
    object Label7: TLabel
      Left = 15
      Top = 100
      Width = 108
      Height = 13
      Caption = 'Number of characters:'
    end
    object laEntries: TLabel
      Left = 160
      Top = 20
      Width = 18
      Height = 13
      Caption = 'xxx'
    end
    object laNumTrans: TLabel
      Left = 160
      Top = 40
      Width = 18
      Height = 13
      Caption = 'xxx'
    end
    object laNumNoTrans: TLabel
      Left = 160
      Top = 60
      Width = 18
      Height = 13
      Caption = 'xxx'
    end
    object laNumFuzzy: TLabel
      Left = 160
      Top = 80
      Width = 18
      Height = 13
      Caption = 'xxx'
    end
    object laNumChars: TLabel
      Left = 160
      Top = 100
      Width = 18
      Height = 13
      Caption = 'xxx'
    end
  end
  object lvHeader: TListView
    Left = 5
    Top = 45
    Width = 431
    Height = 236
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Caption = 'ID'
        Width = 180
      end
      item
        Caption = 'Value'
        Width = 150
      end>
    TabOrder = 2
    ViewStyle = vsReport
  end
  object bbExit: TJrButton
    Left = 330
    Top = 430
    Width = 106
    Height = 36
    Hint = 'Quit program'
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    Default = True
    Images = imlGlyphs
    ImageIndex = 2
    Layout = blGlyphLeft
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    OnClick = bbExitClick
  end
  object OpenDialog: TOpenDialog
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 310
    Top = 310
  end
  object imlGlyphs: TSVGIconImageList
    Size = 24
    SVGIconItems = <
      item
        IconName = 'ok'
        SVGText = 
          '<svg height="48" width="48" xmlns="http://www.w3.org/2000/svg" x' +
          'mlns:xlink="http://www.w3.org/1999/xlink"><linearGradient id="a"' +
          ' gradientUnits="userSpaceOnUse" x1="-74.093187" x2="-95.970514" ' +
          'y1="-8.208767" y2="27.92338"><stop offset="0" stop-color="#6cfb9' +
          'c"/><stop offset="1" stop-color="#098533"/></linearGradient><g s' +
          'troke-linecap="round" stroke-linejoin="round" stroke-width=".955' +
          '976" transform="matrix(1.017301 -.243559 .243559 1.017301 109.99' +
          '408 -7.803349)"><path d="m-73.881889-4.6293179c-.507742-.177402-' +
          '1.08613-.046397-1.470652.378901l-16.382247 18.1161269-5.929986-7' +
          '.5772879c-.567064-.512696-1.435963-.478237-1.948662.088827l-4.29' +
          '4114 3.430523c-.51269.5670599-.46769 1.4382239.09937 1.9509179 0' +
          ' 0 10.728838 14.246903 10.743382 14.259026.132903.120165.282743.' +
          '200489.440367.259942.514909.194217 1.118022.074848 1.510555-.359' +
          '307l22.093584-24.4318239c.512694-.56706195.467698-1.43822394-.09' +
          '9368-1.95091993l-4.28196-3.88539107c-.141768-.128175-.311022-.22' +
          '0401-.480269-.279534z" fill="url(#a)" stroke="#00802a"/><path d=' +
          '"m-74.978545-3.2016699-8.310802 9.222897-7.383516 8.1220699c-.20' +
          '6245.128638-.28299.673517-1.051361.685046-.531614.007977-.577618' +
          '-.200117-1.024273-.665865l-5.23947-6.6531665c-.580564-.75009-.56' +
          '5862-.7148191-1.197772-.3235054l-3.378551 2.652702c-.87807.58076' +
          '19-.87802 1.0586829-.14038 1.8252769 0 0 9.759296 12.933261 9.77' +
          '3051 12.944731.125708.113655.057519.125975.5759.706188.332525.37' +
          '2187.895003-.422683 1.263883-.830659l21.241855-23.4364379c.48180' +
          '1-.53286893.454382-.53069193-.08199-1.01561494l-3.848961-3.39320' +
          '706c-.55318-.497556-.55744-.494409-1.197613.159545z" fill="none"' +
          ' opacity=".4" stroke="#fff" stroke-opacity=".5"/></g></svg>'
      end
      item
        IconName = 'cancel'
        SVGText = 
          '<svg height="48" width="48" xmlns="http://www.w3.org/2000/svg" x' +
          'mlns:xlink="http://www.w3.org/1999/xlink"><linearGradient id="a"' +
          ' gradientUnits="userSpaceOnUse" x1="27.808342" x2="18.608994" y1' +
          '="43.595886" y2="-.483242"><stop offset="0" stop-color="#e01019"' +
          '/><stop offset="1" stop-color="#fc9ca4"/></linearGradient><g str' +
          'oke-linejoin="round"><path d="m42.5 35.000459-10.50009-11 10.500' +
          '001-10-7.500001-7.4999988-10.499999 10.4999988-10.499999-10.4999' +
          '988-7.4999987 7.4999988 10.4999977 10-10.4999977 11 7.4999987 7.' +
          '499999 10.499999-10.5 9.5 10.5z" fill="url(#a)" stroke="#c7000a"' +
          '/><path d="m41 35.000459-10.50009-11 10.5-10-6-5.9999998-10.4996' +
          '9 10.4999998-10.500308-10.4999998-5.9999997 5.9999998 10.4999987' +
          ' 10-10.4999987 11 5.9999997 6 10.499999-10.5 9.500089 10.499541z' +
          '" fill="none" opacity=".499404" stroke="#fff"/></g></svg>'
      end
      item
        IconName = 'close'
        SVGText = 
          '<svg height="48" width="48" xmlns="http://www.w3.org/2000/svg" x' +
          'mlns:xlink="http://www.w3.org/1999/xlink"><linearGradient id="a"' +
          ' gradientUnits="userSpaceOnUse" x1="27.808342" x2="18.608994" y1' +
          '="43.595886" y2="-.483242"><stop offset="0" stop-color="#098633"' +
          '/><stop offset="1" stop-color="#8efcb3"/></linearGradient><g str' +
          'oke-linejoin="round"><path d="m42.5 35.000459-10.50009-11 10.500' +
          '001-10-7.500001-7.4999988-10.499999 10.4999988-10.499999-10.4999' +
          '988-7.4999987 7.4999988 10.4999977 10-10.4999977 11 7.4999987 7.' +
          '499999 10.499999-10.5 9.5 10.5z" fill="url(#a)" stroke="#00822b"' +
          '/><path d="m41 35.000459-10.50009-11 10.5-10-6-5.9999998-10.4996' +
          '9 10.4999998-10.500308-10.4999998-5.9999997 5.9999998 10.4999987' +
          ' 10-10.4999987 11 5.9999997 6 10.499999-10.5 9.500089 10.499541z' +
          '" fill="none" opacity=".499404" stroke="#fff"/></g></svg>'
      end>
    Left = 235
    Top = 430
  end
end
