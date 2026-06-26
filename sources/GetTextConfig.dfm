object frmConfig: TfrmConfig
  Left = 301
  Top = 198
  ActiveControl = EditBasepath
  BorderStyle = bsDialog
  Caption = 'Configuration for template creation'
  ClientHeight = 456
  ClientWidth = 396
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnAfterMonitorDpiChanged = FormAfterMonitorDpiChanged
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    396
    456)
  PixelsPerInch = 96
  TextHeight = 13
  object laVersion: TLabel
    Left = 10
    Top = 432
    Width = 3
    Height = 13
    Anchors = [akLeft, akBottom]
  end
  object bbBaseDir: TJrSpeedButton
    Left = 360
    Top = 15
    Width = 31
    Height = 31
    Anchors = [akLeft, akRight]
    Flat = True
    Images = imlGlyphs
    ImageIndex = 4
    Layout = blGlyphLeft
    OnClick = bbBaseDirClick
  end
  object bbExclude: TJrSpeedButton
    Left = 360
    Top = 80
    Width = 31
    Height = 31
    Anchors = [akLeft, akRight]
    Flat = True
    Images = imlGlyphs
    ImageIndex = 4
    Layout = blGlyphLeft
    OnClick = bbExcludeClick
  end
  object btDefMask: TJrSpeedButton
    Left = 360
    Top = 125
    Width = 26
    Height = 31
    Flat = True
    Images = imlGlyphs
    ImageIndex = 5
    Layout = blGlyphLeft
    OnClick = btDefMaskClick
  end
  object laProg: TLabel
    Left = 10
    Top = 417
    Width = 3
    Height = 13
    Anchors = [akLeft, akBottom]
  end
  object EditMask: TLabeledEdit
    Left = 10
    Top = 130
    Width = 346
    Height = 21
    Hint = 'Here you can specify which files you want to scan.'
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 52
    EditLabel.Height = 13
    EditLabel.Caption = 'File masks:'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
    Text = '*.pas *.dfm *.c *.cpp *.inc *.rc *.dfm *.xfm *.dpr'
  end
  object CheckBoxRecurse: TCheckBox
    Left = 10
    Top = 48
    Width = 337
    Height = 17
    Caption = 'Also search subdirectories'
    Checked = True
    State = cbChecked
    TabOrder = 1
    OnClick = CheckBoxRecurseClick
  end
  object EditBasepath: TLabeledEdit
    Left = 10
    Top = 20
    Width = 346
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 74
    EditLabel.Height = 13
    EditLabel.Caption = 'Basic directory:'
    TabOrder = 0
  end
  object CheckBoxSaveSettings: TCheckBox
    Left = 10
    Top = 390
    Width = 376
    Height = 17
    Hint = 'Save settings as ini file in this basic directory'
    Caption = 'Remember settings'
    Checked = True
    ParentShowHint = False
    ShowHint = True
    State = cbChecked
    TabOrder = 10
  end
  object cbCreateIgnore: TCheckBox
    Left = 10
    Top = 290
    Width = 376
    Height = 17
    Hint = 
      'Create a new ignore.po file or add likely ignores to existing ig' +
      'nore.po file (default domain)'
    Caption = 'Add likely ignores to ignore.po file'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 6
  end
  object cbRemoveIgnore: TCheckBox
    Left = 10
    Top = 315
    Width = 376
    Height = 17
    Hint = 'Don'#39't store items to the template that are present in ignore.po'
    Caption = 'Remove items from template present in ignore.po file'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 7
  end
  object gbDomain: TGroupBox
    Left = 10
    Top = 160
    Width = 381
    Height = 66
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Text domain'
    TabOrder = 4
    DesignSize = (
      381
      66)
    object rbDefault: TRadioButton
      Left = 15
      Top = 20
      Width = 81
      Height = 17
      Caption = 'default'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = rbDefaultClick
    end
    object rbOther: TRadioButton
      Left = 15
      Top = 40
      Width = 81
      Height = 17
      Caption = 'other'
      TabOrder = 1
      OnClick = rbDefaultClick
    end
    object OutputName: TLabeledEdit
      Left = 110
      Top = 35
      Width = 251
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      EditLabel.Width = 31
      EditLabel.Height = 13
      EditLabel.Caption = 'Name:'
      TabOrder = 2
    end
  end
  object ExcludeDirs: TLabeledEdit
    Left = 10
    Top = 85
    Width = 346
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 207
    EditLabel.Height = 13
    EditLabel.Caption = 'Exclude subdirectories (comma separated):'
    TabOrder = 2
  end
  object cbOverwrite: TCheckBox
    Left = 10
    Top = 365
    Width = 376
    Height = 17
    Caption = 'Always overwrite template file'
    TabOrder = 9
  end
  object cbOrder: TCheckBox
    Left = 10
    Top = 340
    Width = 376
    Height = 17
    Caption = 'Sort entries by original strings (msgid)'
    TabOrder = 8
  end
  object rgEncoding: TRadioGroup
    Left = 10
    Top = 230
    Width = 376
    Height = 51
    Caption = 'Encoding of source files:'
    Columns = 3
    ItemIndex = 0
    Items.Strings = (
      'US-ASCII'
      'ISO-8859-1'
      'UTF-8')
    TabOrder = 5
  end
  object ButtonOK: TJrButton
    Left = 215
    Top = 415
    Width = 81
    Height = 36
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    Images = imlGlyphs
    ImageIndex = 0
    Layout = blGlyphLeft
    ModalResult = 1
    ParentDoubleBuffered = True
    TabOrder = 11
  end
  object ButtonCancel: TJrButton
    Left = 300
    Top = 415
    Width = 91
    Height = 36
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    Images = imlGlyphs
    ImageIndex = 1
    Layout = blGlyphLeft
    ModalResult = 2
    ParentDoubleBuffered = True
    TabOrder = 12
  end
  object btnHelp: TJrButton
    Left = 175
    Top = 415
    Width = 36
    Height = 36
    Hint = 'Show program help'
    Anchors = [akRight, akBottom]
    Images = imlGlyphs
    ImageIndex = 2
    Layout = blGlyphLeft
    ParentShowHint = False
    ShowHint = True
    TabOrder = 13
    OnClick = btnHelpClick
  end
  object btnManual: TJrButton
    Left = 135
    Top = 415
    Width = 36
    Height = 36
    Hint = 'Show DxGetText manual'
    Anchors = [akRight, akBottom]
    Images = imlGlyphs
    ImageIndex = 3
    Layout = blGlyphLeft
    ParentShowHint = False
    ShowHint = True
    TabOrder = 14
    OnClick = btnManualClick
  end
  object FileOpenDialog: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <>
    Options = [fdoPickFolders, fdoForceFileSystem]
    Left = 305
    Top = 350
  end
  object pmMask: TPopupMenu
    AutoHotkeys = maManual
    OwnerDraw = True
    Left = 245
    Top = 350
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
        IconName = 'help-blue'
        SVGText = 
          '<svg height="48" width="48" xmlns="http://www.w3.org/2000/svg" x' +
          'mlns:xlink="http://www.w3.org/1999/xlink"><linearGradient id="a"' +
          ' gradientTransform="matrix(.254 0 0 3.759813 .788629 .148567)" g' +
          'radientUnits="userSpaceOnUse" x1="35.433035" x2="41.219128" y1="' +
          '4.953005" y2="4.953005"><stop offset="0" stop-opacity=".068627"/' +
          '><stop offset="1" stop-color="#e6e6e6"/></linearGradient><linear' +
          'Gradient id="b" gradientTransform="matrix(2.262742 0 0 .441942 1' +
          ' -.875)" gradientUnits="userSpaceOnUse" x1="10.496115" x2="10.21' +
          '9901" y1="93.338043" y2="84.287079"><stop offset="0" stop-color=' +
          '"#e6e6e6"/><stop offset="1" stop-color="#b7b7b7"/></linearGradie' +
          'nt><linearGradient id="c" gradientTransform="matrix(.95775 0 0 1' +
          '.027989 1 -.571911)" gradientUnits="userSpaceOnUse" x1="6.587181' +
          '" x2="14.511404" y1="22.132999" y2="22.132999"><stop offset="0" ' +
          'stop-color="#9fbad5"/><stop offset="1" stop-color="#6794c5"/></l' +
          'inearGradient><linearGradient id="d" gradientTransform="matrix(1' +
          '.025428 0 0 .957303 0 -.806758)" gradientUnits="userSpaceOnUse" ' +
          'x1="73.361984" x2="-2.75829" y1="26.652197" y2="21.270376"><stop' +
          ' offset="0" stop-color="#e6e6e6"/><stop offset="1" stop-color="#' +
          'e6e6e6" stop-opacity="0"/></linearGradient><g transform="transla' +
          'te(.485354 .93121)"><path d="m6.3643222 5.5185897c.0907827-1.914' +
          '9894 1.0076536-2.9643083 2.7145562-2.9695457l29.3268976-.0899863' +
          'c.246585-.0007566.568541.2001494.593236.4499311l3.258479 32.9582' +
          '392-1.315302.05663.62924 6.445654c.061012.624987-.18137 1.159308' +
          '-1.071429 1.163523l-30.7106954.145439c-2.5325446.011994-5.135459' +
          '2-2.078714-5.0133709-4.654071z" fill="#526085" stroke="#31416c" ' +
          'stroke-linecap="round" stroke-linejoin="round"/><path d="m40.125' +
          ' 34.875-29.1875.125c-1.5565181.177868-2.8125 1.39612-2.8125 3s1.' +
          '2559819 2.822132 2.8125 3l29.1875.125v-.0625c-1.655622-.07815-3-' +
          '1.387649-3-3.0625s1.344378-2.984348 3-3.0625z" fill="url(#b)"/><' +
          'path d="m9.6875 2.8125c-1.7069103 0-2.637397 1.0090062-2.71875 2' +
          '.8613658l-1.65625 32.1519062c-.09196 3.078427 1.8268732 4.828713' +
          ' 3.8125 5.330478-4.25-1.630671-3.6875-8.991795 1.625-8.961028h30' +
          '.898286l-3.3125-30.9519788c-.025761-.2407128-.347908-.4307432-.5' +
          '9375-.4307432z" fill="url(#c)"/><path d="m9.788627 3.968539h2v29' +
          '.604792h-2z" fill="url(#a)" opacity=".480447" transform="matrix(' +
          '1 0 -.03582731 .999358 0 0)"/><path d="m9.8751008 3.3336831c-1.6' +
          '838994 0-2.3366772.7321628-2.4169335 2.5541479l-1.298904 29.8893' +
          '67c.9333283-1.606747 2.4395958-2.182761 4.8524017-2.182761h29.95' +
          '1416l-2.825902-29.8370739c-.02245-.2370539-.343218-.42368-.58574' +
          '5-.42368z" fill="none" stroke="url(#d)" stroke-linecap="round" s' +
          'troke-miterlimit="20"/></g><text style="font-weight:bold;font-si' +
          'ze:24;font-family:Arial;letter-spacing:0;word-spacing:0;fill:#ff' +
          'f" x="15.556491" y="27.625151"><tspan fill="#fff" x="15.556491" ' +
          'y="27.625151">?</tspan></text></svg>'
      end
      item
        IconName = 'help-red'
        SVGText = 
          '<svg height="48" width="48" xmlns="http://www.w3.org/2000/svg" x' +
          'mlns:xlink="http://www.w3.org/1999/xlink"><linearGradient id="a"' +
          ' gradientTransform="matrix(.254 0 0 3.759813 .788629 .148567)" g' +
          'radientUnits="userSpaceOnUse" x1="35.433035" x2="41.219128" y1="' +
          '4.953005" y2="4.953005"><stop offset="0" stop-opacity=".068627"/' +
          '><stop offset="1" stop-color="#e6e6e6"/></linearGradient><linear' +
          'Gradient id="b" gradientTransform="matrix(2.262742 0 0 .441942 1' +
          ' -.875)" gradientUnits="userSpaceOnUse" x1="10.496115" x2="10.21' +
          '9901" y1="93.338043" y2="84.287079"><stop offset="0" stop-color=' +
          '"#e6e6e6"/><stop offset="1" stop-color="#b7b7b7"/></linearGradie' +
          'nt><linearGradient id="c" gradientTransform="matrix(.95775 0 0 1' +
          '.027989 1 -.571911)" gradientUnits="userSpaceOnUse" x1="6.587181' +
          '" x2="14.511404" y1="22.132999" y2="22.132999"><stop offset="0" ' +
          'stop-color="#d59fa1"/><stop offset="1" stop-color="#c56867"/></l' +
          'inearGradient><linearGradient id="d" gradientTransform="matrix(1' +
          '.025428 0 0 .957303 0 -.806758)" gradientUnits="userSpaceOnUse" ' +
          'x1="73.361984" x2="-2.75829" y1="26.652197" y2="21.270376"><stop' +
          ' offset="0" stop-color="#e6e6e6"/><stop offset="1" stop-color="#' +
          'e6e6e6" stop-opacity="0"/></linearGradient><g transform="transla' +
          'te(.485354 .93121)"><path d="m6.3643222 5.5185897c.0907827-1.914' +
          '9894 1.0076536-2.9643083 2.7145562-2.9695457l29.3268976-.0899863' +
          'c.246585-.0007566.568541.2001494.593236.4499311l3.258479 32.9582' +
          '392-1.315302.05663.62924 6.445654c.061012.624987-.18137 1.159308' +
          '-1.071429 1.163523l-30.7106954.145439c-2.5325446.011994-5.135459' +
          '2-2.078714-5.0133709-4.654071z" fill="#855d52" stroke="#6c3231" ' +
          'stroke-linecap="round" stroke-linejoin="round"/><path d="m40.125' +
          ' 34.875-29.1875.125c-1.5565181.177868-2.8125 1.39612-2.8125 3s1.' +
          '2559819 2.822132 2.8125 3l29.1875.125v-.0625c-1.655622-.07815-3-' +
          '1.387649-3-3.0625s1.344378-2.984348 3-3.0625z" fill="url(#b)"/><' +
          'path d="m9.6875 2.8125c-1.7069103 0-2.637397 1.0090062-2.71875 2' +
          '.8613658l-1.65625 32.1519062c-.09196 3.078427 1.8268732 4.828713' +
          ' 3.8125 5.330478-4.25-1.630671-3.6875-8.991795 1.625-8.961028h30' +
          '.898286l-3.3125-30.9519788c-.025761-.2407128-.347908-.4307432-.5' +
          '9375-.4307432z" fill="url(#c)"/><path d="m9.788627 3.968539h2v29' +
          '.604792h-2z" fill="url(#a)" opacity=".480447" transform="matrix(' +
          '1 0 -.03582731 .999358 0 0)"/><path d="m9.8751008 3.3336831c-1.6' +
          '838994 0-2.3366772.7321628-2.4169335 2.5541479l-1.298904 29.8893' +
          '67c.9333283-1.606747 2.4395958-2.182761 4.8524017-2.182761h29.95' +
          '1416l-2.825902-29.8370739c-.02245-.2370539-.343218-.42368-.58574' +
          '5-.42368z" fill="none" stroke="url(#d)" stroke-linecap="round" s' +
          'troke-miterlimit="20"/></g><text style="font-weight:bold;font-si' +
          'ze:24;font-family:Arial;letter-spacing:0;word-spacing:0;fill:#ff' +
          'f" x="15.556491" y="27.625151"><tspan fill="#fff" x="15.556491" ' +
          'y="27.625151">?</tspan></text></svg>'
      end
      item
        IconName = 'dir-tree'
        SVGText = 
          '<svg height="48" width="48" xmlns="http://www.w3.org/2000/svg" x' +
          'mlns:xlink="http://www.w3.org/1999/xlink"><linearGradient id="a"' +
          ' gradientUnits="userSpaceOnUse" x1="24.511713" x2="24.511713" y1' +
          '="5.873232" y2="16.560732"><stop offset="0" stop-color="#e4a05d"' +
          '/><stop offset="1" stop-color="#af651d"/></linearGradient><linea' +
          'rGradient id="b" gradientUnits="userSpaceOnUse" x1="24.588383" x' +
          '2="22.435516" y1="39.066536" y2="15.191534"><stop offset="0" sto' +
          'p-color="#e3aa57"/><stop offset="1" stop-color="#eabd7c"/></line' +
          'arGradient><g fill="#2e89e4" stroke="#3c68af"><path d="m7 13v32a' +
          '1 1 45 0 0 1 1h2a1 1 135 0 0 1-1v-3a1 1 135 0 1 1-1h9a1 1 135 0 ' +
          '0 1-1v-2a1 1 45 0 0 -1-1h-9a1 1 45 0 1 -1-1v-9a1 1 135 0 1 1-1h9' +
          'a1 1 135 0 0 1-1v-2a1 1 45 0 0 -1-1h-9a1 1 45 0 1 -1-1v-8a1 1 45' +
          ' 0 0 -1-1h-2a1 1 135 0 0 -1 1z" fill-rule="evenodd"/><rect heigh' +
          't="3.846154" ry=".487116" width="3.846154" x="35" y="22.076923"/' +
          '><rect height="3.846154" ry=".487116" width="3.846154" x="42.153' +
          '847" y="22.076923"/><rect height="3.846154" ry=".487116" width="' +
          '3.846154" x="35.076923" y="37.153847"/><rect height="3.846154" r' +
          'y=".487116" width="3.846154" x="42.23077" y="37.153847"/></g><g ' +
          'fill-rule="evenodd" stroke="#b05f1e"><g transform="matrix(.30739' +
          '183 0 0 .30739183 19.0083 15.284522)"><path d="m5.4186638 5.4561' +
          '1c-.4649766 0-.8151104.2352268-.8151104.726723v33.020961c0 .7271' +
          '46.487105 1.21156 1.149057 1.21156h37.7369536c.665802 0 .930308-' +
          '.452766.930308-1.08656v-28.902882c0-.5429651-.400287-.836558-.89' +
          '9058-.836558h-19.109324c-.337013 0-.69668-.198426-.894481-.47128' +
          '7l-2.602231-3.200009c-.217865-.300539-.642075-.461933-1.013278-.' +
          '461933z" fill="url(#a)"/><path d="m7.6864537 16.483782h7.9333453' +
          'c.741734 0 1.36797-.380811 1.687167-.882004.136743-.214709 1.267' +
          '982-2.188228 1.357674-2.318726.298787-.434724.772917-.758916 1.3' +
          '41982-.758916h23.042233c.802837 0 1.449163.643789 1.449163 1.443' +
          '474v24.947849c0 .799684-.646326 1.443473-1.449163 1.443473h-36.9' +
          '209466c-.8028359 0-1.4491623-.643789-1.4491623-1.443473v-19.4335' +
          '55c0-1.974365 1.1403162-2.998122 3.0077076-2.998122z" fill="url(' +
          '#b)" stroke-linejoin="round"/></g><g transform="matrix(.30739183' +
          ' 0 0 .30739183 1.909857 1.434862)"><path d="m5.4186638 5.45611c-' +
          '.4649766 0-.8151104.2352268-.8151104.726723v33.020961c0 .727146.' +
          '487105 1.21156 1.149057 1.21156h37.7369536c.665802 0 .930308-.45' +
          '2766.930308-1.08656v-28.902882c0-.5429651-.400287-.836558-.89905' +
          '8-.836558h-19.109324c-.337013 0-.69668-.198426-.894481-.471287l-' +
          '2.602231-3.200009c-.217865-.300539-.642075-.461933-1.013278-.461' +
          '933z" fill="url(#a)"/><path d="m7.6864537 16.483782h7.9333453c.7' +
          '41734 0 1.36797-.380811 1.687167-.882004.136743-.214709 1.267982' +
          '-2.188228 1.357674-2.318726.298787-.434724.772917-.758916 1.3419' +
          '82-.758916h23.042233c.802837 0 1.449163.643789 1.449163 1.443474' +
          'v24.947849c0 .799684-.646326 1.443473-1.449163 1.443473h-36.9209' +
          '466c-.8028359 0-1.4491623-.643789-1.4491623-1.443473v-19.433555c' +
          '0-1.974365 1.1403162-2.998122 3.0077076-2.998122z" fill="url(#b)' +
          '" stroke-linejoin="round"/></g><g transform="matrix(.30739183 0 ' +
          '0 .30739183 19.0083 30.150183)"><path d="m5.4186638 5.45611c-.46' +
          '49766 0-.8151104.2352268-.8151104.726723v33.020961c0 .727146.487' +
          '105 1.21156 1.149057 1.21156h37.7369536c.665802 0 .930308-.45276' +
          '6.930308-1.08656v-28.902882c0-.5429651-.400287-.836558-.899058-.' +
          '836558h-19.109324c-.337013 0-.69668-.198426-.894481-.471287l-2.6' +
          '02231-3.200009c-.217865-.300539-.642075-.461933-1.013278-.461933' +
          'z" fill="url(#a)"/><path d="m7.6864537 16.483782h7.9333453c.7417' +
          '34 0 1.36797-.380811 1.687167-.882004.136743-.214709 1.267982-2.' +
          '188228 1.357674-2.318726.298787-.434724.772917-.758916 1.341982-' +
          '.758916h23.042233c.802837 0 1.449163.643789 1.449163 1.443474v24' +
          '.947849c0 .799684-.646326 1.443473-1.449163 1.443473h-36.9209466' +
          'c-.8028359 0-1.4491623-.643789-1.4491623-1.443473v-19.433555c0-1' +
          '.974365 1.1403162-2.998122 3.0077076-2.998122z" fill="url(#b)" s' +
          'troke-linejoin="round"/></g></g></svg>'
      end
      item
        IconName = 'chevron-down'
        SVGText = 
          '<svg height="48" viewBox="0 0 48 48" width="48" xmlns="http://ww' +
          'w.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink"><l' +
          'inearGradient id="a" gradientUnits="userSpaceOnUse" x1="5.573668' +
          '" x2="34.058079" y1="10" y2="11.165337"><stop offset="0" stop-co' +
          'lor="#99bfec" stop-opacity=".99537"/><stop offset="1" stop-color' +
          '="#2574ca"/></linearGradient><path d="m9.5128104 24.296521-4.302' +
          '6195-3.820939 16.2633411-10.136733-13.6249615-12.0996396 5.13579' +
          '15-3.2010733 17.927581 15.9205789z" style="stroke:#2a6cbf;stroke' +
          '-width:1.91213;stroke-linecap:round;stroke-linejoin:round;stroke' +
          '-dashoffset:1.33;fill:url(#a)" transform="matrix(.07874668088 .8' +
          '9206451236 -1.19903978969 .10584481434 34.70572286224 9.04373648' +
          '306)"/></svg>'
      end>
    Left = 345
    Top = 240
  end
end
