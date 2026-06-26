object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Spell checking of po translations'
  ClientHeight = 328
  ClientWidth = 496
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  OnAfterMonitorDpiChanged = FormAfterMonitorDpiChanged
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object paBottom: TPanel
    Left = 0
    Top = 191
    Width = 496
    Height = 137
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      496
      137)
    object btGetSuggestion: TJrSpeedButton
      Left = 340
      Top = 35
      Width = 31
      Height = 26
      Hint = 'Show suggestions'
      Anchors = [akTop, akRight]
      Images = imlGlyphs
      ImageIndex = 6
      Layout = blGlyphLeft
      ParentShowHint = False
      ShowHint = True
      OnClick = btGetSuggestionClick
    end
    object Label1: TLabel
      Left = 10
      Top = 20
      Width = 133
      Height = 13
      Caption = 'Mispelled or unknown word:'
    end
    object laEntry: TLabel
      Left = 10
      Top = 5
      Width = 3
      Height = 13
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object bbExit: TJrButton
      Left = 455
      Top = 96
      Width = 36
      Height = 36
      Hint = 'Quit program'
      Anchors = [akTop, akRight]
      Images = imlGlyphs
      ImageIndex = 3
      Layout = blGlyphLeft
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
      OnClick = bbExitClick
    end
    object bbInfo: TJrButton
      Left = 415
      Top = 96
      Width = 36
      Height = 36
      Hint = 'About the program'
      Anchors = [akTop, akRight]
      Images = imlGlyphs
      ImageIndex = 2
      Layout = blGlyphLeft
      ParentShowHint = False
      ShowHint = True
      TabOrder = 7
      OnClick = bbInfoClick
    end
    object bbReload: TJrButton
      Left = 375
      Top = 6
      Width = 116
      Height = 41
      Hint = 'Reload po file'
      Anchors = [akTop, akRight]
      Caption = 'Reload'
      Images = imlGlyphs
      ImageIndex = 4
      Layout = blGlyphLeft
      ParentShowHint = False
      ShowHint = True
      TabOrder = 6
      OnClick = bbReloadClick
    end
    object bbSave: TJrButton
      Left = 375
      Top = 51
      Width = 116
      Height = 41
      Hint = 'Save po file'
      Anchors = [akTop, akRight]
      Caption = 'Save'
      Images = imlGlyphs
      ImageIndex = 5
      Layout = blGlyphLeft
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
      OnClick = bbSaveClick
    end
    object btAdd: TButton
      Left = 10
      Top = 71
      Width = 361
      Height = 26
      Hint = 'Add word to dictionary'
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Add to dictionary and goto next'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnClick = btAddClick
    end
    object btNext: TButton
      Left = 250
      Top = 106
      Width = 121
      Height = 26
      Hint = 'Find next misspelled word'
      Anchors = [akTop, akRight]
      Caption = 'Next word'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnClick = btNextClick
    end
    object btnHelp: TJrButton
      Left = 375
      Top = 96
      Width = 36
      Height = 36
      Hint = 'Show program help'
      Anchors = [akTop, akRight]
      Images = imlGlyphs
      ImageIndex = 1
      Layout = blGlyphLeft
      ParentShowHint = False
      ShowHint = True
      TabOrder = 8
      OnClick = btnHelpClick
    end
    object btReplace: TButton
      Left = 10
      Top = 106
      Width = 236
      Height = 26
      Hint = 'Replace this word'
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Replace word'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = btReplaceClick
    end
    object edWord: TEdit
      Left = 10
      Top = 38
      Width = 326
      Height = 24
      Anchors = [akLeft, akTop, akRight]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
    end
  end
  object paTop: TPanel
    Left = 0
    Top = 0
    Width = 496
    Height = 91
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      496
      91)
    object bbDictionary: TJrSpeedButton
      Left = 460
      Top = 13
      Width = 31
      Height = 31
      Hint = 'Select dictionary'
      Anchors = [akTop, akRight]
      Flat = True
      Images = imlGlyphs
      ImageIndex = 0
      Layout = blGlyphLeft
      ParentShowHint = False
      ShowHint = True
      OnClick = bbDictionaryClick
    end
    object bbTranslation: TJrSpeedButton
      Left = 460
      Top = 57
      Width = 31
      Height = 31
      Hint = 'Open po file'
      Anchors = [akTop, akRight]
      Flat = True
      Images = imlGlyphs
      ImageIndex = 0
      Layout = blGlyphLeft
      ParentShowHint = False
      ShowHint = True
      OnClick = bbTranslationClick
    end
    object Label2: TLabel
      Left = 10
      Top = 50
      Width = 103
      Height = 13
      Caption = 'po file to be checked:'
    end
    object Label5: TLabel
      Left = 10
      Top = 5
      Width = 69
      Height = 13
      Caption = 'Dictionary file:'
    end
    object edDictionary: TComboBox
      Left = 10
      Top = 20
      Width = 451
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnCloseUp = edDictionaryCloseUp
    end
    object edTranslation: TComboBox
      Left = 10
      Top = 65
      Width = 451
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      OnCloseUp = edTranslationCloseUp
    end
  end
  object paCenter: TPanel
    Left = 0
    Top = 91
    Width = 496
    Height = 100
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      496
      100)
    object Label3: TLabel
      Left = 10
      Top = 5
      Width = 96
      Height = 13
      Caption = 'Translation (msgstr)'
    end
    object meTrans: TMemo
      Left = 10
      Top = 20
      Width = 476
      Height = 75
      TabStop = False
      Anchors = [akLeft, akTop, akRight, akBottom]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      TabOrder = 0
    end
  end
  object OpenDialog: TOpenDialog
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 335
    Top = 110
  end
  object imlGlyphs: TSVGIconImageList
    Size = 24
    SVGIconItems = <
      item
        IconName = 'doc-open'
        SVGText = 
          '<svg height="48" width="48" xmlns="http://www.w3.org/2000/svg" x' +
          'mlns:xlink="http://www.w3.org/1999/xlink"><radialGradient id="a"' +
          ' cx="20.706017" cy="37.517986" gradientTransform="matrix(-1.0550' +
          '22 -.02734504 -.177703 1.190929 51.097752 -1.215778)" gradientUn' +
          'its="userSpaceOnUse" r="30.905205"><stop offset="0" stop-color="' +
          '#1f1313"/><stop offset="1" stop-color="#d6b0b0"/></radialGradien' +
          't><linearGradient id="b" gradientTransform="matrix(1.317489 0 0 ' +
          '.816256 -.879573 -1.318166)" gradientUnits="userSpaceOnUse" x1="' +
          '13.035696" x2="12.853771" y1="32.567184" y2="46.689312"><stop of' +
          'fset="0" stop-color="#fff"/><stop offset="1" stop-color="#fff" s' +
          'top-opacity="0"/></linearGradient><linearGradient id="c" gradien' +
          'tTransform="matrix(-1 0 0 1 47.525575 5.909523)" gradientUnits="' +
          'userSpaceOnUse" x1="18.112709" x2="15.514889" y1="31.36775" y2="' +
          '6.18025"><stop offset="0" stop-color="#4e3030"/><stop offset="1"' +
          ' stop-color="#965d5d"/></linearGradient><linearGradient id="d" g' +
          'radientUnits="userSpaceOnUse" x1="22.175976" x2="22.065331" y1="' +
          '36.987999" y2="32.050499"><stop offset="0" stop-color="#f8ce3c"/' +
          '><stop offset="1" stop-color="#f9dc53"/></linearGradient><path d' +
          '="m43.003795 44.596941c-.0218.416304-.459905.832609-.87621.83260' +
          '9h-31.327021c-.416302 0-.8108117-.416305-.789016-.832609l.936443' +
          '-27.226735c.0218-.416303.459897-.832616.876201-.832616h13.270873' +
          'c.485057 0 1.234473-.315589 1.401644-1.106632l.611391-2.893072c.' +
          '155469-.735673.882214-1.037886 1.298518-1.037886h14.77886c.41631' +
          '3 0 .810821.416304.789025.832608z" fill="url(#a)" stroke="url(#c' +
          ')" stroke-linecap="round" stroke-linejoin="round"/><rect fill="#' +
          'ececec" height="29.16666343333" rx="2.49999994286" stroke="#7a7c' +
          '77" stroke-miterlimit="10.433" stroke-width="1.16666883333" widt' +
          'h="26.83333136667" x="10.58333391667" y="2.58333488333"/><rect f' +
          'ill="none" height="26.95773184889" rx="1.15490323501" stroke="#f' +
          'ff" stroke-miterlimit="10.433" stroke-width="1.07446484651" widt' +
          'h="24.62463218219" x="11.65723526325" y="3.71721922135"/><g fill' +
          '="#91938e"><path d="m14.2000017 5.9666682h19.5999972v2.7999996h-' +
          '19.5999972z"/><path d="m14.200001 10.8666668h19.5999972v2.799999' +
          '6h-19.5999972z"/><path d="m14.200001 15.7666654h19.5999972v2.799' +
          '9996h-19.5999972z"/><path d="m14.200001 20.6666647h19.5999972v2.' +
          '7999996h-19.5999972z"/><path d="m14.200001 25.5666654h13.999998v' +
          '2.7999996h-13.999998z"/></g><g transform="matrix(-1 0 0 1 47.525' +
          '575 5.909523)"><path d="m39.783532 39.51062c1.143894-.04406 1.96' +
          '3076-1.096299 2.047035-2.321005.791787-11.548687 1.65936-21.2319' +
          '49 1.65936-21.231949.07215-.247484-.167911-.494967-.48014-.49496' +
          '7h-34.3711566s-1.8503191 21.866892-1.8503191 21.866892c-.1145551' +
          '.982066-.4660075 1.804718-1.5498358 2.183713z" fill="url(#d)" st' +
          'roke="#bea41b" stroke-linejoin="round"/><path d="m9.6202444 16.4' +
          '63921 32.7910986.06481-1.574046 20.001979c-.08432 1.071511-.4506' +
          '78 1.428215-1.872656 1.428215-1.871502 0-28.677968-.03241-31.394' +
          '742-.03241.2335983-.320811.3337557-.988623.3350963-1.004612z" fi' +
          'll="none" opacity=".465909" stroke="url(#b)" stroke-linecap="rou' +
          'nd"/><path d="m9.6202481 16.223182-1.1666467 15.643271s8.2961546' +
          '-4.148078 18.6663476-4.148078 15.55529-11.495193 15.55529-11.495' +
          '193z" fill="#fff" fill-opacity=".089286" fill-rule="evenodd"/></' +
          'g></svg>'
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
        IconName = 'info-blue'
        SVGText = 
          '<svg height="48" width="48" xmlns="http://www.w3.org/2000/svg" x' +
          'mlns:xlink="http://www.w3.org/1999/xlink"><linearGradient id="a"' +
          ' gradientUnits="userSpaceOnUse" x1="25.27776876211" x2="24.95671' +
          '959522" y1="-.28571423142" y2="47.88877673545"><stop offset="0" ' +
          'stop-color="#fff"/><stop offset="1" stop-color="#fff" stop-opaci' +
          'ty="0"/></linearGradient><linearGradient id="b" gradientUnits="u' +
          'serSpaceOnUse" x1="12.674329" x2="37.179268" y1="1.778099" y2="4' +
          '4.221901"><stop offset="0" stop-color="#8fb3d9"/><stop offset="1' +
          '" stop-color="#204a87"/></linearGradient><g transform="translate' +
          '(0 1)"><path d="m45.499979 22.999239c0 11.874491-9.626578 21.500' +
          '741-21.499708 21.500741-11.874218 0-21.5002507-9.626359-21.50025' +
          '07-21.500741 0-11.873949 9.6260327-21.4992191 21.5002507-21.4992' +
          '191 11.87313 0 21.499708 9.6252701 21.499708 21.4992191z" fill="' +
          'url(#b)" stroke="#003380" stroke-width="1.00004"/><path d="m44.4' +
          '9904 22.999272c0 11.32219-9.178617 20.500703-20.499249 20.500703' +
          '-11.321667 0-20.4997657-9.178619-20.4997657-20.500703 0-11.32166' +
          '6 9.1780987-20.4992471 20.4997657-20.4992471 11.320632 0 20.4992' +
          '49 9.1775811 20.499249 20.4992471z" fill="none" opacity=".6" str' +
          'oke="url(#a)" stroke-width="1.00005"/><g fill="#fff"><path d="m2' +
          '1.992178 20.371291h-5.231767v-3.463539h9.789054v15.057279h5.3411' +
          '42v3.463539h-15.768216v-3.463539h5.869787z"/><circle cx="24.0301' +
          '09" cy="11.544907" r="2.946429"/></g></g></svg>'
      end
      item
        IconName = 'switchoff'
        SVGText = 
          '<svg height="48" width="48" xmlns="http://www.w3.org/2000/svg" x' +
          'mlns:xlink="http://www.w3.org/1999/xlink"><linearGradient id="a"' +
          ' gradientUnits="userSpaceOnUse" x1="25.27776876211" x2="24.95671' +
          '959522" y1="-.28571423142" y2="47.88877673545"><stop offset="0" ' +
          'stop-color="#fff"/><stop offset="1" stop-color="#fff" stop-opaci' +
          'ty="0"/></linearGradient><linearGradient id="b" gradientTransfor' +
          'm="matrix(1.0805272 0 0 1.0805026 -1.508286 -3.35076)" gradientU' +
          'nits="userSpaceOnUse" x1="15.046636" x2="15.046636" y1="44.78799' +
          '8" y2="3.885126"><stop offset="0" stop-color="#a40000"/><stop of' +
          'fset="1" stop-color="#c22f2f"/></linearGradient><radialGradient ' +
          'id="c" cx="62.202274" cy="-5.713216" gradientTransform="matrix(-' +
          '7.565785 -.00000062 .0000004 -4.8230546 494.60904 -26.555114)" g' +
          'radientUnits="userSpaceOnUse" r="9.755283"><stop offset="0" stop' +
          '-color="#e78181"/><stop offset=".25288007" stop-color="#e15f5f"/' +
          '><stop offset=".68271071" stop-color="#c22f2f"/><stop offset="1"' +
          ' stop-color="#a40000"/></radialGradient><g transform="translate(' +
          '0 1)"><path d="m45.499979 22.999239c0 11.874491-9.626578 21.5007' +
          '41-21.499708 21.500741-11.874218 0-21.5002507-9.626359-21.500250' +
          '7-21.500741 0-11.873949 9.6260327-21.4992191 21.5002507-21.49921' +
          '91 11.87313 0 21.499708 9.6252701 21.499708 21.4992191z" fill="u' +
          'rl(#c)" stroke="url(#b)" stroke-width="1.00004"/><g fill="none">' +
          '<path d="m44.49904 22.999272c0 11.32219-9.178617 20.500703-20.49' +
          '9249 20.500703-11.321667 0-20.4997657-9.178619-20.4997657-20.500' +
          '703 0-11.321666 9.1780987-20.4992471 20.4997657-20.4992471 11.32' +
          '0632 0 20.499249 9.1775811 20.499249 20.4992471z" opacity=".6" s' +
          'troke="url(#a)" stroke-width="1.00005"/><path d="m18.107539 14.0' +
          '23248c-9.840864 5.366144-5.390853 20.743252 5.859627 20.743252 1' +
          '1.133286 0 16.123044-14.578211 5.859623-20.743252m-5.782894 6.60' +
          '0609v-10.433142" stroke="#c22f2f" stroke-linecap="round" stroke-' +
          'miterlimit="0" stroke-width="5.249"/><g stroke="#feffff" stroke-' +
          'linecap="round" stroke-miterlimit="0" stroke-width="2.18481" tra' +
          'nsform="matrix(1.4073757 0 0 1.4073879 -9.029408 -8.222164)"><pa' +
          'th d="m19.281951 15.80617c-6.992352 3.812838-3.830429 14.73883 4' +
          '.163513 14.73883 7.910671 0 11.456106-10.358346 4.163511-14.7388' +
          '3"/><path d="m23.499983 20.496141v-7.413124"/></g></g></g></svg>'
      end
      item
        IconName = 'refresh'
        SVGText = 
          '<svg height="48" width="48" xmlns="http://www.w3.org/2000/svg" x' +
          'mlns:xlink="http://www.w3.org/1999/xlink"><linearGradient id="a"' +
          ' gradientTransform="matrix(.95419257 0 0 .96222109 .499679 1.718' +
          '761)" gradientUnits="userSpaceOnUse" x1="24.668951" x2="39.68633' +
          '7" y1="42.95816" y2="28.50532"><stop offset="0" stop-color="#fff' +
          '" stop-opacity=".5"/><stop offset=".57424062" stop-color="#7db9e' +
          '8"/><stop offset="1" stop-color="#fff" stop-opacity="0"/></linea' +
          'rGradient><linearGradient id="b" gradientTransform="matrix(1.021' +
          '3968 0 0 1 -.507787 -.999521)" gradientUnits="userSpaceOnUse" x1' +
          '="33.686768" x2="37.039948" y1="35.773647" y2="29.856804"><stop ' +
          'offset="0" stop-color="#14599f"/><stop offset="1" stop-color="#2' +
          '04a87" stop-opacity="0"/></linearGradient><linearGradient id="c"' +
          ' gradientTransform="matrix(1.0213968 0 0 1 -.507787 -.999521)" g' +
          'radientUnits="userSpaceOnUse" x1="31.226292" x2="34.96563" y1="3' +
          '4.600941" y2="26.842505"><stop offset="0" stop-color="#006bbd"/>' +
          '<stop offset="1" stop-color="#3465a4" stop-opacity="0"/></linear' +
          'Gradient><linearGradient id="d" gradientUnits="userSpaceOnUse" x' +
          '1="24.03189188276" x2="35.11537158975" y1="2.995163809" y2="31.9' +
          '0397103798"><stop offset="0" stop-color="#729fcf"/><stop offset=' +
          '"1" stop-color="#006bbd"/></linearGradient><g transform="matrix(' +
          '-1 0 0 1 48.000776 0)"><path d="m24.000386 32.500479c-4.894952-.' +
          '05388-8.553204-4.126736-8.499087-9 .05412-4.873262 4.104132-9.05' +
          '3879 8.999087-9 2.36391.02602 4.43898.509791 6.136756 2.147585l-' +
          '4.136756 4.852415h16v-16.0000003l-4.471103 3.60953c-3.708226-3.6' +
          '439939-7.994745-5.5533785-13.467619-5.6136173-11.133559-.1225452' +
          '-20.013113 8.9469836-20.136204 20.0312096-.123092 11.084225 8.73' +
          '5733 19.753855 19.869292 19.876399" fill="url(#d)" stroke="#1460' +
          '9f" stroke-linejoin="round"/><path d="m23.866941 43.402143c9.509' +
          '325.102473 20.150949-7.091093 20.133835-19.52578l-10.070085-.076' +
          '74c-.185391 5.072466-5.665301 8.889748-10.180305 8.70086" fill="' +
          'url(#c)" stroke="url(#b)" stroke-width="1.01064"/><g fill="none"' +
          '><path d="m24.250386 33.650479c-5.423155-.07929-9.744296-3.86846' +
          '-9.75-10.15-.0049-5.396381 4.672169-10.219134 10.095325-10.13984' +
          '8 2.673075.03908 5.712878.890037 7.726664 3.166851l-3.326033 3.8' +
          '33131 12.383472.02029v-12.4640654l-3.377628 2.8747284c-3.542022-' +
          '3.9509003-7.981015-6.1007985-13.521418-6.1562401-10.597787-.1060' +
          '495-18.806515 8.4013031-18.888409 18.9990571.259072 11.855739 9.' +
          '392781 18.618757 18.702543 18.821492" stroke="#fff" stroke-opaci' +
          'ty=".5" stroke-width="1.193"/><path d="m24.294902 42.455875c7.95' +
          '111 0 18.221834-5.583144 18.322006-18.54338l-8.055423-.207989c.0' +
          '072 5.258838-4.745611 9.805108-10.30785 9.971413" stroke="url(#a' +
          ')" stroke-linejoin="round" stroke-width="1.19775"/></g></g></svg' +
          '>'
      end
      item
        IconName = 'save'
        SVGText = 
          '<svg height="48" viewBox="0 0 48 48" width="48" xmlns="http://ww' +
          'w.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink"><l' +
          'inearGradient id="a" gradientTransform="matrix(.97754193 0 0 1.1' +
          '397756 .56166 -3.270842)" gradientUnits="userSpaceOnUse" x1="40.' +
          '884724" x2="16.879831" y1="71.869133" y2="-.389314"><stop offset' +
          '="0" stop-color="#1e2d69"/><stop offset="1" stop-color="#78a7e0"' +
          '/></linearGradient><linearGradient id="b" gradientTransform="mat' +
          'rix(.985432 0 0 1.148179 .64107 -2.933883)" gradientUnits="userS' +
          'paceOnUse" x1="13.783585" x2="33.074715" y1="-.996729" y2="55.70' +
          '1546"><stop offset="0" stop-color="#fff"/><stop offset="1" stop-' +
          'color="#fff" stop-opacity="0"/></linearGradient><linearGradient ' +
          'id="c" gradientTransform="matrix(1.067698 0 0 1.121532 -1.368937' +
          ' -5.57446)" gradientUnits="userSpaceOnUse" x1="20.125" x2="28.56' +
          '25" y1="21.84375" y2="42.46875"><stop offset="0" stop-color="#85' +
          '8585"/><stop offset=".5" stop-color="#cbcbcb"/><stop offset="1" ' +
          'stop-color="#6b6b6b"/></linearGradient><g transform="matrix(.833' +
          '65948 0 0 .85623432 3.973269 3.866598)"><path d="m4.5590083 3.56' +
          '78147h38.9273337c.589855 0 1.064721.4744086 1.064721 1.0636961v3' +
          '7.7647652c0 .589288-.474866 1.063696-1.064721 1.063696h-36.92542' +
          '93s-3.0666258-3.063672-3.0666258-3.063672v-35.7647892c0-.5892875' +
          '.4748658-1.0636961 1.0647214-1.0636961z" fill="url(#a)" stroke="' +
          '#25375f" stroke-linecap="round" stroke-linejoin="round" stroke-w' +
          'idth="1.00047"/><path d="m9 4h30v23h-30z" fill="#fff"/><rect fil' +
          'l="#d31c00" height="4" rx=".126208" width="30" x="9" y="4"/><rec' +
          't height="2" opacity=".738636" rx=".126208" width="2" x="6" y="6' +
          '"/><g stroke="#000"><path d="m11 12.5h26" opacity=".130682"/><pa' +
          'th d="m11 17.5h26" opacity=".130682"/><path d="m11 22.5h26" opac' +
          'ity=".130682"/></g><path d="m4.6189226 4.5276647h38.7684814c.069' +
          '92 0 .126208.056289.126208.1262077v37.6482386c0 .06992-.05629.12' +
          '6208-.126208.126208h-36.4591222s-2.4355669-2.391373-2.4355669-2.' +
          '391373v-35.3830736c0-.069919.056289-.1262077.1262077-.1262077z" ' +
          'fill="none" opacity=".596591" stroke="url(#b)" stroke-linecap="r' +
          'ound"/><path d="m14.113967 28.562183h19.749824c.887971 0 1.60283' +
          '6.75091 1.602836 1.683653v13.201551h-22.955496v-13.201551c0-.932' +
          '743.714865-1.683653 1.602836-1.683653z" fill="url(#c)" stroke="#' +
          '525252" stroke-width=".999999"/><rect fill="#4967a2" height="10.' +
          '06597" rx=".751207" ry=".751208" stroke="#525252" width="5.02975' +
          '3" x="16.464279" y="30.4566"/></g></svg>'
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
    Left = 435
    Top = 98
  end
  object pmSugg: TPopupMenu
    Alignment = paRight
    AutoHotkeys = maManual
    AutoPopup = False
    OwnerDraw = True
    Left = 385
    Top = 133
  end
end
