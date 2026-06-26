object MainForm: TMainForm
  Left = 0
  Top = 0
  ClientHeight = 401
  ClientWidth = 631
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
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    631
    401)
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 10
    Top = 10
    Width = 112
    Height = 13
    Caption = 'Read translations from '
  end
  object Label1: TLabel
    Left = 10
    Top = 55
    Width = 103
    Height = 13
    Caption = 'Unit to be translated:'
  end
  object Label3: TLabel
    Left = 10
    Top = 100
    Width = 205
    Height = 13
    Caption = 'Strings found in unit (D = dfm, P = source)'
  end
  object Label4: TLabel
    Left = 510
    Top = 10
    Width = 47
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Language'
    ExplicitLeft = 500
  end
  object lvStrings: TListView
    Left = 10
    Top = 115
    Width = 536
    Height = 258
    Anchors = [akLeft, akTop, akRight, akBottom]
    Checkboxes = True
    Columns = <
      item
        Caption = 'Template'
        Width = 240
      end
      item
        Width = 20
      end
      item
        Caption = 'Translation'
        Width = 240
      end>
    TabOrder = 6
    ViewStyle = vsReport
  end
  object edLang: TEdit
    Left = 515
    Top = 25
    Width = 76
    Height = 21
    Anchors = [akTop, akRight]
    TabOrder = 1
    OnChange = edLangChange
  end
  object edPasFile: TEdit
    Left = 10
    Top = 70
    Width = 581
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 382
    Width = 631
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object edPoFile: TComboBox
    Left = 10
    Top = 25
    Width = 496
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object bbExit: TJrButton
    Left = 550
    Top = 340
    Width = 76
    Height = 36
    Hint = 'Quit program'
    Anchors = [akRight, akBottom]
    Images = imlGlyphs
    ImageIndex = 3
    Layout = blGlyphLeft
    ParentShowHint = False
    ShowHint = True
    TabOrder = 7
    OnClick = bbExitClick
  end
  object bbInfo: TJrButton
    Left = 590
    Top = 300
    Width = 36
    Height = 36
    Hint = 'About the program'
    Anchors = [akRight, akBottom]
    Images = imlGlyphs
    ImageIndex = 2
    Layout = blGlyphLeft
    ParentShowHint = False
    ShowHint = True
    TabOrder = 8
    OnClick = bbInfoClick
  end
  object bbReplace: TJrButton
    Left = 550
    Top = 115
    Width = 76
    Height = 91
    Hint = 'Replace selected strings and save in subdirectory'
    Anchors = [akTop, akRight]
    Caption = 'Replace strings'
    Images = imlGlyphs
    ImageIndex = 4
    ParentShowHint = False
    ShowHint = True
    TabOrder = 5
    WordWrap = True
    OnClick = bbReplaceClick
  end
  object bbUnit: TJrButton
    Left = 595
    Top = 62
    Width = 31
    Height = 31
    Hint = 'Open po file'
    Anchors = [akTop, akRight]
    Images = imlGlyphs
    ImageIndex = 0
    Layout = blGlyphLeft
    ParentShowHint = False
    ShowHint = True
    TabOrder = 4
    OnClick = bbUnitClick
  end
  object bbPoFile: TJrButton
    Left = 595
    Top = 17
    Width = 31
    Height = 31
    Hint = 'Open po file'
    Anchors = [akTop, akRight]
    Images = imlGlyphs
    ImageIndex = 0
    Layout = blGlyphLeft
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    OnClick = bbPoFileClick
  end
  object btnHelp: TJrButton
    Left = 550
    Top = 300
    Width = 36
    Height = 36
    Hint = 'Show program help'
    Anchors = [akRight, akBottom]
    Images = imlGlyphs
    ImageIndex = 1
    Layout = blGlyphLeft
    ParentShowHint = False
    ShowHint = True
    TabOrder = 10
    OnClick = btnHelpClick
  end
  object OpenDialog: TOpenDialog
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 565
    Top = 215
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
        IconName = 'replace'
        SVGText = 
          '<svg height="48" viewBox="0 0 48 48" width="48" xmlns="http://ww' +
          'w.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink"><l' +
          'inearGradient id="a" gradientUnits="userSpaceOnUse" x1="26.32695' +
          '8" x2="38.2808" y1="23.567663" y2="25.128147"><stop offset="0" s' +
          'top-color="#e7121a"/><stop offset="1" stop-color="#c21f00"/></li' +
          'nearGradient><linearGradient id="b" gradientTransform="matrix(.6' +
          '931485 0 0 -.6847878 11.470083 38.638069)" gradientUnits="userSp' +
          'aceOnUse" x1="21.244122" x2="37.327244" y1="23.389814" y2="20.94' +
          '1328"><stop offset="0" stop-color="#17cc53"/><stop offset="1" st' +
          'op-color="#009632"/></linearGradient><g transform="matrix(.93800' +
          '421 0 0 .93800421 -4.155855 -9.154477)"><path d="m24.5 13.5-11 1' +
          '0 11 10.46875.03125-6.46875h9.375c4.644932-.191207 7.691828 2.15' +
          '6247 7.5625 5v.125 6.8125c10.220318-7.052696 3.633048-20.077177-' +
          '7.5625-19.96875l-9.40625.0625z" style="stroke:#b42d34;stroke-wid' +
          'th:1.0815;stroke-linecap:round;stroke-linejoin:round;stroke-mite' +
          'rlimit:10;fill:url(#a)"/><path d="m23.46875 15.8125-8.46875 7.68' +
          '75 8.46875 8.0625.03125-4.0625c.000328-.569408.461842-1.030922 1' +
          '.03125-1.03125h9.375c2.505095-.103121 4.601649.454623 6.15625 1.' +
          '53125 1.545505 1.070327 2.506852 2.710188 2.4375 4.5-.000408.010' +
          '53.00048.02071 0 .03125v.09375 4.4375c3.124654-3.062309 3.650684' +
          '-6.681869 2.28125-9.875-1.598769-3.727873-5.679225-6.73782-10.87' +
          '5-6.6875l-9.40625.0625c-.569408-.000328-1.030922-.461842-1.03125' +
          '-1.03125z" fill="none" stroke="#e69b9e" stroke-width=".999999"/>' +
          '</g><g transform="matrix(-.94022418 0 0 -.94022418 52.222948 57.' +
          '221567)"><path d="m24.5 13.5-11 10 11 10.46875.03125-6.46875h9.3' +
          '75c4.644932-.191207 7.691828 2.156247 7.5625 5v.125 6.8125c10.22' +
          '0318-7.052696 3.633048-20.077177-7.5625-19.96875l-9.40625.0625z"' +
          ' display="block" fill="url(#b)" stroke="#00802b" stroke-linejoin' +
          '="round" stroke-width=".999975"/><path d="m23.46875 15.8125-8.46' +
          '875 7.6875 8.46875 8.0625.03125-4.0625c.000328-.569408.461842-1.' +
          '030922 1.03125-1.03125h9.375c2.505095-.103121 4.601649.454623 6.' +
          '15625 1.53125 1.545505 1.070327 2.506852 2.710188 2.4375 4.5-.00' +
          '0408.01053.00048.02071 0 .03125v.09375 4.4375c3.124654-3.062309 ' +
          '3.650684-6.681869 2.28125-9.875-1.598769-3.727873-5.679225-6.737' +
          '82-10.875-6.6875l-9.40625.0625c-.569408-.000328-1.030922-.461842' +
          '-1.03125-1.03125z" display="block" fill="none" opacity=".50298" ' +
          'stroke="#fff" stroke-width="1.0815"/></g></svg>'
      end>
    Left = 495
    Top = 53
  end
end
