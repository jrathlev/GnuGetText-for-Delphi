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
  OnAfterMonitorDpiChanged = FormAfterMonitorDpiChanged
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
    Left = 95
    Top = 265
    Width = 3
    Height = 13
    Anchors = [akLeft, akBottom]
  end
  object laProg: TLabel
    Left = 95
    Top = 250
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
  object bbAll: TJrButton
    Left = 10
    Top = 245
    Width = 36
    Height = 36
    Anchors = [akLeft, akBottom]
    Images = imlGlyphs
    ImageIndex = 5
    Layout = blGlyphLeft
    TabOrder = 1
    OnClick = bbAllClick
  end
  object bbNone: TJrButton
    Left = 50
    Top = 245
    Width = 36
    Height = 36
    Anchors = [akLeft, akBottom]
    Images = imlGlyphs
    ImageIndex = 6
    Layout = blGlyphLeft
    TabOrder = 2
    OnClick = bbNoneClick
  end
  object ButtonCancel: TJrButton
    Left = 390
    Top = 245
    Width = 91
    Height = 36
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    Images = imlGlyphs
    ImageIndex = 1
    Layout = blGlyphLeft
    ParentDoubleBuffered = True
    TabOrder = 3
    OnClick = ButtonCancelClick
  end
  object ButtonOK: TJrButton
    Left = 300
    Top = 245
    Width = 86
    Height = 36
    Anchors = [akRight, akBottom]
    Caption = 'Start'
    Default = True
    Images = imlGlyphs
    ImageIndex = 0
    Layout = blGlyphLeft
    ParentDoubleBuffered = True
    TabOrder = 4
    OnClick = ButtonOKClick
  end
  object btnHelp: TJrButton
    Left = 260
    Top = 245
    Width = 36
    Height = 36
    Hint = 'Show program help'
    Anchors = [akRight, akBottom]
    Images = imlGlyphs
    ImageIndex = 2
    Layout = blGlyphLeft
    ParentShowHint = False
    ShowHint = True
    TabOrder = 5
    OnClick = btnHelpClick
  end
  object btnManual: TJrButton
    Left = 220
    Top = 245
    Width = 36
    Height = 36
    Hint = 'Show DxGetText manual'
    Anchors = [akRight, akBottom]
    Images = imlGlyphs
    ImageIndex = 3
    Layout = blGlyphLeft
    ParentShowHint = False
    ShowHint = True
    TabOrder = 6
    OnClick = btnManualClick
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'exe'
    Filter = 'exe files|*.exe'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofNoReadOnlyReturn, ofEnableSizing]
    Left = 380
    Top = 10
  end
  object imlGlyphs: TSVGIconImageList
    Size = 24
    SVGIconItems = <
      item
        IconName = 'run'
        SVGText = 
          '<svg height="48" width="48" xmlns="http://www.w3.org/2000/svg" x' +
          'mlns:xlink="http://www.w3.org/1999/xlink"><linearGradient id="a"' +
          ' gradientUnits="userSpaceOnUse" x1="39.322529" x2="10.229713" y1' +
          '="44.808796" y2=".83299"><stop offset="0" stop-color="#098633"/>' +
          '<stop offset="1" stop-color="#8dfcb2"/></linearGradient><g trans' +
          'form="translate(0 1)"><path d="m45.499979 22.999239c0 11.874491-' +
          '9.626578 21.500741-21.499708 21.500741-11.874218 0-21.5002507-9.' +
          '626359-21.5002507-21.500741 0-11.873949 9.6260327-21.4992185 21.' +
          '5002507-21.4992185 11.87313 0 21.499708 9.6252695 21.499708 21.4' +
          '992185z" fill="url(#a)" stroke="#00802b" stroke-width="1.00004"/' +
          '><path d="m44.49904 22.999272c0 11.32219-9.178617 20.500703-20.4' +
          '99249 20.500703-11.321667 0-20.499766-9.178619-20.499766-20.5007' +
          '03 0-11.321666 9.178099-20.4992465 20.499766-20.4992465 11.32063' +
          '2 0 20.499249 9.1775805 20.499249 20.4992465z" fill="none" opaci' +
          'ty=".501788" stroke="#fff" stroke-opacity=".5" stroke-width="1.0' +
          '0005"/><path d="m26.995171 25.100637-31.9029081-8.90567 23.41346' +
          '51-23.624546z" fill="#fff" stroke="#00802a" stroke-dashoffset="1' +
          '.33" stroke-linecap="round" stroke-linejoin="round" transform="m' +
          'atrix(.64317347 -.64769705 .64769705 .64317347 7.644791 24.58485' +
          '6)"/></g></svg>'
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
        IconName = 'check-all'
        SVGText = 
          '<svg height="48" viewBox="0 0 48 48" width="48" xmlns="http://ww' +
          'w.w3.org/2000/svg"><g fill="#fff" stroke-linejoin="round" transf' +
          'orm="matrix(.09615255 0 0 .09615255 -1.050895 -1.364958)"><g fil' +
          'l="#fff" stroke="#4a7ebe" stroke-width="15.6"><rect height="112.' +
          '62267" rx="14.077833" width="112.62267" x="49.929874" y="344.400' +
          '12"/><rect height="112.62263" rx="14.07791" ry="14.077828" width' +
          '="112.62329" x="49.929893" y="198.79816"/><rect height="112.6226' +
          '7" rx="14.077833" width="112.62267" x="49.929874" y="53.196178"/' +
          '></g><path d="m352 176-134.4 160-57.6-64" fill="none" stroke="#0' +
          '00" stroke-linecap="round" stroke-width="41.6" transform="matrix' +
          '(.31719408 0 0 .31719408 25.039526 28.306003)"/><path d="m352 17' +
          '6-134.4 160-57.6-64" fill="none" stroke="#000" stroke-linecap="r' +
          'ound" stroke-width="41.6" transform="matrix(.31719408 0 0 .31719' +
          '408 25.039526 173.90605)"/><path d="m352 176-134.4 160-57.6-64" ' +
          'fill="none" stroke="#000" stroke-linecap="round" stroke-width="4' +
          '1.6" transform="matrix(.31721529 0 0 .31719164 25.036328 318.572' +
          '63)"/></g><g fill="#666"><path d="m18 9h20v3h-20z"/><path d="m18' +
          ' 23h20v3h-20z"/><path d="m18 37h20v3h-20z"/></g></svg>'
      end
      item
        IconName = 'check-none'
        SVGText = 
          '<svg height="48" viewBox="0 0 48 48" width="48" xmlns="http://ww' +
          'w.w3.org/2000/svg"><g fill="#fff" stroke="#4a7ebe" stroke-linejo' +
          'in="round" stroke-width="1.49997978"><rect height="10.8289569083' +
          '1" rx="1.35361954142" width="10.82895690831" x="3.74998970628" y' +
          '="3.74999016495"/><rect height="10.82895306221" rx="1.3536269451' +
          '7" ry="1.35361906066" width="10.82901652289" x="3.74999153318" y' +
          '="17.74999201931"/><rect height="10.82895690831" rx="1.353619541' +
          '42" width="10.82895690831" x="3.74998970628" y="31.74999175831"/' +
          '></g><g fill="#666"><path d="m18 9h20v3h-20z"/><path d="m18 23h2' +
          '0v3h-20z"/><path d="m18 37h20v3h-20z"/></g></svg>'
      end>
    Left = 430
    Top = 10
  end
end
