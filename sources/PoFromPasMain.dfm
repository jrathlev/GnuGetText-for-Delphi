object frmMain: TfrmMain
  Left = 0
  Top = 0
  ClientHeight = 286
  ClientWidth = 501
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
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar: TStatusBar
    Left = 0
    Top = 267
    Width = 501
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object paTop: TPanel
    Left = 0
    Top = 0
    Width = 501
    Height = 106
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      501
      106)
    object Label2: TLabel
      Left = 10
      Top = 10
      Width = 192
      Height = 13
      Caption = 'Directory with translated Pascal sources'
    end
    object Label1: TLabel
      Left = 10
      Top = 55
      Width = 98
      Height = 13
      Caption = 'Selected source files'
    end
    object btDir: TJrSpeedButton
      Left = 465
      Top = 17
      Width = 31
      Height = 31
      Anchors = [akTop, akRight]
      Flat = True
      Images = imlGlyphs
      ImageIndex = 1
      Layout = blGlyphLeft
      OnClick = btDirClick
    end
    object sbSources: TJrSpeedButton
      Left = 465
      Top = 62
      Width = 31
      Height = 31
      Hint = 'Select Pascal sources'
      Anchors = [akTop, akRight]
      Flat = True
      Images = imlGlyphs
      ImageIndex = 0
      Layout = blGlyphLeft
      ParentShowHint = False
      ShowHint = True
      OnClick = sbSourcesClick
    end
    object edDir: TComboBox
      Left = 10
      Top = 25
      Width = 451
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnCloseUp = edDirCloseUp
    end
    object edSources: TComboBox
      Left = 10
      Top = 70
      Width = 451
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
    end
  end
  object paCenter: TPanel
    Left = 0
    Top = 106
    Width = 501
    Height = 161
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      501
      161)
    object Label5: TLabel
      Left = 10
      Top = 60
      Width = 141
      Height = 13
      Caption = 'Insert translations into po file'
    end
    object sbCopy: TJrSpeedButton
      Left = 435
      Top = 70
      Width = 31
      Height = 31
      Hint = 'Copy directory'
      Anchors = [akTop, akRight]
      Flat = True
      Images = imlGlyphs
      ImageIndex = 6
      Layout = blGlyphLeft
      ParentShowHint = False
      ShowHint = True
      OnClick = sbCopyClick
    end
    object sbEdit: TJrSpeedButton
      Left = 465
      Top = 67
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
      OnClick = sbEditClick
    end
    object cbOverwrite: TCheckBox
      Left = 245
      Top = 15
      Width = 206
      Height = 36
      Caption = 'Overwrite existing translations'
      TabOrder = 1
      WordWrap = True
    end
    object edEdit: TComboBox
      Left = 10
      Top = 75
      Width = 421
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
      OnCloseUp = edEditCloseUp
    end
    object rgEncoding: TRadioGroup
      Left = 5
      Top = 5
      Width = 221
      Height = 46
      Caption = 'Encoding of source files:'
      Columns = 2
      ItemIndex = 0
      Items.Strings = (
        'ISO-8859-1'
        'UTF-8')
      TabOrder = 0
    end
    object bbExit: TJrButton
      Left = 460
      Top = 120
      Width = 36
      Height = 36
      Hint = 'Quit program'
      Anchors = [akRight, akBottom]
      Images = imlGlyphs
      ImageIndex = 4
      Layout = blGlyphLeft
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
      OnClick = bbExitClick
    end
    object bbInfo: TJrButton
      Left = 420
      Top = 120
      Width = 36
      Height = 36
      Hint = 'About the program'
      Anchors = [akRight, akBottom]
      Images = imlGlyphs
      ImageIndex = 3
      Layout = blGlyphLeft
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
      OnClick = bbInfoClick
    end
    object bbSave: TJrButton
      Left = 105
      Top = 115
      Width = 256
      Height = 41
      Hint = 'Copy translated strings as comments'
      Anchors = [akRight, akBottom]
      Caption = 'Insert translations and save'
      Images = imlGlyphs
      ImageIndex = 5
      Layout = blGlyphLeft
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      OnClick = bbSaveClick
    end
    object btnHelp: TJrButton
      Left = 380
      Top = 120
      Width = 36
      Height = 36
      Hint = 'Show program help'
      Anchors = [akRight, akBottom]
      Images = imlGlyphs
      ImageIndex = 2
      Layout = blGlyphLeft
      ParentShowHint = False
      ShowHint = True
      TabOrder = 6
      OnClick = btnHelpClick
    end
  end
  object OpenDialog: TOpenDialog
    Options = [ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 435
    Top = 115
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
        IconName = 'app-save'
        SVGText = 
          '<svg height="48" width="48" xmlns="http://www.w3.org/2000/svg" x' +
          'mlns:xlink="http://www.w3.org/1999/xlink"><linearGradient id="a"' +
          '><stop offset="0" stop-color="#fff"/><stop offset="1" stop-color' +
          '="#fff" stop-opacity="0"/></linearGradient><radialGradient id="b' +
          '" cx="31" cy="12" gradientUnits="userSpaceOnUse" r="11.125"><sto' +
          'p offset="0" stop-color="#fafcff"/><stop offset="1" stop-color="' +
          '#80b3ff"/></radialGradient><linearGradient id="c" gradientTransf' +
          'orm="matrix(.96592585 -.25881896 .25881896 .96592585 -4.11682 6.' +
          '729649)" gradientUnits="userSpaceOnUse" x1="28.357094" x2="17.73' +
          '2124" xlink:href="#a" y1="22.794661" y2="5.187518"/><linearGradi' +
          'ent id="d" gradientTransform="matrix(.97754193 0 0 1.1397756 .56' +
          '166 -3.270842)" gradientUnits="userSpaceOnUse" x1="40.884724" x2' +
          '="16.879831" y1="71.869133" y2="-.389314"><stop offset="0" stop-' +
          'color="#1e2d69"/><stop offset="1" stop-color="#78a7e0"/></linear' +
          'Gradient><linearGradient id="e" gradientTransform="matrix(.98543' +
          '2 0 0 1.148179 .64107 -2.933883)" gradientUnits="userSpaceOnUse"' +
          ' x1="13.783585" x2="33.074715" xlink:href="#a" y1="-.996729" y2=' +
          '"55.701546"/><linearGradient id="f" gradientTransform="matrix(1.' +
          '067698 0 0 1.121532 -1.368937 -5.57446)" gradientUnits="userSpac' +
          'eOnUse" x1="20.125" x2="28.5625" y1="21.84375" y2="42.46875"><st' +
          'op offset="0" stop-color="#858585"/><stop offset=".5" stop-color' +
          '="#cbcbcb"/><stop offset="1" stop-color="#6b6b6b"/></linearGradi' +
          'ent><g transform="matrix(1.3998729 .37509468 -.37509468 1.399872' +
          '9 -19.391118 -8.922602)"><path d="m31 .875c-.32102.00000002-.583' +
          '975.0443823-.8125.0625l-.5.03125-.03125.5-.1875 2.65625c-1.06009' +
          '2.2090559-2.041676.6405265-2.90625 1.21875l-2.40625-2.03125-1.84' +
          '375 1.84375 2.03125 2.40625c-.578224.864573-1.009695 1.846158-1.' +
          '21875 2.90625l-2.65625.1875-.5.03125-.03125.5c-.018119.228528-.0' +
          '625.491482-.0625.8125 0 .321017.04438.583973.0625.8125l.03125.5.' +
          '5.03125 2.65625.1875c.209056 1.060092.640526 2.041676 1.21875 2.' +
          '90625l-1.71875 2.03125-.3125.375.3125.375c.35567.411929.744319.8' +
          '0058 1.15625 1.15625l.375.3125.375-.3125 2.03125-1.71875c.864573' +
          '.578224 1.846158 1.009695 2.90625 1.21875l.1875 2.65625.03125.5.' +
          '5.03125c.228528.01812.491482.0625.8125.0625.321018.000001.583973' +
          '-.04438.8125-.0625l.5-.03125.03125-.5.1875-2.65625c1.060091-.209' +
          '056 2.041676-.640526 2.90625-1.21875l2.03125 1.71875.375.3125.37' +
          '5-.3125c.411933-.355671.80058-.74432 1.15625-1.15625l.3125-.375-' +
          '.3125-.375-1.71875-2.03125c.578224-.864573 1.009695-1.846158 1.2' +
          '1875-2.90625l2.65625-.1875.5-.03125.03125-.5c.018117-.228525.062' +
          '5-.491482.0625-.8125-.000001-.321015-.04438-.583972-.0625-.8125l' +
          '-.03125-.5-.5-.03125-2.65625-.1875c-.209055-1.0600925-.640526-2.' +
          '0416762-1.21875-2.90625l1.71875-2.03125.3125-.375-.3125-.375c-.3' +
          '55668-.4119287-.744319-.8005796-1.15625-1.15625l-.375-.3125-.375' +
          '.3125-2.03125 1.71875c-.864573-.5782237-1.846158-1.0096946-2.906' +
          '25-1.21875l-.1875-2.65625-.03125-.5-.5-.03125c-.228527-.01811779' +
          '-.491484-.06249877-.8125-.0625zm.505086 6.6894357c1.932001-.0000' +
          '024 3.72824 2.7516393 3.728242 4.6836403.000002 1.932002-2.57976' +
          '8 4.037843-4.511769 4.037845-1.932002.000002-3.971157-2.330894-3' +
          '.971159-4.262895-.000001-1.932002 2.822685-4.4585881 4.754686-4.' +
          '4585903z" fill="url(#b)"/><g fill="none"><circle cx="23.5" cy="1' +
          '9" r="8.5" stroke="url(#c)" stroke-linecap="square" stroke-width' +
          '="2.42857" transform="matrix(.411765 -.00000048 .00000048 .41176' +
          '5 21.323532 4.176472)"/><path d="m352.23859 522.61906 3.17072 4.' +
          '68566 5.19675-.67311 1.8461-5.34796 5.83668 1.56393-1.07521 5.55' +
          '452 4.16396 3.18131 5.08875-2.47254 3.02317 5.23628-4.68565 3.17' +
          '072.6731 5.19675 5.34797 1.8461-1.56394 5.83668-5.55452-1.07521-' +
          '3.1813 4.16396 2.47254 5.08875-5.23629 3.02317-3.17071-4.68566s-' +
          '5.19675.67311-5.19675.67311l-1.8461 5.34797-5.83668-1.56394 1.07' +
          '521-5.55452-4.16397-3.1813-5.08874 2.47254-3.02317-5.23629 4.685' +
          '65-3.17071-.67311-5.19675-5.34796-1.8461 1.56393-5.83669 5.55452' +
          ' 1.07522 3.18131-4.16397-2.47254-5.08874z" stroke="#2a65bd" stro' +
          'ke-width="2" transform="matrix(.5295138 -.14188275 .14188275 .52' +
          '95138 -236.54083 -223.97618)"/><ellipse cx="360" cy="542.10858" ' +
          'rx="8.040032" ry="8.097341" stroke="#2a65bd" stroke-width="2" tr' +
          'ansform="matrix(.5295138 -.14188275 .14188275 .5295138 -236.5408' +
          '3 -223.97618)"/></g></g><g transform="matrix(.71331347 0 0 .7326' +
          '2946 14.864302 14.752601)"><path d="m4.5590083 3.5678147h38.9273' +
          '337c.589855 0 1.064721.4744086 1.064721 1.0636961v37.7647652c0 .' +
          '589288-.474866 1.063696-1.064721 1.063696h-36.9254293s-3.0666258' +
          '-3.063672-3.0666258-3.063672v-35.7647892c0-.5892875.4748658-1.06' +
          '36961 1.0647214-1.0636961z" fill="url(#d)" stroke="#25375f" stro' +
          'ke-linecap="round" stroke-linejoin="round" stroke-width="1.00047' +
          '"/><path d="m9 4h30v23h-30z" fill="#fff"/><rect fill="#d31c00" h' +
          'eight="4" rx=".147501" width="30" x="9" y="4"/><rect height="2" ' +
          'opacity=".738636" rx=".147501" width="2" x="6" y="6"/><g stroke=' +
          '"#000"><path d="m11 12.5h26" opacity=".130682"/><path d="m11 17.' +
          '5h26" opacity=".130682"/><path d="m11 22.5h26" opacity=".130682"' +
          '/></g><path d="m4.6189226 4.5276647h38.7684814c.06992 0 .126208.' +
          '056289.126208.1262077v37.6482386c0 .06992-.05629.126208-.126208.' +
          '126208h-36.4591222s-2.4355669-2.391373-2.4355669-2.391373v-35.38' +
          '30736c0-.069919.056289-.1262077.1262077-.1262077z" fill="none" o' +
          'pacity=".596591" stroke="url(#e)" stroke-linecap="round"/><path ' +
          'd="m14.113967 28.562183h19.749824c.887971 0 1.602836.75091 1.602' +
          '836 1.683653v13.201551h-22.955496v-13.201551c0-.932743.714865-1.' +
          '683653 1.602836-1.683653z" fill="url(#f)" stroke="#525252" strok' +
          'e-width=".999999"/><rect fill="#4967a2" height="10.06597" rx=".8' +
          '77946" ry=".877947" stroke="#525252" width="5.029753" x="16.4642' +
          '79" y="30.4566"/></g></svg>'
      end
      item
        IconName = 'arrow-dl-red'
        SVGText = 
          '<svg height="48" viewBox="0 0 48 48" width="48" xmlns="http://ww' +
          'w.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink"><l' +
          'inearGradient id="a" gradientUnits="userSpaceOnUse" x1="26.32695' +
          '8" x2="38.2808" y1="23.567663" y2="25.128147"><stop offset="0" s' +
          'top-color="#e7121a"/><stop offset="1" stop-color="#c21f00"/></li' +
          'nearGradient><g transform="matrix(1.2897558 0 0 -1.2897558 -14.7' +
          '14301 58.138236)"><path d="m24.5 13.5-11 10 11 10.46875.03125-6.' +
          '46875h9.375c4.644932-.191207 7.691828 2.156247 7.5625 5v.125 6.8' +
          '125c10.220318-7.052696 3.633048-20.077177-7.5625-19.96875l-9.406' +
          '25.0625z" style="stroke:#b42d34;stroke-width:1.0815;stroke-linec' +
          'ap:round;stroke-linejoin:round;stroke-miterlimit:10;fill:url(#a)' +
          '"/><path d="m23.46875 15.8125-8.46875 7.6875 8.46875 8.0625.0312' +
          '5-4.0625c.000328-.569408.461842-1.030922 1.03125-1.03125h9.375c2' +
          '.505095-.103121 4.601649.454623 6.15625 1.53125 1.545505 1.07032' +
          '7 2.506852 2.710188 2.4375 4.5-.000408.01053.00048.02071 0 .0312' +
          '5v.09375 4.4375c3.124654-3.062309 3.650684-6.681869 2.28125-9.87' +
          '5-1.598769-3.727873-5.679225-6.73782-10.875-6.6875l-9.40625.0625' +
          'c-.569408-.000328-1.030922-.461842-1.03125-1.03125z" fill="none"' +
          ' stroke="#e69b9e" stroke-width=".999999"/></g></svg>'
      end>
    Left = 380
    Top = 83
  end
end
