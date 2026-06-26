object frmMain: TfrmMain
  Left = 290
  Top = 216
  ClientHeight = 305
  ClientWidth = 760
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poDesigned
  OnAfterMonitorDpiChanged = FormAfterMonitorDpiChanged
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnMain: TPanel
    Left = 0
    Top = 0
    Width = 760
    Height = 259
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 761
    DesignSize = (
      760
      259)
    object Label1: TLabel
      Left = 10
      Top = 78
      Width = 15
      Height = 13
      Caption = 'ID:'
    end
    object laRefLine: TLabel
      Left = 365
      Top = 75
      Width = 3
      Height = 13
      Alignment = taRightJustify
    end
    object Label3: TLabel
      Left = 10
      Top = 145
      Width = 57
      Height = 13
      Caption = 'Translation:'
    end
    object Label4: TLabel
      Left = 390
      Top = 145
      Width = 57
      Height = 13
      Caption = 'Translation:'
    end
    object laEdLine: TLabel
      Left = 745
      Top = 75
      Width = 3
      Height = 13
      Alignment = taRightJustify
    end
    object laEntry: TLabel
      Left = 10
      Top = 60
      Width = 3
      Height = 13
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object laEditFile: TLabel
      Left = 10
      Top = 15
      Width = 94
      Height = 13
      Caption = 'po file to be edited:'
    end
    object Label5: TLabel
      Left = 390
      Top = 15
      Width = 159
      Height = 13
      Caption = 'po file with changed translations:'
    end
    object meRef: TMemo
      Left = 10
      Top = 160
      Width = 361
      Height = 61
      Anchors = [akLeft, akTop, akBottom]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 6
      OnChange = meRefChange
    end
    object meEdit: TMemo
      Left = 385
      Top = 160
      Width = 366
      Height = 61
      TabStop = False
      Anchors = [akLeft, akTop, akRight, akBottom]
      ReadOnly = True
      TabOrder = 7
    end
    object meID: TMemo
      Left = 10
      Top = 95
      Width = 741
      Height = 41
      TabStop = False
      Anchors = [akLeft, akTop, akRight]
      ReadOnly = True
      TabOrder = 5
    end
    object cbEdit: TComboBox
      Left = 10
      Top = 30
      Width = 301
      Height = 21
      Hint = 'List of po files to be edited'
      DropDownCount = 15
      ParentShowHint = False
      PopupMenu = pmFileList
      ShowHint = True
      TabOrder = 0
      OnCloseUp = cbEditCloseUp
    end
    object cbComp: TComboBox
      Left = 390
      Top = 30
      Width = 331
      Height = 21
      Hint = 'List of po files with changed translations'
      Anchors = [akLeft, akTop, akRight]
      ParentShowHint = False
      PopupMenu = pmFileList
      ShowHint = True
      Sorted = True
      TabOrder = 3
      OnCloseUp = cbCompCloseUp
    end
    object bbEditFile: TJrButton
      Left = 315
      Top = 22
      Width = 31
      Height = 31
      Hint = 'Open po file'
      Images = imlGlyphs
      ImageIndex = 2
      Layout = blGlyphLeft
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = bbEditFileClick
    end
    object bbComp: TJrButton
      Left = 725
      Top = 22
      Width = 31
      Height = 31
      Hint = 'Open po file'
      Anchors = [akTop, akRight]
      Images = imlGlyphs
      ImageIndex = 2
      Layout = blGlyphLeft
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
      OnClick = bbCompClick
    end
    object bbCopyName: TJrButton
      Left = 350
      Top = 22
      Width = 31
      Height = 31
      Hint = 'Copy filepath to right'
      Images = imlGlyphs
      ImageIndex = 3
      Layout = blGlyphLeft
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnClick = bbCopyNameClick
    end
    object bbSaveChanges: TJrButton
      Tag = 1
      Left = 340
      Top = 225
      Width = 31
      Height = 31
      Hint = 'Save changes'
      Anchors = [akLeft, akBottom]
      Images = imlGlyphs
      ImageIndex = 0
      Layout = blGlyphLeft
      ParentShowHint = False
      ShowHint = True
      TabOrder = 17
      OnClick = bbSaveChangesClick
    end
    object bbCopyAll: TJrButton
      Tag = 1
      Left = 390
      Top = 225
      Width = 41
      Height = 31
      Hint = 'Copy all different strings'
      Anchors = [akLeft, akBottom]
      Images = imlGlyphs
      ImageIndex = 11
      Layout = blGlyphLeft
      ParentShowHint = False
      ShowHint = True
      TabOrder = 11
      OnClick = bbCopyAllClick
    end
    object bbCopyId: TJrButton
      Tag = 1
      Left = 435
      Top = 225
      Width = 41
      Height = 31
      Hint = 'Copy selected entry'
      Anchors = [akLeft, akBottom]
      Images = imlGlyphs
      ImageIndex = 8
      Layout = blGlyphLeft
      ParentShowHint = False
      ShowHint = True
      TabOrder = 8
      OnClick = bbCopyIdClick
    end
    object bbDown: TJrButton
      Tag = 1
      Left = 535
      Top = 225
      Width = 31
      Height = 31
      Hint = 'Next entry'
      Anchors = [akLeft, akBottom]
      Images = imlGlyphs
      ImageIndex = 6
      Layout = blGlyphLeft
      ParentShowHint = False
      ShowHint = True
      TabOrder = 9
      OnClick = bbDownClick
    end
    object bbFirst: TJrButton
      Tag = 1
      Left = 720
      Top = 225
      Width = 31
      Height = 31
      Hint = 'First entry'
      Anchors = [akLeft, akBottom]
      Images = imlGlyphs
      ImageIndex = 13
      Layout = blGlyphLeft
      ParentShowHint = False
      ShowHint = True
      TabOrder = 15
      OnClick = bbFirstClick
    end
    object bbLast: TJrButton
      Tag = 1
      Left = 685
      Top = 225
      Width = 31
      Height = 31
      Hint = 'Last entry'
      Anchors = [akLeft, akBottom]
      Images = imlGlyphs
      ImageIndex = 12
      Layout = blGlyphLeft
      ParentShowHint = False
      ShowHint = True
      TabOrder = 14
      OnClick = bbLastClick
    end
    object bbUp: TJrButton
      Tag = 1
      Left = 570
      Top = 225
      Width = 31
      Height = 31
      Hint = 'Previous entry'
      Anchors = [akLeft, akBottom]
      Images = imlGlyphs
      ImageIndex = 7
      Layout = blGlyphLeft
      ParentShowHint = False
      ShowHint = True
      TabOrder = 10
      OnClick = bbUpClick
    end
    object bbMUp: TJrButton
      Tag = 1
      Left = 645
      Top = 225
      Width = 31
      Height = 31
      Hint = '10 entries back'
      Anchors = [akLeft, akBottom]
      Images = imlGlyphs
      ImageIndex = 10
      Layout = blGlyphLeft
      ParentShowHint = False
      ShowHint = True
      TabOrder = 13
      OnClick = bbMUpClick
    end
    object bbMDn: TJrButton
      Tag = 1
      Left = 610
      Top = 225
      Width = 31
      Height = 31
      Hint = '10 entries forward'
      Anchors = [akLeft, akBottom]
      Images = imlGlyphs
      ImageIndex = 9
      Layout = blGlyphLeft
      ParentShowHint = False
      ShowHint = True
      TabOrder = 12
      OnClick = bbMDnClick
    end
    object bbUndo: TJrButton
      Tag = 1
      Left = 305
      Top = 225
      Width = 31
      Height = 31
      Hint = 'Undo last change'
      Anchors = [akLeft, akBottom]
      Images = imlGlyphs
      ImageIndex = 5
      Layout = blGlyphLeft
      ParentShowHint = False
      ShowHint = True
      TabOrder = 16
      OnClick = bbUndoClick
    end
    object bbCopyAndNext: TJrButton
      Tag = 1
      Left = 480
      Top = 225
      Width = 41
      Height = 31
      Hint = 'Copy selected entry and go to next'
      Anchors = [akLeft, akBottom]
      Images = imlGlyphs
      ImageIndex = 14
      Layout = blGlyphLeft
      ParentShowHint = False
      ShowHint = True
      TabOrder = 18
      OnClick = bbCopyAndNextClick
    end
    object bbHeader: TJrButton
      Tag = 1
      Left = 10
      Top = 223
      Width = 31
      Height = 31
      Hint = 'Copy header data'
      Anchors = [akLeft, akBottom]
      Images = imlGlyphs
      ImageIndex = 4
      Layout = blGlyphLeft
      ParentShowHint = False
      ShowHint = True
      TabOrder = 19
      OnClick = bbHeaderClick
    end
  end
  object pnTools: TPanel
    Left = 0
    Top = 259
    Width = 760
    Height = 46
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitWidth = 761
    DesignSize = (
      760
      46)
    object bbExit: TJrButton
      Left = 715
      Top = 5
      Width = 36
      Height = 36
      Hint = 'Quit program'
      Anchors = [akTop, akRight]
      Images = imlGlyphs
      ImageIndex = 20
      Layout = blGlyphLeft
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnClick = bbExitClick
    end
    object bbInfo: TJrButton
      Left = 675
      Top = 5
      Width = 36
      Height = 36
      Hint = 'About the program'
      Anchors = [akTop, akRight]
      Images = imlGlyphs
      ImageIndex = 19
      Layout = blGlyphLeft
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = bbInfoClick
    end
    object bbSave: TJrButton
      Left = 335
      Top = 5
      Width = 36
      Height = 36
      Hint = 'Save edited file'
      Images = imlGlyphs
      ImageIndex = 18
      Layout = blGlyphLeft
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnClick = bbSaveClick
    end
    object bbReload: TJrButton
      Tag = 1
      Left = 10
      Top = 5
      Width = 36
      Height = 36
      Hint = 'Reload files'
      Images = imlGlyphs
      ImageIndex = 17
      Layout = blGlyphLeft
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      OnClick = bbReloadClick
    end
    object bbEditPo: TJrButton
      Tag = 1
      Left = 50
      Top = 5
      Width = 36
      Height = 36
      Hint = 'Edit po file'
      Images = imlGlyphs
      ImageIndex = 15
      Layout = blGlyphLeft
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
      OnClick = bbEditPoClick
    end
    object btnHelp: TJrButton
      Left = 635
      Top = 5
      Width = 36
      Height = 36
      Hint = 'Show program help'
      Anchors = [akTop, akRight]
      Images = imlGlyphs
      ImageIndex = 1
      Layout = blGlyphLeft
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
      OnClick = btnHelpClick
    end
  end
  object SaveDialog: TSaveDialog
    Left = 291
    Top = 430
  end
  object OpenDialog: TOpenDialog
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 655
    Top = 55
  end
  object pmFileList: TPopupMenu
    AutoHotkeys = maManual
    OwnerDraw = True
    Left = 588
    Top = 60
    object pmiEdit: TMenuItem
      Caption = 'Edit list'
      OnClick = pmiEditClick
      OnDrawItem = pmiDrawItem
      OnMeasureItem = pmiMeasureItem
    end
    object pmiClear: TMenuItem
      Caption = 'Clear list'
      OnClick = pmiClearClick
      OnDrawItem = pmiDrawItem
      OnMeasureItem = pmiMeasureItem
    end
    object N21: TMenuItem
      Caption = '-'
    end
    object pmiCancel: TMenuItem
      Caption = 'Cancel'
    end
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
        IconName = 'arrow-right-red'
        SVGText = 
          '<svg height="48" viewBox="0 0 48 48" width="48" xmlns="http://ww' +
          'w.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink"><l' +
          'inearGradient id="a" gradientUnits="userSpaceOnUse" x1="32.47497' +
          '6" x2="16.120304" y1="37.413391" y2="6.52634"><stop offset="0" s' +
          'top-color="#e0101b"/><stop offset="1" stop-color="#fc9ba3"/></li' +
          'nearGradient><g stroke-miterlimit="10" transform="matrix(0 1.000' +
          '2613 -1.0002613 0 46.005369 .469809)"><path d="m14.519136 38.5 1' +
          '8.005029-.0039v-12.991632l7.995366-.0078-17.144722-19.9974545-16' +
          '.8462505 19.9980705 7.9958815.0038z" fill="url(#a)" fill-rule="e' +
          'venodd" stroke="#c7000b" stroke-linecap="round" stroke-linejoin=' +
          '"round"/><path d="m15.520704 37.496094 16.001405.003906v-12.9929' +
          '5l6.816811-.015625-14.954276-17.4525854-14.7065267 17.4569424 6.' +
          '8399007.0052z" fill="none" opacity=".481283" stroke="#fff"/></g>' +
          '</svg>'
      end
      item
        IconName = 'collapse'
        SVGText = 
          '<svg height="48" viewBox="0 0 48 48" width="48" xmlns="http://ww' +
          'w.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink"><l' +
          'inearGradient id="a" gradientUnits="userSpaceOnUse" x1="32.47497' +
          '6" x2="16.120304" y1="37.413391" y2="6.52634"><stop offset="0" s' +
          'top-color="#e0101b"/><stop offset="1" stop-color="#fcdb9b"/></li' +
          'nearGradient><linearGradient id="b" gradientTransform="matrix(1.' +
          '1315337 0 0 1.1248055 -3.157039 -47.492996)" gradientUnits="user' +
          'SpaceOnUse" x1="21.691689" x2="21.549656" y1="30.958708" y2="41.' +
          '088745"><stop offset="0" stop-color="#458ad0"/><stop offset="1" ' +
          'stop-color="#aecdeb"/></linearGradient><g stroke-miterlimit="10"' +
          ' transform="matrix(1.0002613 0 0 1.0002613 .469809 7.999453)"><p' +
          'ath d="m14.519136 38.5 18.005029-.0039v-12.991632l7.995366-.0078' +
          '-17.144722-19.9974545-16.8462505 19.9980705 7.9958815.0038z" fil' +
          'l="url(#a)" fill-rule="evenodd" stroke="#c00" stroke-linecap="ro' +
          'und" stroke-linejoin="round"/><path d="m15.520704 37.496094 16.0' +
          '01405.003906v-12.99295l6.816811-.015625-14.954276-17.4525854-14.' +
          '7065267 17.4569424 6.8399007.0052z" fill="none" opacity=".481283' +
          '" stroke="#fff"/></g><rect height="8.945099" rx=".725868" style=' +
          '"stroke:#2e73b8;stroke-width:1.0549;stroke-linecap:round;stroke-' +
          'linejoin:round;stroke-dashoffset:1.18155;fill:url(#b)" transform' +
          '="scale(1 -1)" width="42.945" x="2.527453" y="-11.472551"/><path' +
          ' d="m3.589468 3.589468h40.821064v6.821076h-40.821064z" fill="non' +
          'e" opacity=".481283" stroke="#fff" stroke-linecap="square" strok' +
          'e-miterlimit="10"/></svg>'
      end
      item
        IconName = 'undo'
        SVGText = 
          '<svg height="48" viewBox="0 0 48 48" width="48" xmlns="http://ww' +
          'w.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink"><l' +
          'inearGradient id="a" gradientTransform="matrix(.6931485 0 0 -.68' +
          '47878 11.470083 38.638069)" gradientUnits="userSpaceOnUse" x1="2' +
          '1.244122" x2="37.327244" y1="23.389814" y2="20.941328"><stop off' +
          'set="0" stop-color="#17cc53"/><stop offset="1" stop-color="#0096' +
          '32"/></linearGradient><g transform="matrix(1.2928082 0 0 1.29280' +
          '82 -14.806552 -10.219029)"><path d="m24.5 13.5-11 10 11 10.46875' +
          '.03125-6.46875h9.375c4.644932-.191207 7.691828 2.156247 7.5625 5' +
          'v.125 6.8125c10.220318-7.052696 3.633048-20.077177-7.5625-19.968' +
          '75l-9.40625.0625z" display="block" fill="url(#a)" stroke="#00802' +
          'b" stroke-linejoin="round" stroke-width=".999975"/><path d="m23.' +
          '46875 15.8125-8.46875 7.6875 8.46875 8.0625.03125-4.0625c.000328' +
          '-.569408.461842-1.030922 1.03125-1.03125h9.375c2.505095-.103121 ' +
          '4.601649.454623 6.15625 1.53125 1.545505 1.070327 2.506852 2.710' +
          '188 2.4375 4.5-.000408.01053.00048.02071 0 .03125v.09375 4.4375c' +
          '3.124654-3.062309 3.650684-6.681869 2.28125-9.875-1.598769-3.727' +
          '873-5.679225-6.73782-10.875-6.6875l-9.40625.0625c-.569408-.00032' +
          '8-1.030922-.461842-1.03125-1.03125z" display="block" fill="none"' +
          ' opacity=".50298" stroke="#fff" stroke-width="1.0815"/></g></svg' +
          '>'
      end
      item
        IconName = 'b-down'
        SVGText = 
          '<svg height="48" viewBox="0 0 48 48" width="48" xmlns="http://ww' +
          'w.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink"><l' +
          'inearGradient id="a" gradientUnits="userSpaceOnUse" x1=".986122"' +
          ' x2="28.027756" y1="10" y2="10"><stop offset="0" stop-color="#99' +
          'bfec" stop-opacity=".99537"/><stop offset="1" stop-color="#2574c' +
          'a"/></linearGradient><path d="m.98612217 25.612494-.00000008-31.' +
          '2249884 27.04163391 15.6124944z" style="stroke:#2a6cbf;stroke-wi' +
          'dth:1.91213;stroke-linecap:round;stroke-linejoin:round;stroke-da' +
          'shoffset:1.33;fill:url(#a)" transform="matrix(0 .89553343371 -1.' +
          '20370238614 0 36.03702361713 11.00859398808)"/></svg>'
      end
      item
        IconName = 'b-up'
        SVGText = 
          '<svg height="48" viewBox="0 0 48 48" width="48" xmlns="http://ww' +
          'w.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink"><l' +
          'inearGradient id="a" gradientUnits="userSpaceOnUse" x1=".986122"' +
          ' x2="28.027756" y1="10" y2="10"><stop offset="0" stop-color="#99' +
          'bfec" stop-opacity=".99537"/><stop offset="1" stop-color="#2574c' +
          'a"/></linearGradient><path d="m.98612217 25.612494-.00000008-31.' +
          '2249884 27.04163391 15.6124944z" style="stroke:#2a6cbf;stroke-wi' +
          'dth:1.91213;stroke-linecap:round;stroke-linejoin:round;stroke-da' +
          'shoffset:1.33;fill:url(#a)" transform="matrix(0 -.89553343371 -1' +
          '.20370238614 0 36.03702361713 36.99140630659)"/></svg>'
      end
      item
        IconName = 'b-left'
        SVGText = 
          '<svg height="48" viewBox="0 0 48 48" width="48" xmlns="http://ww' +
          'w.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink"><l' +
          'inearGradient id="a" gradientUnits="userSpaceOnUse" x1=".986122"' +
          ' x2="28.027756" y1="10" y2="10"><stop offset="0" stop-color="#99' +
          'bfec" stop-opacity=".99537"/><stop offset="1" stop-color="#2574c' +
          'a"/></linearGradient><path d="m.98612217 25.612494-.00000008-31.' +
          '2249884 27.04163391 15.6124944z" style="stroke:#2a6cbf;stroke-wi' +
          'dth:1.91213;stroke-linecap:round;stroke-linejoin:round;stroke-da' +
          'shoffset:1.33;fill:url(#a)" transform="matrix(-.89553343045 0 0 ' +
          '-1.20370239785 36.99140597455 36.03702373969)"/></svg>'
      end
      item
        IconName = 'b-down-fast'
        SVGText = 
          '<svg height="48" viewBox="0 0 48 48" width="48" xmlns="http://ww' +
          'w.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink"><l' +
          'inearGradient id="a" gradientUnits="userSpaceOnUse" x1=".986122"' +
          ' x2="28.027756" y1="10" y2="10"><stop offset="0" stop-color="#99' +
          'bfec" stop-opacity=".99537"/><stop offset="1" stop-color="#2574c' +
          'a"/></linearGradient><g style="stroke:#2a6cbf;stroke-width:1.912' +
          '13;stroke-linecap:round;stroke-linejoin:round;stroke-dashoffset:' +
          '1.33;fill:url(#a)"><path d="m.98612217 25.612494-.00000008-31.22' +
          '49884 27.04163391 15.6124944z" transform="matrix(0 .89553343371 ' +
          '-1.20370238614 0 35.40132891883 17.8291856939)"/><path d="m.9861' +
          '2217 25.612494-.00000008-31.2249884 27.04163391 15.6124944z" tra' +
          'nsform="matrix(0 .89553343371 -1.20370238614 0 35.45779861713 4.' +
          '97308293958)"/></g></svg>'
      end
      item
        IconName = 'b-up-fast'
        SVGText = 
          '<svg height="48" viewBox="0 0 48 48" width="48" xmlns="http://ww' +
          'w.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink"><l' +
          'inearGradient id="a" gradientUnits="userSpaceOnUse" x1=".986122"' +
          ' x2="28.027756" y1="10" y2="10"><stop offset="0" stop-color="#99' +
          'bfec" stop-opacity=".99537"/><stop offset="1" stop-color="#2574c' +
          'a"/></linearGradient><g style="stroke:#2a6cbf;stroke-width:1.912' +
          '13;stroke-linecap:round;stroke-linejoin:round;stroke-dashoffset:' +
          '1.33;fill:url(#a)"><path d="m.98612217 25.612494-.00000008-31.22' +
          '49884 27.04163391 15.6124944z" transform="matrix(0 -.89553343371' +
          ' -1.20370238614 0 35.98055391883 29.0997923061)"/><path d="m.986' +
          '12217 25.612494-.00000008-31.2249884 27.04163391 15.6124944z" tr' +
          'ansform="matrix(0 -.89553343371 -1.20370238614 0 36.03702361713 ' +
          '41.95589506042)"/></g></svg>'
      end
      item
        IconName = 'b-left-fast'
        SVGText = 
          '<svg height="48" viewBox="0 0 48 48" width="48" xmlns="http://ww' +
          'w.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink"><l' +
          'inearGradient id="a" gradientUnits="userSpaceOnUse" x1=".986122"' +
          ' x2="28.027756" y1="10" y2="10"><stop offset="0" stop-color="#99' +
          'bfec" stop-opacity=".99537"/><stop offset="1" stop-color="#2574c' +
          'a"/></linearGradient><g style="stroke:#2a6cbf;stroke-width:1.912' +
          '13;stroke-linecap:round;stroke-linejoin:round;stroke-dashoffset:' +
          '1.33;fill:url(#a)"><path d="m.98612217 25.612494-.00000008-31.22' +
          '49884 27.04163391 15.6124944z" transform="matrix(-.89553343371 0' +
          ' 0 -1.20370238614 29.9558953061 36.00855391883)"/><path d="m.986' +
          '12217 25.612494-.00000008-31.2249884 27.04163391 15.6124944z" tr' +
          'ansform="matrix(-.89553343371 0 0 -1.20370238614 42.81199806042 ' +
          '36.06502361713)"/></g></svg>'
      end
      item
        IconName = 'b-down-stop'
        SVGText = 
          '<svg height="48" viewBox="0 0 48 48" width="48" xmlns="http://ww' +
          'w.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink"><l' +
          'inearGradient id="a"><stop offset="0" stop-color="#99bfec" stop-' +
          'opacity=".99537"/><stop offset="1" stop-color="#2574ca"/></linea' +
          'rGradient><linearGradient id="b" gradientUnits="userSpaceOnUse" ' +
          'x1=".986122" x2="28.027756" xlink:href="#a" y1="10" y2="10"/><li' +
          'nearGradient id="c" gradientUnits="userSpaceOnUse" x1="8" x2="44' +
          '" xlink:href="#a" y1="30.825639" y2="43.825639"/><g stroke="#2a6' +
          'cbf" stroke-linecap="round" stroke-linejoin="round"><path d="m.9' +
          '8612217 25.612494-.00000008-31.2249884 27.04163391 15.6124944z" ' +
          'fill="url(#b)" stroke-dashoffset="1.33" stroke-width="1.91213" t' +
          'ransform="matrix(0 .89553343 -1.2037024 0 36.037024 4.973083)"/>' +
          '<rect fill="url(#c)" height="7.952574" ry=".484228" stroke-dasho' +
          'ffset="1.18155" stroke-width="1.6987" width="37.952999" x="5.023' +
          '712" y="32.84935"/></g></svg>'
      end
      item
        IconName = 'b-up-stop'
        SVGText = 
          '<svg height="48" viewBox="0 0 48 48" width="48" xmlns="http://ww' +
          'w.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink"><l' +
          'inearGradient id="a"><stop offset="0" stop-color="#99bfec" stop-' +
          'opacity=".99537"/><stop offset="1" stop-color="#2574ca"/></linea' +
          'rGradient><linearGradient id="b" gradientUnits="userSpaceOnUse" ' +
          'x1=".986122" x2="28.027756" xlink:href="#a" y1="10" y2="10"/><li' +
          'nearGradient id="c" gradientUnits="userSpaceOnUse" x1="-40" x2="' +
          '-4" xlink:href="#a" y1="-15.825635" y2="-2.825635"/><g stroke="#' +
          '2a6cbf" stroke-linecap="round" stroke-linejoin="round"><path d="' +
          'm.98612217 25.612494-.00000008-31.2249884 27.04163391 15.6124944' +
          'z" fill="url(#b)" stroke-dashoffset="1.33" stroke-width="1.91213' +
          '" transform="matrix(0 -.89553343 1.2037024 0 11.962976 41.678191' +
          ')"/><rect fill="url(#c)" height="7.952574" ry=".484228" stroke-d' +
          'ashoffset="1.18155" stroke-width="1.6987" transform="scale(-1)" ' +
          'width="37.952999" x="-42.976288" y="-13.801924"/></g></svg>'
      end
      item
        IconName = 'b-left-down'
        SVGText = 
          '<svg height="48" viewBox="0 0 48 48" width="48" xmlns="http://ww' +
          'w.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink"><l' +
          'inearGradient id="a" gradientUnits="userSpaceOnUse" x1=".986122"' +
          ' x2="28.027756" y1="10" y2="10"><stop offset="0" stop-color="#99' +
          'bfec" stop-opacity=".99537"/><stop offset="1" stop-color="#2574c' +
          'a"/></linearGradient><linearGradient id="b" gradientUnits="userS' +
          'paceOnUse" x1="33.641075" x2="16.120304" y1="73.472824" y2="6.52' +
          '634"><stop offset="0" stop-color="#fc9ba3"/><stop offset="1" sto' +
          'p-color="#e0101b"/></linearGradient><path d="m.98612217 25.61249' +
          '4-.00000008-31.2249884 27.04163391 15.6124944z" style="stroke:#2' +
          'a6cbf;stroke-width:1.91213;stroke-linecap:round;stroke-linejoin:' +
          'round;stroke-dashoffset:1.33;fill:url(#a)" transform="matrix(-.8' +
          '6344845572 0 0 -1.16057641796 26.02595303187 35.60576393926)"/><' +
          'g stroke-miterlimit="10" transform="matrix(-.57157789 0 0 -.5715' +
          '7789 50.445823 48.016613)"><path d="m14.519136 69.510778h18.0050' +
          '29v-44.00631l7.995366-.0078-17.144722-19.9974545-16.8462505 19.9' +
          '980705 7.9958815.0038z" fill="url(#b)" fill-rule="evenodd" strok' +
          'e="#c7000b" stroke-linecap="round" stroke-linejoin="round"/><pat' +
          'h d="m15.520704 68.636007h16.001405v-44.128957l6.816811-.015625-' +
          '14.954276-17.4525854-14.7065267 17.4569424 6.8399007.0052z" fill' +
          '="none" opacity=".481283" stroke="#fff"/></g></svg>'
      end
      item
        IconName = 'doc-app'
        SVGText = 
          '<svg height="48" viewBox="0 0 48 48" width="48" xmlns="http://ww' +
          'w.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink"><r' +
          'adialGradient id="a" cx="31" cy="12" gradientUnits="userSpaceOnU' +
          'se" r="11.125"><stop offset="0" stop-color="#fafcff"/><stop offs' +
          'et="1" stop-color="#80b3ff"/></radialGradient><linearGradient id' +
          '="b" gradientTransform="matrix(.96592585 -.25881896 .25881896 .9' +
          '6592585 -4.11682 6.729649)" gradientUnits="userSpaceOnUse" x1="2' +
          '8.357094" x2="17.732124" y1="22.794661" y2="5.187518"><stop offs' +
          'et="0" stop-color="#fff"/><stop offset="1" stop-color="#fff" sto' +
          'p-opacity="0"/></linearGradient><rect fill="#ececec" height="41.' +
          '666668" rx="2.5" stroke="#7a7c77" stroke-miterlimit="10.433" str' +
          'oke-width="1.66667" width="38.333336" x="4.833332" y="3.166666"/' +
          '><rect fill="none" height="38.511051" rx=".863101" stroke="#fff"' +
          ' stroke-miterlimit="10.433" stroke-width="1.53495" width="35.178' +
          '051" x="6.367477" y="4.786501"/><g fill="#91938e"><path d="m10.0' +
          '00001 8h28v4h-28z"/><path d="m10 14.999999h28v4h-28z"/><path d="' +
          'm10 21.999998h28v4h-28z"/><path d="m10 28.999998h28v4h-28z"/><pa' +
          'th d="m10 36h20v4h-20z"/></g><g transform="matrix(1.0396796 .278' +
          '58121 -.27858121 1.0396796 5.112909 -7.11217)"><path d="m31 .875' +
          'c-.32102.00000002-.583975.0443823-.8125.0625l-.5.03125-.03125.5-' +
          '.1875 2.65625c-1.060092.2090559-2.041676.6405265-2.90625 1.21875' +
          'l-2.40625-2.03125-1.84375 1.84375 2.03125 2.40625c-.578224.86457' +
          '3-1.009695 1.846158-1.21875 2.90625l-2.65625.1875-.5.03125-.0312' +
          '5.5c-.018119.228528-.0625.491482-.0625.8125 0 .321017.04438.5839' +
          '73.0625.8125l.03125.5.5.03125 2.65625.1875c.209056 1.060092.6405' +
          '26 2.041676 1.21875 2.90625l-1.71875 2.03125-.3125.375.3125.375c' +
          '.35567.411929.744319.80058 1.15625 1.15625l.375.3125.375-.3125 2' +
          '.03125-1.71875c.864573.578224 1.846158 1.009695 2.90625 1.21875l' +
          '.1875 2.65625.03125.5.5.03125c.228528.01812.491482.0625.8125.062' +
          '5.321018.000001.583973-.04438.8125-.0625l.5-.03125.03125-.5.1875' +
          '-2.65625c1.060091-.209056 2.041676-.640526 2.90625-1.21875l2.031' +
          '25 1.71875.375.3125.375-.3125c.411933-.355671.80058-.74432 1.156' +
          '25-1.15625l.3125-.375-.3125-.375-1.71875-2.03125c.578224-.864573' +
          ' 1.009695-1.846158 1.21875-2.90625l2.65625-.1875.5-.03125.03125-' +
          '.5c.018117-.228525.0625-.491482.0625-.8125-.000001-.321015-.0443' +
          '8-.583972-.0625-.8125l-.03125-.5-.5-.03125-2.65625-.1875c-.20905' +
          '5-1.0600925-.640526-2.0416762-1.21875-2.90625l1.71875-2.03125.31' +
          '25-.375-.3125-.375c-.355668-.4119287-.744319-.8005796-1.15625-1.' +
          '15625l-.375-.3125-.375.3125-2.03125 1.71875c-.864573-.5782237-1.' +
          '846158-1.0096946-2.90625-1.21875l-.1875-2.65625-.03125-.5-.5-.03' +
          '125c-.228527-.01811779-.491484-.06249877-.8125-.0625zm.505086 6.' +
          '6894357c1.932001-.0000024 3.72824 2.7516393 3.728242 4.6836403.0' +
          '00002 1.932002-2.579768 4.037843-4.511769 4.037845-1.932002.0000' +
          '02-3.971157-2.330894-3.971159-4.262895-.000001-1.932002 2.822685' +
          '-4.4585881 4.754686-4.4585903z" fill="url(#a)"/><g fill="none"><' +
          'circle cx="23.5" cy="19" r="8.5" stroke="url(#b)" stroke-linecap' +
          '="square" stroke-width="2.42857" transform="matrix(.411765 -.000' +
          '00048 .00000048 .411765 21.323532 4.176472)"/><g stroke="#2a65bd' +
          '" stroke-width="2" transform="matrix(.5295138 -.14188275 .141882' +
          '75 .5295138 -236.54083 -223.97618)"><path d="m352.23859 522.6190' +
          '6 3.17072 4.68566 5.19675-.67311 1.8461-5.34796 5.83668 1.56393-' +
          '1.07521 5.55452 4.16396 3.18131 5.08875-2.47254 3.02317 5.23628-' +
          '4.68565 3.17072.6731 5.19675 5.34797 1.8461-1.56394 5.83668-5.55' +
          '452-1.07521-3.1813 4.16396 2.47254 5.08875-5.23629 3.02317-3.170' +
          '71-4.68566s-5.19675.67311-5.19675.67311l-1.8461 5.34797-5.83668-' +
          '1.56394 1.07521-5.55452-4.16397-3.1813-5.08874 2.47254-3.02317-5' +
          '.23629 4.68565-3.17071-.67311-5.19675-5.34796-1.8461 1.56393-5.8' +
          '3669 5.55452 1.07522 3.18131-4.16397-2.47254-5.08874z"/><ellipse' +
          ' cx="360" cy="542.10858" rx="8.040032" ry="8.097341"/></g></g></' +
          'g></svg>'
      end
      item
        IconName = 'doc-edit'
        SVGText = 
          '<svg height="48" viewBox="0 0 48 48" width="48" xmlns="http://ww' +
          'w.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink"><l' +
          'inearGradient id="a" gradientUnits="userSpaceOnUse" x1="34.47094' +
          '7" x2="36.824364" y1="24.522668" y2="26.876085"><stop offset="0"' +
          ' stop-color="#fcaf3e"/><stop offset="1" stop-color="#ce5c00"/></' +
          'linearGradient><linearGradient id="b" gradientUnits="userSpaceOn' +
          'Use" x1="29.840059" x2="29.202465" y1="33.665848" y2="34.303443"' +
          '><stop offset="0" stop-color="#ce5c00"/><stop offset="1" stop-co' +
          'lor="#ce5c00"/></linearGradient><linearGradient id="c" gradientU' +
          'nits="userSpaceOnUse" x1="26.379272" x2="25.485056" y1="34.38983' +
          '9" y2="32.714375"><stop offset="0" stop-color="#e9b96e"/><stop o' +
          'ffset="1" stop-color="#fff"/></linearGradient><radialGradient id' +
          '="d" cx="-138.83727" cy="128.00087" gradientTransform="matrix(.3' +
          '20394 -.3203948 .322414 .3224148 50.518433 -74.157887)" gradient' +
          'Units="userSpaceOnUse" r="9.126702"><stop offset="0" stop-color=' +
          '"#f9a9a9"/><stop offset="1" stop-color="#ab5f5f"/></radialGradie' +
          'nt><linearGradient id="e" gradientTransform="matrix(.1892115 -.1' +
          '892253 .1892115 .1892253 49.738733 -37.732231)" gradientUnits="u' +
          'serSpaceOnUse" x1="-158.75" x2="-158.75" y1="115.93846" y2="134.' +
          '25"><stop offset="0" stop-color="#ddd"/><stop offset=".34467545"' +
          ' stop-color="#fff"/><stop offset=".72694808" stop-color="#737373' +
          '"/><stop offset="1" stop-color="#bbb"/></linearGradient><rect fi' +
          'll="#ececec" height="41.666668" rx="2.5" stroke="#7a7c77" stroke' +
          '-miterlimit="10.433" stroke-width="1.66667" width="38.333336" x=' +
          '"4.833332" y="3.166666"/><rect fill="none" height="38.511051" rx' +
          '=".863101" stroke="#fff" stroke-miterlimit="10.433" stroke-width' +
          '="1.53495" width="35.178051" x="6.367477" y="4.786501"/><g fill=' +
          '"#91938e"><path d="m10.000001 8h28v4h-28z"/><path d="m10 14.9999' +
          '99h28v4h-28z"/><path d="m10 21.999998h28v4h-28z"/><path d="m10 2' +
          '8.999998h28v4h-28z"/><path d="m10 36h20v4h-20z"/></g><g enable-b' +
          'ackground="new" transform="matrix(1.22195 0 0 1.2219395 -13.1641' +
          '15 2.562108)"><g><path d="m25.89225 30.18459 19-19c2.175049.3599' +
          '61 3.084719 1.732225 3.5 3.5l-19 19-4.616117.704505z" fill="url(' +
          '#a)" fill-rule="evenodd" stroke="url(#b)" stroke-linejoin="round' +
          '"/><path d="m26.792248 30.68459 18.49775-18.397748c1.089745.1784' +
          '35 1.517261.987944 2 2l-18.397748 18.49775-3.300003.900001z" fil' +
          'l="none" opacity=".282353" stroke="#fff"/><g><path d="m24.549577' +
          ' 34.633026 1.666322-4.180309s1.199535.24536 1.932177.975089.9983' +
          '91 1.943828.998391 1.943828z" fill="url(#c)" fill-rule="evenodd"' +
          '/><path d="m23 21.5-5.5 1.5 2-5" fill="none" stroke="#e9b96e" st' +
          'roke-linecap="round" stroke-linejoin="round" transform="translat' +
          'e(6.39225 12.18459)"/><path d="m23.95475 33.68459-.90625 2.25 2.' +
          '34375-.65625c.002-.03184 0-.06141 0-.09375 0-.802125-.645308-1.4' +
          '59801-1.4375-1.5z" fill-rule="evenodd"/></g></g><path d="m42.821' +
          '682 13.147263c1.834152-.500531 3.885052 1.651475 3.449911 3.4499' +
          '19l2.195397-2.195402c1.066511-2.466563-1.132252-4.4097129-3.4947' +
          '14-3.494723z" fill="url(#d)" stroke="#ef2929"/><path d="m40.5619' +
          '76 15.25084c1.936448-.528484 4.101732 1.743706 3.642321 3.642586' +
          'l2.317841-2.31801c.754457-1.595776-2.044633-4.337495-3.689625-3.' +
          '689891z" fill="url(#e)" stroke="#888a85"/></g></svg>'
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
      end>
    Left = 510
    Top = 60
  end
end
