object FileListDialog: TFileListDialog
  Left = 261
  Top = 170
  Caption = 'Dialog'
  ClientHeight = 431
  ClientWidth = 371
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnAfterMonitorDpiChanged = FormAfterMonitorDpiChanged
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object paBottom: TPanel
    Left = 0
    Top = 391
    Width = 371
    Height = 40
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      371
      40)
    object CancelBtn: TJrButton
      Left = 270
      Top = -2
      Width = 96
      Height = 36
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancel'
      Images = imlGlyphs
      ImageIndex = 1
      Layout = blGlyphLeft
      ModalResult = 2
      TabOrder = 0
    end
    object OKBtn: TJrButton
      Left = 185
      Top = -2
      Width = 81
      Height = 36
      Anchors = [akRight, akBottom]
      Caption = 'OK'
      Images = imlGlyphs
      ImageIndex = 0
      Layout = blGlyphLeft
      ModalResult = 1
      TabOrder = 1
    end
  end
  object paTop: TPanel
    Left = 0
    Top = 0
    Width = 371
    Height = 26
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object laName: TLabel
      Left = 10
      Top = 8
      Width = 31
      Height = 13
      Caption = 'Name:'
    end
    object lblListfile: TLabel
      Left = 50
      Top = 8
      Width = 3
      Height = 13
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
  end
  object paCenter: TPanel
    Left = 0
    Top = 26
    Width = 371
    Height = 365
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      371
      365)
    object Label2: TLabel
      Left = 10
      Top = 38
      Width = 55
      Height = 13
      Caption = 'List of files:'
    end
    object Label3: TLabel
      Left = 10
      Top = 5
      Width = 70
      Height = 13
      Caption = 'Common path:'
    end
    object lblPath: TLabel
      Left = 20
      Top = 22
      Width = 3
      Height = 13
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object btSaveAs: TJrSpeedButton
      Left = 333
      Top = 326
      Width = 31
      Height = 31
      Hint = 'Save list as new file'
      Anchors = [akRight, akBottom]
      Images = imlGlyphs
      ImageIndex = 7
      Layout = blGlyphLeft
      ParentShowHint = False
      ShowHint = True
      OnClick = btSaveAsClick
    end
    object DeleteBtn: TJrSpeedButton
      Left = 333
      Top = 165
      Width = 31
      Height = 31
      Hint = 'Remove file'
      Anchors = [akTop, akRight]
      Images = imlGlyphs
      ImageIndex = 6
      Layout = blGlyphLeft
      ParentShowHint = False
      ShowHint = True
      OnClick = DeleteBtnClick
    end
    object DownBtn: TJrSpeedButton
      Left = 333
      Top = 90
      Width = 31
      Height = 31
      Hint = 'Move file down'
      Anchors = [akTop, akRight]
      Images = imlGlyphs
      ImageIndex = 3
      Layout = blGlyphLeft
      ParentShowHint = False
      ShowHint = True
      OnClick = DownBtnClick
    end
    object InsertBtn: TJrSpeedButton
      Left = 333
      Top = 130
      Width = 31
      Height = 31
      Hint = 'Add files'
      Anchors = [akTop, akRight]
      Images = imlGlyphs
      ImageIndex = 5
      Layout = blGlyphLeft
      ParentShowHint = False
      ShowHint = True
      OnClick = InsertBtnClick
    end
    object UpBtn: TJrSpeedButton
      Left = 333
      Top = 55
      Width = 31
      Height = 31
      Hint = 'Move file up'
      Anchors = [akTop, akRight]
      Images = imlGlyphs
      ImageIndex = 4
      Layout = blGlyphLeft
      ParentShowHint = False
      ShowHint = True
      OnClick = UpBtnClick
    end
    object lbxFiles: TListBox
      Left = 10
      Top = 55
      Width = 314
      Height = 301
      AutoComplete = False
      Anchors = [akLeft, akTop, akRight, akBottom]
      ItemHeight = 13
      TabOrder = 0
    end
  end
  object OpenDialog: TOpenDialog
    Options = [ofAllowMultiSelect, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 65
    Top = 145
  end
  object SaveDialog: TSaveDialog
    Left = 60
    Top = 190
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
        IconName = 'list-add'
        SVGText = 
          '<svg height="48" width="48" xmlns="http://www.w3.org/2000/svg" x' +
          'mlns:xlink="http://www.w3.org/1999/xlink"><linearGradient id="a"' +
          ' gradientUnits="userSpaceOnUse" x1="16.475557" x2="31.524443" y1' +
          '="2.326762" y2="43.673237"><stop offset="0" stop-color="#68cf8b"' +
          '/><stop offset="1" stop-color="#14612e"/></linearGradient><rect ' +
          'fill="#fff" height="41.66666744204" rx="2.49999980091" stroke="#' +
          '7a7c77" stroke-miterlimit="10.433" stroke-width="1.66666571523" ' +
          'width="38.33333355545" x="4.83333242904" y="3.16666710887"/><g f' +
          'ill="#91938e"><path d="m15.00000041473 7.50000140705h24.00000005' +
          '363v3.99999919023h-24.00000005363z"/><path d="m9.00000040132 7.5' +
          '0000140705h3.99999919023v3.99999919023h-3.99999919023z"/><path d' +
          '="m15.00000041473 15.00000080977h24.00000005363v3.99999919023h-2' +
          '4.00000005363z"/><path d="m9.00000040132 15.00000080977h3.999999' +
          '19023v3.99999919023h-3.99999919023z"/><path d="m15.00000041473 2' +
          '2.00000123477h24.00000005363v3.99999919023h-24.00000005363z"/><p' +
          'ath d="m9.00000040132 22.00000123477h3.99999919023v3.99999919023' +
          'h-3.99999919023z"/><path d="m15.00000041473 28.99999920363h24.00' +
          '000005363v3.99999919023h-24.00000005363z"/><path d="m9.000000401' +
          '32 28.99999920363h3.99999919023v3.99999919023h-3.99999919023z"/>' +
          '<path d="m15.00000041473 35.99999962863h24.00000005363v3.9999991' +
          '9023h-24.00000005363z"/><path d="m9.00000040132 35.99999962863h3' +
          '.99999919023v3.99999919023h-3.99999919023z"/></g><g transform="m' +
          'atrix(.59090911 0 0 .5909091 19.818182 .409091)"><path d="m45.49' +
          '9979 22.999239c0 11.874491-9.626578 21.500741-21.499708 21.50074' +
          '1-11.874218 0-21.5002507-9.626359-21.5002507-21.500741 0-11.8739' +
          '49 9.6260327-21.4992185 21.5002507-21.4992185 11.87313 0 21.4997' +
          '08 9.6252695 21.499708 21.4992185z" fill="url(#a)" stroke="#0080' +
          '2a" stroke-width="1.00004"/><path d="m44.49904 22.999272c0 11.32' +
          '219-9.178617 20.500703-20.499249 20.500703-11.321667 0-20.499766' +
          '-9.178619-20.499766-20.500703 0-11.321666 9.178099-20.4992465 20' +
          '.499766-20.4992465 11.320632 0 20.499249 9.1775805 20.499249 20.' +
          '4992465z" fill="none" opacity=".488677" stroke="#fff" stroke-wid' +
          'th="1.00005"/><path d="m16.928933 10.27208-5.656854 5.656854 6.3' +
          '63961 6.363961c.707107.707107.707107.707107 0 1.414213l-6.363961' +
          ' 6.363961 5.656854 5.656855 6.363961-6.363961c.707107-.707107.70' +
          '7107-.707107 1.414214 0l6.363961 6.363961 5.656854-5.656855-6.36' +
          '3961-6.363961c-.707107-.707106-.698038-.683703 0-1.414213l6.3639' +
          '61-6.363961-5.656854-5.656854-6.363961 6.363961c-.707107.707106-' +
          '.707107.707106-1.414214 0z" fill="#fff" fill-rule="evenodd" tran' +
          'sform="matrix(.70710678 .70710678 -.70710678 .70710678 23.292894' +
          ' -10.23402)"/></g></svg>'
      end
      item
        IconName = 'list-rem'
        SVGText = 
          '<svg height="48" width="48" xmlns="http://www.w3.org/2000/svg" x' +
          'mlns:xlink="http://www.w3.org/1999/xlink"><radialGradient id="a"' +
          ' cx="62.202274" cy="-5.713216" gradientTransform="matrix(-6.5311' +
          '785 3.8190065 -2.1525833 -3.6812995 417.95599 -257.58295)" gradi' +
          'entUnits="userSpaceOnUse" r="9.755283"><stop offset="0" stop-col' +
          'or="#e87d7d"/><stop offset="1" stop-color="#b82f25"/></radialGra' +
          'dient><linearGradient id="b" gradientTransform="matrix(1.0805272' +
          ' 0 0 1.0805026 -1.508286 -3.35076)" gradientUnits="userSpaceOnUs' +
          'e" x1="3.633849" x2="15.046636" y1="11.439008" y2="3.885126"><st' +
          'op offset="0" stop-color="#6e1c16"/><stop offset="1" stop-color=' +
          '"#932f29"/></linearGradient><linearGradient id="c" gradientUnits' +
          '="userSpaceOnUse" x1="25.27776876211" x2="24.95671959522" y1="-.' +
          '28571423142" y2="47.88877673545"><stop offset="0" stop-color="#e' +
          '0e0e0"/><stop offset="1" stop-color="#e0e0e0" stop-opacity="0"/>' +
          '</linearGradient><rect fill="#fff" height="41.66666744204" rx="2' +
          '.49999980091" stroke="#7a7c77" stroke-miterlimit="10.433" stroke' +
          '-width="1.66666571523" width="38.33333355545" x="4.83333242904" ' +
          'y="3.16666710887"/><g fill="#91938e"><path d="m15.00000041473 7.' +
          '50000140705h24.00000005363v3.99999919023h-24.00000005363z"/><pat' +
          'h d="m9.00000040132 7.50000140705h3.99999919023v3.99999919023h-3' +
          '.99999919023z"/><path d="m15.00000041473 15.00000080977h24.00000' +
          '005363v3.99999919023h-24.00000005363z"/><path d="m9.00000040132 ' +
          '15.00000080977h3.99999919023v3.99999919023h-3.99999919023z"/><pa' +
          'th d="m15.00000041473 22.00000123477h24.00000005363v3.9999991902' +
          '3h-24.00000005363z"/><path d="m9.00000040132 22.00000123477h3.99' +
          '999919023v3.99999919023h-3.99999919023z"/><path d="m15.000000414' +
          '73 28.99999920363h24.00000005363v3.99999919023h-24.00000005363z"' +
          '/><path d="m9.00000040132 28.99999920363h3.99999919023v3.9999991' +
          '9023h-3.99999919023z"/><path d="m15.00000041473 35.99999962863h2' +
          '4.00000005363v3.99999919023h-24.00000005363z"/><path d="m9.00000' +
          '040132 35.99999962863h3.99999919023v3.99999919023h-3.99999919023' +
          'z"/></g><g transform="matrix(.56818183 0 0 .56818182 20.863636 .' +
          '431818)"><path d="m45.499979 22.999239c0 11.874491-9.626578 21.5' +
          '00741-21.499708 21.500741-11.874218 0-21.5002507-9.626359-21.500' +
          '2507-21.500741 0-11.873949 9.6260327-21.4992185 21.5002507-21.49' +
          '92185 11.87313 0 21.499708 9.6252695 21.499708 21.4992185z" fill' +
          '="url(#a)" stroke="url(#b)" stroke-width="1.00004"/><path d="m44' +
          '.49904 22.999272c0 11.32219-9.178617 20.500703-20.499249 20.5007' +
          '03-11.321667 0-20.499766-9.178619-20.499766-20.500703 0-11.32166' +
          '6 9.178099-20.4992465 20.499766-20.4992465 11.320632 0 20.499249' +
          ' 9.1775805 20.499249 20.4992465z" fill="none" opacity=".6" strok' +
          'e="url(#c)" stroke-width="1.00005"/><path d="m10.938356 19.83985' +
          '7h26.123287v6.320287h-26.123287z" style="fill:#fff;stroke:#9b1e1' +
          'a;stroke-width:.679713;stroke-linecap:round;stroke-linejoin:roun' +
          'd;stroke-dashoffset:1.33"/></g></svg>'
      end
      item
        IconName = 'save-as'
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
          'stop-color="#6b6b6b"/></linearGradient><linearGradient id="d" gr' +
          'adientUnits="userSpaceOnUse" x1="16.475557" x2="31.524443" y1="2' +
          '.326762" y2="43.673237"><stop offset="0" stop-color="#68cf8b"/><' +
          'stop offset="1" stop-color="#14612e"/></linearGradient><g transf' +
          'orm="matrix(.83365948 0 0 .85623432 2.50398 7.373433)"><path d="' +
          'm4.5590083 3.5678147h38.9273337c.589855 0 1.064721.4744086 1.064' +
          '721 1.0636961v37.7647652c0 .589288-.474866 1.063696-1.064721 1.0' +
          '63696h-36.9254293s-3.0666258-3.063672-3.0666258-3.063672v-35.764' +
          '7892c0-.5892875.4748658-1.0636961 1.0647214-1.0636961z" fill="ur' +
          'l(#a)" stroke="#25375f" stroke-linecap="round" stroke-linejoin="' +
          'round" stroke-width="1.00047"/><path d="m9 4h30v23h-30z" fill="#' +
          'fff"/><rect fill="#d31c00" height="4" rx=".126208" width="30" x=' +
          '"9" y="4"/><rect height="2" opacity=".738636" rx=".126208" width' +
          '="2" x="6" y="6"/><g stroke="#000"><path d="m11 12.5h26" opacity' +
          '=".130682"/><path d="m11 17.5h26" opacity=".130682"/><path d="m1' +
          '1 22.5h26" opacity=".130682"/></g><path d="m4.6189226 4.5276647h' +
          '38.7684814c.06992 0 .126208.056289.126208.1262077v37.6482386c0 .' +
          '06992-.05629.126208-.126208.126208h-36.4591222s-2.4355669-2.3913' +
          '73-2.4355669-2.391373v-35.3830736c0-.069919.056289-.1262077.1262' +
          '077-.1262077z" fill="none" opacity=".596591" stroke="url(#b)" st' +
          'roke-linecap="round"/><path d="m14.113967 28.562183h19.749824c.8' +
          '87971 0 1.602836.75091 1.602836 1.683653v13.201551h-22.955496v-1' +
          '3.201551c0-.932743.714865-1.683653 1.602836-1.683653z" fill="url' +
          '(#c)" stroke="#525252" stroke-width=".999999"/><rect fill="#4967' +
          'a2" height="10.06597" rx=".751207" ry=".751208" stroke="#525252"' +
          ' width="5.029753" x="16.464279" y="30.4566"/></g><g transform="m' +
          'atrix(.59090911 0 0 .5909091 19.818182 .409091)"><path d="m45.49' +
          '9979 22.999239c0 11.874491-9.626578 21.500741-21.499708 21.50074' +
          '1-11.874218 0-21.5002507-9.626359-21.5002507-21.500741 0-11.8739' +
          '49 9.6260327-21.4992185 21.5002507-21.4992185 11.87313 0 21.4997' +
          '08 9.6252695 21.499708 21.4992185z" fill="url(#d)" stroke="#0080' +
          '2a" stroke-width="1.00004"/><path d="m44.49904 22.999272c0 11.32' +
          '219-9.178617 20.500703-20.499249 20.500703-11.321667 0-20.499766' +
          '-9.178619-20.499766-20.500703 0-11.321666 9.178099-20.4992465 20' +
          '.499766-20.4992465 11.320632 0 20.499249 9.1775805 20.499249 20.' +
          '4992465z" fill="none" opacity=".488677" stroke="#fff" stroke-wid' +
          'th="1.00005"/><path d="m16.928933 10.27208-5.656854 5.656854 6.3' +
          '63961 6.363961c.707107.707107.707107.707107 0 1.414213l-6.363961' +
          ' 6.363961 5.656854 5.656855 6.363961-6.363961c.707107-.707107.70' +
          '7107-.707107 1.414214 0l6.363961 6.363961 5.656854-5.656855-6.36' +
          '3961-6.363961c-.707107-.707106-.698038-.683703 0-1.414213l6.3639' +
          '61-6.363961-5.656854-5.656854-6.363961 6.363961c-.707107.707106-' +
          '.707107.707106-1.414214 0z" fill="#fff" fill-rule="evenodd" stro' +
          'ke="#00802a" stroke-linecap="round" stroke-linejoin="round" tran' +
          'sform="matrix(.70710678 .70710678 -.70710678 .70710678 23.292894' +
          ' -10.23402)"/></g></svg>'
      end>
    Left = 60
    Top = 248
  end
end
