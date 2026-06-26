object ShowMsgDialog: TShowMsgDialog
  Left = 349
  Top = 341
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  ClientHeight = 131
  ClientWidth = 543
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
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  DesignSize = (
    543
    131)
  PixelsPerInch = 96
  TextHeight = 13
  object imgIcon: TSVGIconImage
    Left = 5
    Top = 10
    Width = 36
    Height = 36
    AutoSize = True
    Center = False
    ImageList = imlIcons
    ImageIndex = 1
    Proportional = False
    Stretch = False
  end
  object stInfo: TStaticText
    Left = 50
    Top = 15
    Width = 486
    Height = 66
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    TabStop = True
  end
  object btClose: TJrButton
    Left = 427
    Top = 90
    Width = 112
    Height = 36
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    Images = imlGlyphs
    ImageIndex = 1
    Layout = blGlyphLeft
    ModalResult = 2
    TabOrder = 1
    WordWrap = True
  end
  object btCenter: TJrButton
    Left = 337
    Top = 90
    Width = 87
    Height = 36
    Anchors = [akRight, akBottom]
    Caption = '&Yes'
    Default = True
    Images = imlGlyphs
    ImageIndex = 2
    Layout = blGlyphLeft
    ModalResult = 1
    TabOrder = 2
    WordWrap = True
  end
  object btLeft: TJrButton
    Left = 247
    Top = 90
    Width = 87
    Height = 36
    Anchors = [akRight, akBottom]
    Caption = 'No'
    Images = imlGlyphs
    ImageIndex = 0
    Layout = blGlyphLeft
    ModalResult = 1
    TabOrder = 3
    WordWrap = True
  end
  object Timer: TTimer
    Enabled = False
    Interval = 5000
    OnTimer = TimerTimer
    Left = 255
    Top = 10
  end
  object imlGlyphs: TSVGIconImageList
    ColorDepth = cdDeviceDependent
    Size = 24
    SVGIconItems = <
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
        IconName = 'error'
        SVGText = 
          '<svg height="48" width="48" xmlns="http://www.w3.org/2000/svg" x' +
          'mlns:xlink="http://www.w3.org/1999/xlink"><linearGradient id="a"' +
          ' gradientTransform="matrix(1.0805272 0 0 1.0805026 -1.508286 -3.' +
          '350759)" gradientUnits="userSpaceOnUse" x1="15.046636" x2="15.04' +
          '6636" y1="44.787998" y2="3.885126"><stop offset="0" stop-color="' +
          '#a40000"/><stop offset="1" stop-color="#c22f2f"/></linearGradien' +
          't><radialGradient id="b" cx="62.202274" cy="-5.713216" gradientT' +
          'ransform="matrix(-7.565785 -.00000062 .0000004 -4.8230546 494.60' +
          '904 -26.555114)" gradientUnits="userSpaceOnUse" r="9.755284"><st' +
          'op offset="0" stop-color="#e78181"/><stop offset=".25288007" sto' +
          'p-color="#e15f5f"/><stop offset=".68271071" stop-color="#c22f2f"' +
          '/><stop offset="1" stop-color="#a40000"/></radialGradient><path ' +
          'd="m45.499979 22.999239c0 11.874491-9.626578 21.500741-21.499708' +
          ' 21.500741-11.874218 0-21.5002507-9.626359-21.5002507-21.500741 ' +
          '0-11.873949 9.6260327-21.4992185 21.5002507-21.4992185 11.87313 ' +
          '0 21.499708 9.6252695 21.499708 21.4992185z" fill="url(#b)" stro' +
          'ke="url(#a)" stroke-width="1.000041"/><path d="m44.49904 22.9992' +
          '72c0 11.32219-9.178617 20.500703-20.499249 20.500703-11.321667 0' +
          '-20.499766-9.178619-20.499766-20.500703 0-11.321666 9.178099-20.' +
          '4992465 20.499766-20.4992465 11.320632 0 20.499249 9.1775805 20.' +
          '499249 20.4992465z" fill="none" opacity=".6" stroke="#fff" strok' +
          'e-opacity=".75" stroke-width="1.00005"/><path d="m16.471763 12-3' +
          '.471761 3.471761 7.162792 7.126246c.220049.222824.220049.581162 ' +
          '0 .803987l-7.162792 7.126245 3.471761 3.471761 7.126245-7.126246' +
          'c.222824-.22005.581162-.22005.803987 0l7.126246 7.126246 3.47176' +
          '1-3.471761-7.126246-7.126245c-.220051-.222825-.220051-.581163 0-' +
          '.803987l7.126246-7.126246-3.471761-3.471761-7.126246 7.126245c-.' +
          '222825.220051-.581163.220051-.803987 0z" fill="#fff" fill-rule="' +
          'evenodd"/></svg>'
      end
      item
        IconName = 'arrow-rev-green'
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
      end>
    Left = 305
    Top = 10
  end
  object imlIcons: TSVGIconImageList
    Size = 32
    SVGIconItems = <
      item
        IconName = 'warning'
        SVGText = 
          '<svg height="48" width="48" xmlns="http://www.w3.org/2000/svg"><' +
          'g transform="matrix(1.566667 0 0 1.566667 -8.925566 -23.94764)">' +
          '<g transform="matrix(1 0 .00453785 1 -.138907 0)"><path d="m33.2' +
          '82781 38.644744-10.87499-20.249979c-.312499-.562499-.874999-.874' +
          '998-1.499998-.874998-.625 0-1.187499.374998-1.499998.937498l-10.' +
          '6249902 20.24998c-.25.499999-.25 1.187499.0625 1.687498.312499.5' +
          '.812499.749999 1.4374992.749999h21.499978c.624999 0 1.187498-.31' +
          '2499 1.437499-.812499.312499-.5.312499-1.124999.0625-1.687499z" ' +
          'fill="#c00" stroke="#9f0000" stroke-width=".638298" transform="m' +
          'atrix(1 0 -.00872668 1 .328074 1.276596)"/><path d="m9.5 37.6c-.' +
          '3.5 0 .9.5.9h28.2c.5 0 .8-.4.5-.9l-14.3-26.6c-.3-.5-.7-.5-.9 0z"' +
          ' fill="#fff" transform="matrix(.625 0 -.00553493 .634254 6.16405' +
          '3 15.76055)"/><path d="m32.323106 38.183905-10.172835-18.918239c' +
          '-.433291-.814976-.588573-1.076453-1.241865-1.076453-.561881 0-.8' +
          '54279.380807-1.257101 1.150078l-9.9023765 18.903005c-.5751636 1.' +
          '061292-.6361047 1.337932-.3551641 1.805049.280939.467117.6390326' +
          '.441675 1.9626766.472146h19.618152c1.232232.01523 1.509395-.0786' +
          '5 1.734147-.545771.280941-.467118.159059-.83772-.385634-1.789815' +
          'z" fill="none" opacity=".5" stroke="#f5c8c8" stroke-width=".6382' +
          '98" transform="matrix(1 0 -.00872668 1 .318277 1.276596)"/></g><' +
          'path d="m23.9 36.5c-1.3 0-2.3-1-2.3-2.3 0-1.4.9-2.3 2.3-2.3s2.2.' +
          '9 2.3 2.3c0 1.3-.9 2.3-2.3 2.3zm-1.4-5.9-.6-11.5h4l-.6 11.5h-2.9' +
          'z" transform="matrix(.555088 0 0 .555052 7.749711 17.80196)"/></' +
          'g></svg>'
      end
      item
        IconName = 'error'
        SVGText = 
          '<svg height="48" width="48" xmlns="http://www.w3.org/2000/svg" x' +
          'mlns:xlink="http://www.w3.org/1999/xlink"><linearGradient id="a"' +
          ' gradientUnits="userSpaceOnUse" x1="25.27776876211" x2="24.95671' +
          '959522" y1="-.28571423142" y2="47.88877673545"><stop offset="0" ' +
          'stop-color="#fff"/><stop offset="1" stop-color="#fff" stop-opaci' +
          'ty="0"/></linearGradient><linearGradient id="b" gradientTransfor' +
          'm="matrix(1.0805272 0 0 1.0805026 -1.508286 -3.350759)" gradient' +
          'Units="userSpaceOnUse" x1="15.046636" x2="15.046636" y1="44.7879' +
          '98" y2="3.885126"><stop offset="0" stop-color="#a40000"/><stop o' +
          'ffset="1" stop-color="#c22f2f"/></linearGradient><radialGradient' +
          ' id="c" cx="62.202274" cy="-5.713216" gradientTransform="matrix(' +
          '-7.565785 -.00000062 .0000004 -4.8230546 494.60904 -26.555114)" ' +
          'gradientUnits="userSpaceOnUse" r="9.755284"><stop offset="0" sto' +
          'p-color="#e78181"/><stop offset=".25288007" stop-color="#e15f5f"' +
          '/><stop offset=".68271071" stop-color="#c22f2f"/><stop offset="1' +
          '" stop-color="#a40000"/></radialGradient><path d="m45.499979 22.' +
          '999239c0 11.874491-9.626578 21.500741-21.499708 21.500741-11.874' +
          '218 0-21.5002507-9.626359-21.5002507-21.500741 0-11.873949 9.626' +
          '0327-21.4992185 21.5002507-21.4992185 11.87313 0 21.499708 9.625' +
          '2695 21.499708 21.4992185z" fill="url(#c)" stroke="url(#b)" stro' +
          'ke-width="1.000041"/><path d="m44.49904 22.999272c0 11.32219-9.1' +
          '78617 20.500703-20.499249 20.500703-11.321667 0-20.499766-9.1786' +
          '19-20.499766-20.500703 0-11.321666 9.178099-20.4992465 20.499766' +
          '-20.4992465 11.320632 0 20.499249 9.1775805 20.499249 20.4992465' +
          'z" fill="none" opacity=".6" stroke="url(#a)" stroke-width="1.000' +
          '05"/><path d="m16.471763 12-3.471761 3.471761 7.162792 7.126246c' +
          '.220049.222824.220049.581162 0 .803987l-7.162792 7.126245 3.4717' +
          '61 3.471761 7.126245-7.126246c.222824-.22005.581162-.22005.80398' +
          '7 0l7.126246 7.126246 3.471761-3.471761-7.126246-7.126245c-.2200' +
          '51-.222825-.220051-.581163 0-.803987l7.126246-7.126246-3.471761-' +
          '3.471761-7.126246 7.126245c-.222825.220051-.581163.220051-.80398' +
          '7 0z" fill="#fff" fill-rule="evenodd"/></svg>'
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
        IconName = 'query-green'
        SVGText = 
          '<svg height="48" width="48" xmlns="http://www.w3.org/2000/svg" x' +
          'mlns:xlink="http://www.w3.org/1999/xlink"><linearGradient id="a"' +
          ' gradientUnits="userSpaceOnUse" x1="13.000003" x2="35" y1="3.947' +
          '439" y2="42.052555"><stop offset="0" stop-color="#68cf8a"/><stop' +
          ' offset="1" stop-color="#14632e"/></linearGradient><g transform=' +
          '"translate(0 1)"><path d="m45.499979 22.999239c0 11.874491-9.626' +
          '578 21.500741-21.499708 21.500741-11.874218 0-21.5002507-9.62635' +
          '9-21.5002507-21.500741 0-11.873949 9.6260327-21.4992191 21.50025' +
          '07-21.4992191 11.87313 0 21.499708 9.6252701 21.499708 21.499219' +
          '1z" fill="url(#a)" stroke="#358425" stroke-width="1.00004"/><pat' +
          'h d="m44.49904 22.999272c0 11.32219-9.178617 20.500703-20.499249' +
          ' 20.500703-11.321667 0-20.4997657-9.178619-20.4997657-20.500703 ' +
          '0-11.321666 9.1780987-20.4992471 20.4997657-20.4992471 11.320632' +
          ' 0 20.499249 9.1775811 20.499249 20.4992471z" fill="none" opacit' +
          'y=".500596" stroke="#fff" stroke-width="1.00005"/><path d="m21.0' +
          '46877 29.49609q0-1.567707.41927-2.807289.419271-1.239582 1.02083' +
          '3-2.114582.601562-.874999 1.585936-2.041664.783853-.911458 1.221' +
          '353-1.531249.4375-.63802.729166-1.385416.309896-.765624.309896-1' +
          '.658852v-.01823q0-1.239582-.619792-1.91406-.601562-.692708-1.713' +
          '54-.692708-1.093749 0-1.71354.765624-.601562.747395-.619791 2.11' +
          '4582v.01823h-5.177079v-.01823q.09115-2.406248 1.002603-4.101559.' +
          '911458-1.71354 2.552081-2.606769 1.658853-.893228 3.937497-.8932' +
          '28 2.36979 0 4.065101.802082 1.695311.802083 2.57031 2.333332.89' +
          '3229 1.513019.893229 3.664059v.01823q0 1.403645-.401042 2.533852' +
          '-.382812 1.130208-.966145 1.986978-.565104.838541-1.513019 1.950' +
          '519-.893229 1.07552-1.403645 1.804686-.492187.729166-.838541 1.6' +
          '95311-.346354.947916-.346354 2.096352zm.01823 2.898435h4.994787v' +
          '4.994788h-4.994787z" fill="#fff" transform="translate(0 -1)"/></' +
          'g></svg>'
      end>
    Left = 195
    Top = 15
  end
end
