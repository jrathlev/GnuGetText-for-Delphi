object ShowtextDialog: TShowtextDialog
  Left = 302
  Top = 156
  ClientHeight = 442
  ClientWidth = 521
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  OnAfterMonitorDpiChanged = FormAfterMonitorDpiChanged
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnPaint = FormPaint
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 521
    Height = 46
    Align = alTop
    TabOrder = 0
    DesignSize = (
      521
      46)
    object EndeBtn: TJrButton
      Left = 415
      Top = 5
      Width = 101
      Height = 36
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Close'
      Images = imlGlyphs
      ImageIndex = 2
      Layout = blGlyphLeft
      ModalResult = 2
      Spacing = 8
      TabOrder = 0
      OnClick = EndeBtnClick
    end
    object PrintBtn: TJrButton
      Left = 5
      Top = 5
      Width = 91
      Height = 36
      Caption = '&Print'
      Images = imlGlyphs
      ImageIndex = 5
      Layout = blGlyphLeft
      TabOrder = 1
      OnClick = PrintBtnClick
    end
    object DeleteBtn: TJrButton
      Left = 95
      Top = 5
      Width = 91
      Height = 36
      Caption = '&Erase'
      Images = imlGlyphs
      ImageIndex = 3
      Layout = blGlyphLeft
      TabOrder = 2
      OnClick = DeleteBtnClick
    end
    object SearchBtn: TJrButton
      Left = 185
      Top = 5
      Width = 91
      Height = 36
      Caption = '&Search'
      Images = imlGlyphs
      ImageIndex = 4
      Layout = blGlyphLeft
      TabOrder = 3
      OnClick = SearchBtnClick
    end
    object CopyBtn: TJrButton
      Left = 275
      Top = 5
      Width = 91
      Height = 36
      Caption = '&Copy'
      Images = imlGlyphs
      ImageIndex = 6
      Layout = blGlyphLeft
      TabOrder = 4
      OnClick = CopyBtnClick
    end
    object UpdateBtn: TJrButton
      Left = 375
      Top = 5
      Width = 36
      Height = 36
      Hint = 'Update'
      Anchors = [akTop, akRight]
      BiDiMode = bdLeftToRight
      Images = imlGlyphs
      ImageIndex = 7
      Layout = blGlyphLeft
      ParentBiDiMode = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
      OnClick = UpdateBtnClick
    end
  end
  object Memo: TMemo
    Left = 0
    Top = 46
    Width = 521
    Height = 376
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 1
    OnKeyDown = MemoKeyDown
    OnKeyUp = MemoKeyUp
    OnMouseDown = MemoMouseDown
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 422
    Width = 521
    Height = 20
    Panels = <>
    SimplePanel = True
  end
  object PrintDialog: TPrintDialog
    Left = 330
    Top = 50
  end
  object FindDialog: TFindDialog
    OnFind = FindDialogFind
    Left = 375
    Top = 50
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
        IconName = 'doc-delete'
        SVGText = 
          '<svg height="48" viewBox="0 0 48 48" width="48" xmlns="http://ww' +
          'w.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink"><l' +
          'inearGradient id="a" gradientUnits="userSpaceOnUse" x1="13" x2="' +
          '35.000008" y1="3.947442" y2="42.05257"><stop offset="0" stop-col' +
          'or="#e78181"/><stop offset="1" stop-color="#a40000"/></linearGra' +
          'dient><rect fill="#ececec" height="41.666668" rx="2.5" stroke="#' +
          '7a7c77" stroke-miterlimit="10.433" stroke-width="1.66667" width=' +
          '"38.333336" x="4.833332" y="3.166666"/><rect fill="none" height=' +
          '"38.511051" rx=".863101" stroke="#fff" stroke-miterlimit="10.433' +
          '" stroke-width="1.53495" width="35.178051" x="6.367477" y="4.786' +
          '501"/><g fill="#91938e"><path d="m10.000001 8h28v4h-28z"/><path ' +
          'd="m10 14.999999h28v4h-28z"/><path d="m10 21.999998h28v4h-28z"/>' +
          '<path d="m10 28.999998h28v4h-28z"/><path d="m10 36h20v4h-20z"/><' +
          '/g><g transform="matrix(.59090911 0 0 .5909091 19.818182 .409091' +
          ')"><path d="m45.499979 22.999239c0 11.874491-9.626578 21.500741-' +
          '21.499708 21.500741-11.874218 0-21.5002507-9.626359-21.5002507-2' +
          '1.500741 0-11.873949 9.6260327-21.4992185 21.5002507-21.4992185 ' +
          '11.87313 0 21.499708 9.6252695 21.499708 21.4992185z" fill="url(' +
          '#a)" stroke="#b21a1a" stroke-width="1.00004"/><path d="m44.49904' +
          ' 22.999272c0 11.32219-9.178617 20.500703-20.499249 20.500703-11.' +
          '321667 0-20.499766-9.178619-20.499766-20.500703 0-11.321666 9.17' +
          '8099-20.4992465 20.499766-20.4992465 11.320632 0 20.499249 9.177' +
          '5805 20.499249 20.4992465z" fill="none" opacity=".6" stroke="#ff' +
          'f" stroke-opacity=".75" stroke-width="1.00005"/><path d="m16.471' +
          '763 12-3.471761 3.471761 7.162792 7.126246c.220049.222824.220049' +
          '.581162 0 .803987l-7.162792 7.126245 3.471761 3.471761 7.126245-' +
          '7.126246c.222824-.22005.581162-.22005.803987 0l7.126246 7.126246' +
          ' 3.471761-3.471761-7.126246-7.126245c-.220051-.222825-.220051-.5' +
          '81163 0-.803987l7.126246-7.126246-3.471761-3.471761-7.126246 7.1' +
          '26245c-.222825.220051-.581163.220051-.803987 0z" fill="#fff" fil' +
          'l-rule="evenodd"/></g></svg>'
      end
      item
        IconName = 'doc-view'
        SVGText = 
          '<svg height="48" viewBox="0 0 48 48" width="48" xmlns="http://ww' +
          'w.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink"><l' +
          'inearGradient id="a" gradientUnits="userSpaceOnUse" x1="27.36634' +
          '1" x2="31.335964" y1="26.580296" y2="30.557772"><stop offset="0"' +
          ' stop-color="#8a8a8a"/><stop offset="1" stop-color="#484848"/></' +
          'linearGradient><linearGradient id="b" gradientTransform="matrix(' +
          '1.334593 0 0 1.291292 -6.973842 -7.460658)" gradientUnits="userS' +
          'paceOnUse" x1="30.65625" x2="33.21875" y1="34" y2="31.0625"><sto' +
          'p offset="0" stop-color="#d03030"/><stop offset=".5" stop-color=' +
          '"#c93737"/><stop offset="1" stop-color="#e38a8a"/></linearGradie' +
          'nt><linearGradient id="c" gradientUnits="userSpaceOnUse" x1="18.' +
          '292673" x2="17.500893" y1="13.602121" y2="25.743469"><stop offse' +
          't="0" stop-color="#fff"/><stop offset=".5" stop-color="#fff" sto' +
          'p-opacity=".219048"/><stop offset="1" stop-color="#fff"/></linea' +
          'rGradient><radialGradient id="d" cx="18.240929" cy="21.817987" g' +
          'radientUnits="userSpaceOnUse" r="8.308505"><stop offset="0" stop' +
          '-color="#729fcf" stop-opacity=".207843"/><stop offset="1" stop-c' +
          'olor="#729fcf" stop-opacity=".676191"/></radialGradient><radialG' +
          'radient id="e" cx="15.414371" cy="13.078408" gradientTransform="' +
          'matrix(2.592963 0 0 2.252104 -25.05975 -18.941)" gradientUnits="' +
          'userSpaceOnUse" r="6.65625"><stop offset="0" stop-color="#fff"/>' +
          '<stop offset="1" stop-color="#fff" stop-opacity=".247619"/></rad' +
          'ialGradient><rect fill="#ececec" height="41.666668" rx="2.5" str' +
          'oke="#7a7c77" stroke-miterlimit="10.433" stroke-width="1.66667" ' +
          'width="38.333336" x="4.833332" y="3.166666"/><rect fill="none" h' +
          'eight="38.511051" rx=".863101" stroke="#fff" stroke-miterlimit="' +
          '10.433" stroke-width="1.53495" width="35.178051" x="6.367477" y=' +
          '"4.786501"/><g fill="#91938e"><path d="m10.000001 8h28v4h-28z"/>' +
          '<path d="m10 14.999999h28v4h-28z"/><path d="m10 21.999998h28v4h-' +
          '28z"/><path d="m10 28.999998h28v4h-28z"/><path d="m10 36h20v4h-2' +
          '0z"/></g><g transform="matrix(1.0385865 0 0 1.0385865 -1.247917 ' +
          '-1.733141)"><g fill-rule="evenodd"><path d="m18.627569 3.1435548' +
          'c-8.13913 0-14.7448008 6.6056711-14.7448008 14.7448012 0 8.13913' +
          ' 6.6056708 14.744802 14.7448008 14.744802 3.479555 0 6.551001-1.' +
          '384393 9.073723-3.402647-.205377 1.006881-.07803 2.035368.756144' +
          ' 2.759925l10.964084 9.52741c1.233416 1.071329 3.087462.93096 4.1' +
          '5879-.302457 1.071328-1.233418.930959-3.087462-.302457-4.15879l-' +
          '10.964084-9.527411c-.671527-.583279-1.492878-.755969-2.306238-.6' +
          '42722 1.9867-2.512422 3.364839-5.548803 3.364839-8.99811 0-8.139' +
          '1301-6.605671-14.7448012-14.744801-14.7448012zm-.07562 1.2261833' +
          'c7.639459 0 13.291775 4.7889505 13.291775 13.2917749 0 8.675113-' +
          '5.81669 13.291775-13.291775 13.291775-7.302949 0-13.2917734-5.47' +
          '8092-13.2917734-13.291775 0-7.9841069 5.8246384-13.291775 13.291' +
          '7734-13.2917749z" fill="#dcdcdc" stroke="url(#a)" stroke-linecap' +
          '="round" stroke-miterlimit="10" stroke-width="3.00582" transform' +
          '="matrix(.665377 0 0 .665377 15.98645 17.90835)"/><path d="m18.6' +
          '02905 3.0803551c-8.16544 0-14.7924642 6.627024-14.7924642 14.792' +
          '4639 0 8.16544 6.6270242 14.792464 14.7924642 14.792464 3.490803' +
          ' 0 6.572177-1.388867 9.103055-3.413645-.206041 1.010136-.07829 2' +
          '.041947.758587 2.768846l10.999526 9.558207c1.237403 1.074792 3.0' +
          '97442.93397 4.172233-.303435 1.074791-1.237404.933968-3.097442-.' +
          '303435-4.172233l-10.999525-9.558208c-.673698-.585164-1.497704-.7' +
          '58413-2.313693-.644799 1.993122-2.520544 3.375716-5.56674 3.3757' +
          '16-9.027197 0-8.1654399-6.627024-14.7924639-14.792464-14.7924639' +
          'zm-.07586 3.1860692c6.281108.0000002 11.378818 5.0977107 11.3788' +
          '18 11.3788187s-5.09771 11.378818-11.378818 11.378818-11.3788184-' +
          '5.09771-11.3788184-11.378818c.0000002-6.281108 5.0977104-11.3788' +
          '187 11.3788184-11.3788187z" fill="#dcdcdc" transform="matrix(.66' +
          '5377 0 0 .665377 15.98645 17.90835)"/><path d="m39.507004 41.577' +
          '69c-.478672-2.273187 1.39733-4.811422 3.584053-4.788375 0 0-10.7' +
          '60367-9.258111-10.760367-9.258111-2.944791-.05671-4.269502 2.272' +
          '616-3.776814 4.599922z" fill="url(#b)" transform="matrix(.665377' +
          ' 0 0 .665377 15.98645 17.90835)"/></g><circle cx="17.500893" cy=' +
          '"18.920233" fill="none" r="11.048544" stroke="url(#c)" stroke-li' +
          'necap="round" stroke-miterlimit="10" stroke-width="1.20643" tran' +
          'sform="matrix(.82888874 0 0 .82888874 13.707304 13.798294)"/><re' +
          'ct height="4.440478" rx="3.211203" ry="2.837393" style="opacity:' +
          '.433155;fill:none;stroke:#fff;stroke-width:1.50295;stroke-lineca' +
          'p:round;stroke-miterlimit:10" transform="matrix(.50101957 .43784' +
          '268 -.43176447 .50626673 15.98645 17.90835)" width="19.048439" x' +
          '="40.373337" y=".140861"/><circle cx="17.589281" cy="18.478292" ' +
          'r="8.308505" style="fill-rule:evenodd;stroke:#3063a3;stroke-widt' +
          'h:1.07457;stroke-linecap:round;stroke-miterlimit:10;fill:url(#d)' +
          '" transform="matrix(.93060559 0 0 .93060559 11.844919 12.386414)' +
          '"/><path d="m18.156915 7.3966938c-5.20759 0-9.4245469 4.2169572-' +
          '9.4245469 9.4245472 0 1.503975.4203072 2.887773 1.0471719 4.1499' +
          '03 1.25238.461613 2.582757.775683 3.994767.775683 6.170955 0 11.' +
          '099282-4.861637 11.480106-10.937129-1.730964-2.0455312-4.210039-' +
          '3.4130042-7.097498-3.4130042z" fill="url(#e)" fill-rule="evenodd' +
          '" opacity=".834225" transform="matrix(.665377 0 0 .665377 15.986' +
          '45 17.90835)"/></g></svg>'
      end
      item
        IconName = 'printer'
        SVGText = 
          '<svg height="48" width="48" xmlns="http://www.w3.org/2000/svg" x' +
          'mlns:xlink="http://www.w3.org/1999/xlink"><linearGradient id="a"' +
          '><stop offset="0" stop-color="#fff"/><stop offset="1" stop-color' +
          '="#fff" stop-opacity="0"/></linearGradient><linearGradient id="b' +
          '" gradientTransform="matrix(1.0441567 0 0 .986366 -.473743 .2739' +
          '67)" gradientUnits="userSpaceOnUse" x1="11.3125" x2="11.3125" xl' +
          'ink:href="#a" y1="21.6875" y2="33.216167"/><linearGradient id="c' +
          '" gradientTransform="matrix(1.120543 0 0 1.2799981 -3.893311 -12' +
          '.05992)" gradientUnits="userSpaceOnUse" x1="15.916752" x2="15.91' +
          '6752" xlink:href="#a" y1="38.720707" y2="43.940079"/><linearGrad' +
          'ient id="d"><stop offset="0" stop-color="#555753"/><stop offset=' +
          '"1" stop-color="#888a85"/></linearGradient><linearGradient id="e' +
          '" gradientTransform="matrix(1.129032 0 0 1.3333314 -4.097046 -14' +
          '.166581)" gradientUnits="userSpaceOnUse" x1="10.823892" x2="10.6' +
          '02463" xlink:href="#d" y1="43.8125" y2="34.705021"/><linearGradi' +
          'ent id="f"><stop offset="0" stop-color="#d3d7cf"/><stop offset="' +
          '1" stop-color="#eeeeec"/></linearGradient><linearGradient id="g"' +
          ' gradientUnits="userSpaceOnUse" x1="39.314965" x2="8.857596" xli' +
          'nk:href="#f" y1="58.195485" y2="38.000004"/><linearGradient id="' +
          'h" gradientTransform="matrix(.8853266 0 0 1.499469 5.487998 -12.' +
          '734338)" gradientUnits="userSpaceOnUse" x1="38.742561" x2="38.74' +
          '2561" y1="29.743778" y2="31.167559"><stop offset="0" stop-color=' +
          '"#babdb6"/><stop offset="1" stop-color="#fff"/></linearGradient>' +
          '<linearGradient id="i" gradientTransform="matrix(.6666666 0 0 1 ' +
          '14.99973 2.999994)" gradientUnits="userSpaceOnUse" x1="40.791222' +
          '" x2="40.791222" y1="30.003317" y2="29.084894"><stop offset="0" ' +
          'stop-color="#789e2d"/><stop offset="1" stop-color="#a7cc5c"/></l' +
          'inearGradient><linearGradient id="j" gradientTransform="matrix(1' +
          '.079998 0 0 1.003906 -1.920261 -12.099611)" gradientUnits="userS' +
          'paceOnUse" x1="36.523464" x2="36.523464" y1="32.096741" y2="13.7' +
          '49178"><stop offset="0" stop-color="#888a85"/><stop offset="1" s' +
          'top-color="#888a85"/></linearGradient><linearGradient id="k" gra' +
          'dientTransform="matrix(1.079998 0 0 1.003906 -1.920261 -8.083986' +
          ')" gradientUnits="userSpaceOnUse" x1="17.409122" x2="21.360058" ' +
          'y1="33.322712" y2="-23.806805"><stop offset="0" stop-color="#fff' +
          '"/><stop offset="1" stop-color="#babdb6"/></linearGradient><line' +
          'arGradient id="l" gradientTransform="matrix(1.003915 0 0 .984132' +
          '7 .955808 -.703082)" gradientUnits="userSpaceOnUse" x1="17.5" x2' +
          '="17.5" y1="30.755291" y2="20.140139"><stop offset="0" stop-colo' +
          'r="#505a5e"/><stop offset="1" stop-color="#babdb6"/></linearGrad' +
          'ient><linearGradient id="m" gradientUnits="userSpaceOnUse" x1="5' +
          '.126253" x2="5.126253" xlink:href="#a" y1="23.372576" y2="67.140' +
          '252"/><linearGradient id="n" gradientUnits="userSpaceOnUse" x1="' +
          '4.263787" x2="4.263787" y1="35.072957" y2="41.01215"><stop offse' +
          't="0" stop-color="#8e918d"/><stop offset="1" stop-color="#4a5356' +
          '"/></linearGradient><linearGradient id="o" gradientUnits="userSp' +
          'aceOnUse" x1="6.05672" x2="6.05672" y1="40.414815" y2="37.147973' +
          '"><stop offset="0" stop-color="#647175"/><stop offset="1" stop-c' +
          'olor="#9da09c"/></linearGradient><linearGradient id="p" gradient' +
          'Units="userSpaceOnUse" x1=".749729" x2=".749729" xlink:href="#d"' +
          ' y1="65.989037" y2="22.36777"/><linearGradient id="q" gradientTr' +
          'ansform="matrix(1 0 0 .921571 -.000271 4.278139)" gradientUnits=' +
          '"userSpaceOnUse" x1="35.5" x2="35.5" xlink:href="#f" y1="31.1901' +
          '65" y2="24.248672"/><path d="m6.9997292 19.499992-2.25 2.74879c-' +
          '2.1505939 2.627347-3.25 5.068643-3.25 8.754929v5.496281h44.99999' +
          '98v-5.496281c0-3.686286-1.099407-6.127583-3.25-8.754929l-2.25-2.' +
          '74879z" fill="url(#q)" stroke="url(#p)" stroke-linecap="square" ' +
          'stroke-linejoin="round"/><path d="m2.4997292 36.499992v2.90625c0' +
          ' 1.151747.942003 2.09375 2.0937499 2.09375h38.8124999c1.151747 0' +
          ' 2.09375-.942005 2.09375-2.09375v-2.90625z" fill="url(#o)" strok' +
          'e="url(#n)" stroke-linecap="round" stroke-linejoin="round"/><pat' +
          'h d="m7.5116275 20.562492-1.9121622 2.289256c-2.0353965 2.436793' +
          '-3.0997361 4.486071-3.0997361 7.85621v4.792034h42.9999948v-4.792' +
          '034c0-3.370139-1.072112-5.413519-3.099735-7.85621l-1.90026-2.289' +
          '256z" fill="none" stroke="url(#m)" stroke-linecap="square" strok' +
          'e-width=".999999"/><rect fill="url(#l)" height="10.000113" rx=".' +
          '507813" ry=".5" stroke="#454e51" stroke-linecap="round" stroke-l' +
          'inejoin="round" stroke-width=".999777" width="29.000111" x="9.49' +
          '9618" y="20.49988"/><g stroke-linecap="square"><path d="m12.1197' +
          '29 5.4999929h23.759982c.897479 0 1.619999.665754 1.619999 1.5l.0' +
          '00002 14.6869111-26.999981-.0039-.000002-14.6830051c0-.834246.72' +
          '252-1.5 1.62-1.5z" fill="url(#k)" stroke="url(#j)" stroke-linejo' +
          'in="round"/><rect fill="none" height="15.000061" rx=".623645" ry' +
          '=".528426" stroke="#fff" stroke-linejoin="round" stroke-width=".' +
          '999876" width="25.000061" x="11.499729" y="6.499994"/><rect fill' +
          '="url(#i)" height="2" rx="1" stroke="url(#h)" width="3" x="40.99' +
          '9729" y="31.499992"/><path d="m9.1932771 34.499992h29.6129049c.9' +
          '63685 0 1.693549.847638 1.693549 1.733v6.266999l-33.0000019.0000' +
          '11v-6.26701c0-.987249.7736581-1.733 1.693548-1.733z" fill="url(#' +
          'g)" stroke="url(#e)"/><path d="m9.3061214 35.539992c-.4537495 0-' +
          '.8063915.296562-.8063915.81v5.149998h31.0000001v-5.149998c0-.393' +
          '277-.349367-.81-.806391-.81z" fill="none" stroke="url(#c)"/></g>' +
          '<path d="m10.499913 22.500362h26.999687v6.999631h-26.999687z" fi' +
          'll="none" opacity=".243137" stroke="url(#b)" stroke-linecap="rou' +
          'nd"/><path d="m10.499699 21.500022h27.000118" style="fill:#888a8' +
          '5;fill-opacity:.75;fill-rule:evenodd;stroke:#465053;stroke-width' +
          ':.999941;stroke-linecap:square"/><path d="m9.9997292 21.999992h2' +
          '7.9999998v2.993392c-6.461538-1.003304-23.153846-2.006608-27.9999' +
          '998 2.006608z" fill="#fff" opacity=".262745"/></svg>'
      end
      item
        IconName = 'clip-copy-l'
        SVGText = 
          '<svg height="48" viewBox="0 0 48 48" width="48" xmlns="http://ww' +
          'w.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink"><l' +
          'inearGradient id="a"><stop offset="0" stop-color="#97978a"/><sto' +
          'p offset=".5" stop-color="#c2c2b9"/><stop offset="1" stop-color=' +
          '"#7d7d6f"/></linearGradient><linearGradient id="b" gradientUnits' +
          '="userSpaceOnUse" x1="35.957069" x2="10.955403" y1="38.000122" y' +
          '2="11.048571"><stop offset="0" stop-color="#df1111"/><stop offse' +
          't="1" stop-color="#f89f9f"/></linearGradient><linearGradient id=' +
          '"c" gradientTransform="matrix(.5336431 0 0 .4945334 82.283445 9.' +
          '494772)" gradientUnits="userSpaceOnUse" x1="22.308331" x2="35.78' +
          '5294" y1="18.99214" y2="39.498238"><stop offset="0" stop-color="' +
          '#fcfcfc"/><stop offset="1" stop-color="#cfcfc9"/></linearGradien' +
          't><linearGradient id="d" gradientUnits="userSpaceOnUse" x1="86.1' +
          '2496745535" x2="100.02062845725" y1="14.69071225618" y2="28.4254' +
          '3428369"><stop offset="0" stop-color="#f0d827"/><stop offset="1"' +
          ' stop-color="#ed8634"/></linearGradient><linearGradient id="e" g' +
          'radientTransform="matrix(.5271003 0 0 .5007451 82.17076 9.45728)' +
          '" gradientUnits="userSpaceOnUse" x1="25.404572" x2="25.464211" x' +
          'link:href="#a" y1="3.818019" y2="9.323351"/><linearGradient id="' +
          'f" gradientTransform="matrix(.2697729 0 0 .2562843 88.475278 9.1' +
          '65526)" gradientUnits="userSpaceOnUse" x1="25.404572" x2="25.404' +
          '572" xlink:href="#a" y1="3.818019" y2="6.481061"/><linearGradien' +
          't id="g" gradientTransform="matrix(.50336 0 0 .4426226 82.752401' +
          ' 9.879784)" gradientUnits="userSpaceOnUse" x1="25.404572" x2="25' +
          '.464211" xlink:href="#a" y1="3.818019" y2="9.323351"/><g fill-ru' +
          'le="evenodd" transform="matrix(1.6547487 0 0 1.6547487 -127.3265' +
          '2 -11.649422)"><rect fill="url(#d)" height="20.553301" rx=".6950' +
          '03" ry=".695002" stroke="#714c16" stroke-linecap="round" stroke-' +
          'linejoin="round" width="19.546926" x="85.302315" y="11.710633"/>' +
          '<rect fill="url(#c)" height="18.01515" rx=".283676" stroke="#888' +
          'a85" width="15.498841" x="87.339348" y="12.726903"/><rect fill="' +
          '#5c5c5c" height="2.002981" rx=".492671" width="6.008941" x="92.0' +
          '80238" y="9.45728"/><rect fill="url(#e)" height="3.505216" rx=".' +
          '695003" ry=".695002" stroke="#5c5c5c" width="9.487805" x="90.340' +
          '813" y="11.209889"/><rect fill="url(#f)" height="1.79399" rx=".1' +
          '62961" width="4.855914" x="92.656761" y="10.06252"/><rect fill="' +
          'url(#g)" height="3.09836" rx=".507223" width="9.060478" x="90.55' +
          '4474" y="11.428966"/></g><g stroke-miterlimit="10" transform="ma' +
          'trix(0 -.70575829 -.70575121 0 28.526256 40.602289)"><path d="m1' +
          '4.519136 38.5 18.005029-.0039v-12.991632l7.995366-.0078-17.14472' +
          '2-19.9974545-16.8462505 19.9980705 7.9958815.0038z" style="fill-' +
          'rule:evenodd;stroke:#850006;stroke-width:1.00555;stroke-linecap:' +
          'round;stroke-linejoin:round;fill:url(#b)"/><path d="m15.520704 3' +
          '7.496094 16.001405.003906v-12.99295l6.816811-.015625-14.954276-1' +
          '7.4525854-14.7065267 17.4569424 6.8399007.0052z" fill="none" opa' +
          'city=".481283" stroke="#fff"/></g></svg>'
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
      end>
    Left = 425
    Top = 49
  end
end
