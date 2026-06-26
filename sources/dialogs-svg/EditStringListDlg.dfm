object EditStringListDialog: TEditStringListDialog
  Left = 337
  Top = 316
  ActiveControl = lbxStringList
  Caption = 'Edit items from list'
  ClientHeight = 297
  ClientWidth = 330
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poDesigned
  OnCreate = FormCreate
  DesignSize = (
    330
    297)
  PixelsPerInch = 96
  TextHeight = 13
  object lbDesc: TLabel
    Left = 5
    Top = 5
    Width = 3
    Height = 13
  end
  object lbxStringList: TListBox
    Left = 5
    Top = 20
    Width = 201
    Height = 270
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 2
  end
  object gbxMove: TGroupBox
    Left = 210
    Top = 140
    Width = 113
    Height = 61
    Anchors = [akTop, akRight]
    Caption = 'Move item'
    TabOrder = 3
    object UpBtn: TJrButton
      Left = 15
      Top = 20
      Width = 36
      Height = 31
      Hint = 'Move item up'
      Images = imlGlyphs
      ImageIndex = 2
      Layout = blGlyphLeft
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnClick = UpBtnClick
    end
    object DownBtn: TJrButton
      Left = 65
      Top = 20
      Width = 36
      Height = 31
      Hint = 'Move item down'
      Images = imlGlyphs
      ImageIndex = 3
      Layout = blGlyphLeft
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = DownBtnClick
    end
  end
  object OKBtn: TJrButton
    Left = 210
    Top = 215
    Width = 113
    Height = 36
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    Images = imlGlyphs
    ImageIndex = 0
    Layout = blGlyphLeft
    ModalResult = 1
    TabOrder = 0
  end
  object CancelBtn: TJrButton
    Left = 210
    Top = 255
    Width = 113
    Height = 36
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    Images = imlGlyphs
    ImageIndex = 1
    Layout = blGlyphLeft
    ModalResult = 2
    TabOrder = 1
  end
  object btnDelete: TJrButton
    Left = 210
    Top = 100
    Width = 113
    Height = 36
    Anchors = [akTop, akRight]
    Caption = 'Remove'
    Images = imlGlyphs
    ImageIndex = 6
    Layout = blGlyphLeft
    Margin = 8
    TabOrder = 4
    OnClick = btnDeleteClick
  end
  object btnEdit: TJrButton
    Left = 210
    Top = 60
    Width = 113
    Height = 36
    Anchors = [akTop, akRight]
    Caption = 'Edit'
    Images = imlGlyphs
    ImageIndex = 5
    Layout = blGlyphLeft
    Margin = 8
    TabOrder = 5
    OnClick = btnEditClick
  end
  object btnAdd: TJrButton
    Left = 210
    Top = 20
    Width = 113
    Height = 36
    Anchors = [akTop, akRight]
    Caption = 'Add'
    Images = imlGlyphs
    ImageIndex = 4
    Layout = blGlyphLeft
    Margin = 8
    TabOrder = 6
    OnClick = btnAddClick
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
        IconName = 'list-edit'
        SVGText = 
          '<svg height="48" width="48" xmlns="http://www.w3.org/2000/svg" x' +
          'mlns:xlink="http://www.w3.org/1999/xlink"><linearGradient id="a"' +
          ' gradientUnits="userSpaceOnUse" x1="34.470947" x2="36.824364" y1' +
          '="24.522668" y2="26.876085"><stop offset="0" stop-color="#fcaf3e' +
          '"/><stop offset="1" stop-color="#ce5c00"/></linearGradient><line' +
          'arGradient id="b" gradientUnits="userSpaceOnUse" x1="29.840059" ' +
          'x2="29.202465" y1="33.665848" y2="34.303443"><stop offset="0" st' +
          'op-color="#ce5c00"/><stop offset="1" stop-color="#ce5c00"/></lin' +
          'earGradient><linearGradient id="c" gradientUnits="userSpaceOnUse' +
          '" x1="26.379272" x2="25.485056" y1="34.389839" y2="32.714375"><s' +
          'top offset="0" stop-color="#e9b96e"/><stop offset="1" stop-color' +
          '="#fff"/></linearGradient><radialGradient id="d" cx="-138.83727"' +
          ' cy="128.00087" gradientTransform="matrix(.320394 -.3203948 .322' +
          '414 .3224148 50.518433 -74.157887)" gradientUnits="userSpaceOnUs' +
          'e" r="9.126702"><stop offset="0" stop-color="#f9a9a9"/><stop off' +
          'set="1" stop-color="#ab5f5f"/></radialGradient><linearGradient i' +
          'd="e" gradientTransform="matrix(.1892115 -.1892253 .1892115 .189' +
          '2253 49.738733 -37.732231)" gradientUnits="userSpaceOnUse" x1="-' +
          '158.75" x2="-158.75" y1="115.93846" y2="134.25"><stop offset="0"' +
          ' stop-color="#ddd"/><stop offset=".34467545" stop-color="#fff"/>' +
          '<stop offset=".72694808" stop-color="#737373"/><stop offset="1" ' +
          'stop-color="#bbb"/></linearGradient><rect fill="#fff" height="41' +
          '.66666744204" rx="2.49999980091" stroke="#7a7c77" stroke-miterli' +
          'mit="10.433" stroke-width="1.66666571523" width="38.33333355545"' +
          ' x="4.83333242904" y="3.16666710887"/><g fill="#91938e"><path d=' +
          '"m15.00000041473 6.99999997318h24.00000005363v3.99999919023h-24.' +
          '00000005363z"/><path d="m9.00000040132 6.99999997318h3.999999190' +
          '23v3.99999919023h-3.99999919023z"/><path d="m15.00000041473 14.0' +
          '0000039818h24.00000005363v3.99999919023h-24.00000005363z"/><path' +
          ' d="m9.00000040132 14.00000039818h3.99999919023v3.99999919023h-3' +
          '.99999919023z"/><path d="m15.00000041473 21.00000082318h24.00000' +
          '005363v3.99999919023h-24.00000005363z"/><path d="m9.00000040132 ' +
          '21.00000082318h3.99999919023v3.99999919023h-3.99999919023z"/><pa' +
          'th d="m15.00000041473 27.99999879204h24.00000005363v3.9999991902' +
          '3h-24.00000005363z"/><path d="m9.00000040132 27.99999879204h3.99' +
          '999919023v3.99999919023h-3.99999919023z"/><path d="m15.000000414' +
          '73 35.00000167318h24.00000005363v3.99999919023h-24.00000005363z"' +
          '/><path d="m9.00000040132 35.00000167318h3.99999919023v3.9999991' +
          '9023h-3.99999919023z"/></g><g enable-background="new" transform=' +
          '"matrix(1.2219501 0 0 1.2219395 -13.164117 2.562108)"><g><path d' +
          '="m25.89225 30.18459 19-19c2.175049.359961 3.084719 1.732225 3.5' +
          ' 3.5l-19 19-4.616117.704505z" fill="url(#a)" fill-rule="evenodd"' +
          ' stroke="url(#b)" stroke-linejoin="round"/><path d="m26.792248 3' +
          '0.68459 18.49775-18.397748c1.089745.178435 1.517261.987944 2 2l-' +
          '18.397748 18.49775-3.300003.900001z" fill="none" opacity=".28235' +
          '3" stroke="#fff"/><g><path d="m24.549577 34.633026 1.666322-4.18' +
          '0309s1.199535.24536 1.932177.975089.998391 1.943828.998391 1.943' +
          '828z" fill="url(#c)" fill-rule="evenodd"/><path d="m23 21.5-5.5 ' +
          '1.5 2-5" fill="none" stroke="#e9b96e" stroke-linecap="round" str' +
          'oke-linejoin="round" transform="translate(6.39225 12.18459)"/><p' +
          'ath d="m23.95475 33.68459-.90625 2.25 2.34375-.65625c.002-.03184' +
          ' 0-.06141 0-.09375 0-.802125-.645308-1.459801-1.4375-1.5z" fill-' +
          'rule="evenodd"/></g></g><path d="m42.821682 13.147263c1.834152-.' +
          '500531 3.885052 1.651475 3.449911 3.449919l2.195397-2.195402c1.0' +
          '66511-2.466563-1.132252-4.4097129-3.494714-3.494723z" fill="url(' +
          '#d)" stroke="#ef2929"/><path d="m40.561976 15.25084c1.936448-.52' +
          '8484 4.101732 1.743706 3.642321 3.642586l2.317841-2.31801c.75445' +
          '7-1.595776-2.044633-4.337495-3.689625-3.689891z" fill="url(#e)" ' +
          'stroke="#888a85"/></g></svg>'
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
        IconName = 'list-delete'
        SVGText = 
          '<svg height="48" width="48" xmlns="http://www.w3.org/2000/svg" x' +
          'mlns:xlink="http://www.w3.org/1999/xlink"><linearGradient id="a"' +
          ' gradientUnits="userSpaceOnUse" x1="13.000001" x2="35.000004" y1' +
          '="3.947439" y2="42.052559"><stop offset="0" stop-color="#e87d7d"' +
          '/><stop offset="1" stop-color="#a40000"/></linearGradient><rect ' +
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
          '08 9.6252695 21.499708 21.4992185z" fill="url(#a)" stroke="#c00"' +
          ' stroke-width="1.00004"/><path d="m44.49904 22.999272c0 11.32219' +
          '-9.178617 20.500703-20.499249 20.500703-11.321667 0-20.499766-9.' +
          '178619-20.499766-20.500703 0-11.321666 9.178099-20.4992465 20.49' +
          '9766-20.4992465 11.320632 0 20.499249 9.1775805 20.499249 20.499' +
          '2465z" fill="none" opacity=".6" stroke="#fff" stroke-opacity=".5' +
          '" stroke-width="1.00005"/><path d="m16.471763 12-3.471761 3.4717' +
          '61 7.162792 7.126246c.220049.222824.220049.581162 0 .803987l-7.1' +
          '62792 7.126245 3.471761 3.471761 7.126245-7.126246c.222824-.2200' +
          '5.581162-.22005.803987 0l7.126246 7.126246 3.471761-3.471761-7.1' +
          '26246-7.126245c-.220051-.222825-.220051-.581163 0-.803987l7.1262' +
          '46-7.126246-3.471761-3.471761-7.126246 7.126245c-.222825.220051-' +
          '.581163.220051-.803987 0z" fill="#fff" fill-rule="evenodd"/></g>' +
          '</svg>'
      end>
    Left = 25
    Top = 57
  end
end
