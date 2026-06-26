object EditHistListDialog: TEditHistListDialog
  Left = 337
  Top = 316
  ActiveControl = lbxStringList
  BorderStyle = bsSizeToolWin
  Caption = 'Edit list entries'
  ClientHeight = 276
  ClientWidth = 336
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  Position = poDesigned
  OnAfterMonitorDpiChanged = FormAfterMonitorDpiChanged
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  DesignSize = (
    336
    276)
  PixelsPerInch = 96
  TextHeight = 13
  object lbxStringList: TListBox
    Left = 5
    Top = 25
    Width = 201
    Height = 246
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    MultiSelect = True
    TabOrder = 1
  end
  object gbxMove: TGroupBox
    Left = 215
    Top = 135
    Width = 111
    Height = 61
    Anchors = [akRight, akBottom]
    Caption = 'Move entry'
    TabOrder = 4
    object UpBtn: TJrButton
      Left = 15
      Top = 20
      Width = 36
      Height = 36
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
      Left = 60
      Top = 20
      Width = 36
      Height = 36
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
  object lbDesc: TStaticText
    Left = 5
    Top = 7
    Width = 22
    Height = 17
    Caption = 'xxx'
    TabOrder = 0
  end
  object OKBtn: TJrButton
    Left = 215
    Top = 205
    Width = 111
    Height = 31
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    Images = imlGlyphs
    ImageIndex = 0
    Layout = blGlyphLeft
    ModalResult = 1
    TabOrder = 5
  end
  object CancelBtn: TJrButton
    Left = 215
    Top = 240
    Width = 111
    Height = 31
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    Images = imlGlyphs
    ImageIndex = 1
    Layout = blGlyphLeft
    ModalResult = 2
    TabOrder = 6
  end
  object btnDelete: TJrButton
    Left = 215
    Top = 90
    Width = 111
    Height = 36
    Hint = 'Remove item'
    Anchors = [akRight, akBottom]
    Caption = 'Remove'
    Images = imlGlyphs
    ImageIndex = 5
    Layout = blGlyphLeft
    Margin = 8
    TabOrder = 3
    OnClick = btnDeleteClick
  end
  object btnEdit: TJrButton
    Left = 215
    Top = 50
    Width = 111
    Height = 36
    Hint = 'Edit item'
    Anchors = [akRight, akBottom]
    Caption = 'Edit'
    Images = imlGlyphs
    ImageIndex = 4
    Layout = blGlyphLeft
    Margin = 8
    TabOrder = 2
    OnClick = btnEditClick
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
        IconName = 'list-repl'
        SVGText = 
          '<svg height="48" width="48" xmlns="http://www.w3.org/2000/svg" x' +
          'mlns:xlink="http://www.w3.org/1999/xlink"><radialGradient id="a"' +
          ' cx="62.202274" cy="-5.713216" gradientTransform="matrix(-7.5657' +
          '85 -.00000062 .0000004 -4.8230546 494.60904 -26.555114)" gradien' +
          'tUnits="userSpaceOnUse" r="9.755284"><stop offset="0" stop-color' +
          '="#6bbff3"/><stop offset=".46111804" stop-color="#1691c8"/><stop' +
          ' offset="1" stop-color="#1b627a"/></radialGradient><linearGradie' +
          'nt id="b" gradientTransform="matrix(.6104318 0 0 -.49747444 7.79' +
          '0176 24.676459)" gradientUnits="userSpaceOnUse" x1="22.312141" x' +
          '2="22.312141" xlink:href="#c" y1="20.909737" y2="24.184505"/><li' +
          'nearGradient id="c"><stop offset="0" stop-color="#fff"/><stop of' +
          'fset="1" stop-color="#fff"/></linearGradient><linearGradient id=' +
          '"d" gradientTransform="matrix(-.6104318 0 0 .49747444 40.209823 ' +
          '21.323543)" gradientUnits="userSpaceOnUse" x1="22.312141" x2="22' +
          '.312141" xlink:href="#c" y1="20.909737" y2="24.184505"/><linearG' +
          'radient id="e" gradientTransform="matrix(1.0805272 0 0 1.0805026' +
          ' -1.508286 -3.350759)" gradientUnits="userSpaceOnUse" x1="15.046' +
          '636" x2="15.046636" y1="44.787998" y2="3.885126"><stop offset="0' +
          '" stop-color="#09507c"/><stop offset="1" stop-color="#2294b6"/><' +
          '/linearGradient><linearGradient id="f" gradientUnits="userSpaceO' +
          'nUse" x1="25.27776876211" x2="24.95671959522" y1="-.28571423142"' +
          ' y2="47.88877673545"><stop offset="0" stop-color="#dad3d3"/><sto' +
          'p offset="1" stop-color="#dad3d3" stop-opacity="0"/></linearGrad' +
          'ient><rect fill="#fff" height="41.66666744204" rx="2.49999980091' +
          '" stroke="#7a7c77" stroke-miterlimit="10.433" stroke-width="1.66' +
          '666571523" width="38.33333355545" x="4.83333242904" y="3.1666671' +
          '0887"/><g fill="#91938e"><path d="m15.00000041473 7.50000140705h' +
          '24.00000005363v3.99999919023h-24.00000005363z"/><path d="m9.0000' +
          '0040132 7.50000140705h3.99999919023v3.99999919023h-3.99999919023' +
          'z"/><path d="m15.00000041473 15.00000080977h24.00000005363v3.999' +
          '99919023h-24.00000005363z"/><path d="m9.00000040132 15.000000809' +
          '77h3.99999919023v3.99999919023h-3.99999919023z"/><path d="m15.00' +
          '000041473 22.00000123477h24.00000005363v3.99999919023h-24.000000' +
          '05363z"/><path d="m9.00000040132 22.00000123477h3.99999919023v3.' +
          '99999919023h-3.99999919023z"/><path d="m15.00000041473 28.999999' +
          '20363h24.00000005363v3.99999919023h-24.00000005363z"/><path d="m' +
          '9.00000040132 28.99999920363h3.99999919023v3.99999919023h-3.9999' +
          '9919023z"/><path d="m15.00000041473 35.99999962863h24.0000000536' +
          '3v3.99999919023h-24.00000005363z"/><path d="m9.00000040132 35.99' +
          '999962863h3.99999919023v3.99999919023h-3.99999919023z"/></g><g t' +
          'ransform="matrix(.59090911 0 0 .5909091 19.818182 .409091)"><pat' +
          'h d="m45.499979 22.999239c0 11.874491-9.626578 21.500741-21.4997' +
          '08 21.500741-11.874218 0-21.5002507-9.626359-21.5002507-21.50074' +
          '1 0-11.873949 9.6260327-21.4992185 21.5002507-21.4992185 11.8731' +
          '3 0 21.499708 9.6252695 21.499708 21.4992185z" fill="url(#a)" st' +
          'roke="url(#e)" stroke-width="1.00004"/><path d="m44.49904 22.999' +
          '272c0 11.32219-9.178617 20.500703-20.499249 20.500703-11.321667 ' +
          '0-20.499766-9.178619-20.499766-20.500703 0-11.321666 9.178099-20' +
          '.4992465 20.499766-20.4992465 11.320632 0 20.499249 9.1775805 20' +
          '.499249 20.4992465z" fill="none" opacity=".6" stroke="url(#f)" s' +
          'troke-width="1.00005"/><g stroke="#fff" stroke-width=".799858"><' +
          'path d="m19.265171 6.414529-9.687318 7.264651 9.687318 7.605182.' +
          '02752-4.699321h8.256237c4.09063-.138906 6.773926 1.566437 6.6600' +
          '31 3.632325v.09081 4.949044c9.00068-5.123538 3.199501-14.585369-' +
          '6.660029-14.5066l-8.283758.0454z" fill="url(#b)"/><path d="m28.7' +
          '34828 39.585472 9.687318-7.26465-9.687318-7.605182-.02752 4.6993' +
          '21h-8.256237c-4.09063.138906-6.773926-1.566437-6.660031-3.632325' +
          'v-.09081-4.949044c-9.0006791 5.123538-3.199501 14.585368 6.66002' +
          '9 14.506599l8.283758-.0454z" fill="url(#d)"/></g></g></svg>'
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
      end>
    Left = 260
    Top = 10
  end
end
