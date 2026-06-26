object InputTextDialog: TInputTextDialog
  Left = 275
  Top = 268
  BorderStyle = bsDialog
  Caption = 'Edit text'
  ClientHeight = 106
  ClientWidth = 311
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
    311
    106)
  PixelsPerInch = 96
  TextHeight = 13
  object TextFeld: TComboBox
    Left = 10
    Top = 30
    Width = 291
    Height = 21
    Style = csSimple
    Anchors = [akLeft, akTop, akRight]
    DropDownCount = 15
    ParentShowHint = False
    ShowHint = False
    TabOrder = 1
    OnKeyPress = TextFeldKeyPress
  end
  object Descriptor: TStaticText
    Left = 10
    Top = 10
    Width = 53
    Height = 17
    Caption = 'Descriptor'
    TabOrder = 0
  end
  object OKBtn: TJrButton
    Left = 135
    Top = 65
    Width = 66
    Height = 36
    Anchors = [akTop, akRight]
    Caption = 'OK'
    Default = True
    Images = imlGlyphs
    ImageIndex = 0
    Layout = blGlyphLeft
    ModalResult = 1
    TabOrder = 3
  end
  object CancelBtn: TJrButton
    Left = 205
    Top = 65
    Width = 96
    Height = 36
    Anchors = [akTop, akRight]
    Cancel = True
    Caption = 'Cancel'
    Images = imlGlyphs
    ImageIndex = 1
    Layout = blGlyphLeft
    ModalResult = 2
    TabOrder = 4
  end
  object CharTabBtn: TJrButton
    Left = 10
    Top = 65
    Width = 121
    Height = 36
    Caption = 'Character table'
    Images = imlGlyphs
    ImageIndex = 2
    Layout = blGlyphLeft
    TabOrder = 2
    OnClick = CharTabBtnClick
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
        IconName = 'doc-view'
        SVGText = 
          '<svg height="48" viewBox="0 0 48 48" width="48" xmlns="http://ww' +
          'w.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink"><l' +
          'inearGradient id="a" gradientTransform="matrix(.9754464 0 0 1.06' +
          '02677 1.262032 .902902)" gradientUnits="userSpaceOnUse" x1="11" ' +
          'x2="11" y1="17" y2="-3"><stop offset="0" stop-color="#f0f0ee"/><' +
          'stop offset="1" stop-color="#dedede"/></linearGradient><linearGr' +
          'adient id="b" gradientUnits="userSpaceOnUse" x1="27.366341" x2="' +
          '31.335964" y1="26.580296" y2="30.557772"><stop offset="0" stop-c' +
          'olor="#8a8a8a"/><stop offset="1" stop-color="#484848"/></linearG' +
          'radient><linearGradient id="c" gradientTransform="matrix(1.33459' +
          '3 0 0 1.291292 -6.973842 -7.460658)" gradientUnits="userSpaceOnU' +
          'se" x1="30.65625" x2="33.21875" y1="34" y2="31.0625"><stop offse' +
          't="0" stop-color="#d03030"/><stop offset=".5" stop-color="#c9373' +
          '7"/><stop offset="1" stop-color="#e38a8a"/></linearGradient><lin' +
          'earGradient id="d" gradientUnits="userSpaceOnUse" x1="18.292673"' +
          ' x2="17.500893" y1="13.602121" y2="25.743469"><stop offset="0" s' +
          'top-color="#fff"/><stop offset=".5" stop-color="#fff" stop-opaci' +
          'ty=".219048"/><stop offset="1" stop-color="#fff"/></linearGradie' +
          'nt><radialGradient id="e" cx="18.240929" cy="21.817987" gradient' +
          'Units="userSpaceOnUse" r="8.308505"><stop offset="0" stop-color=' +
          '"#729fcf" stop-opacity=".207843"/><stop offset="1" stop-color="#' +
          '729fcf" stop-opacity=".676191"/></radialGradient><radialGradient' +
          ' id="f" cx="15.414371" cy="13.078408" gradientTransform="matrix(' +
          '2.592963 0 0 2.252104 -25.05975 -18.941)" gradientUnits="userSpa' +
          'ceOnUse" r="6.65625"><stop offset="0" stop-color="#fff"/><stop o' +
          'ffset="1" stop-color="#fff" stop-opacity=".247619"/></radialGrad' +
          'ient><g transform="matrix(2.4939273 0 0 2.4939273 -4.690687 -6.0' +
          '16196)"><rect fill="url(#a)" height="16.964285" rx="1.017857" st' +
          'roke="#888a85" stroke-miterlimit="10.433" stroke-width=".678571"' +
          ' width="15.607142" x="3.700648" y="3.553572"/><rect fill="none" ' +
          'height="15.607142" rx=".339286" stroke="#fff" stroke-miterlimit=' +
          '"10.433" stroke-width=".678571" width="14.129219" x="4.5" y="4.2' +
          '32143"/><path id="g" d="m5.890584 5.820617h10.857142v1.002435h-1' +
          '0.857142z" fill="#91938e"/><use transform="matrix(1.0340909 0 0 ' +
          '1.6 -.200815 -3.692857)" xlink:href="#g"/><use transform="matrix' +
          '(1.0340909 0 0 1.6 -.200816 -.88604)" xlink:href="#g"/><use tran' +
          'sform="matrix(1.0340909 0 0 1.6 -.200816 1.920778)" xlink:href="' +
          '#g"/><use transform="matrix(1.0340909 0 0 1.6 -.200816 4.727596)' +
          '" xlink:href="#g"/><use transform="matrix(.73863634 0 0 1.6 1.53' +
          '9584 7.534414)" xlink:href="#g"/></g><g transform="matrix(1.0385' +
          '865 0 0 1.0385865 -1.247917 -1.733141)"><g fill-rule="evenodd"><' +
          'path d="m18.627569 3.1435548c-8.13913 0-14.7448008 6.6056711-14.' +
          '7448008 14.7448012 0 8.13913 6.6056708 14.744802 14.7448008 14.7' +
          '44802 3.479555 0 6.551001-1.384393 9.073723-3.402647-.205377 1.0' +
          '06881-.07803 2.035368.756144 2.759925l10.964084 9.52741c1.233416' +
          ' 1.071329 3.087462.93096 4.15879-.302457 1.071328-1.233418.93095' +
          '9-3.087462-.302457-4.15879l-10.964084-9.527411c-.671527-.583279-' +
          '1.492878-.755969-2.306238-.642722 1.9867-2.512422 3.364839-5.548' +
          '803 3.364839-8.99811 0-8.1391301-6.605671-14.7448012-14.744801-1' +
          '4.7448012zm-.07562 1.2261833c7.639459 0 13.291775 4.7889505 13.2' +
          '91775 13.2917749 0 8.675113-5.81669 13.291775-13.291775 13.29177' +
          '5-7.302949 0-13.2917734-5.478092-13.2917734-13.291775 0-7.984106' +
          '9 5.8246384-13.291775 13.2917734-13.2917749z" fill="#dcdcdc" str' +
          'oke="url(#b)" stroke-linecap="round" stroke-miterlimit="10" stro' +
          'ke-width="3.00582" transform="matrix(.665377 0 0 .665377 15.9864' +
          '5 17.90835)"/><path d="m18.602905 3.0803551c-8.16544 0-14.792464' +
          '2 6.627024-14.7924642 14.7924639 0 8.16544 6.6270242 14.792464 1' +
          '4.7924642 14.792464 3.490803 0 6.572177-1.388867 9.103055-3.4136' +
          '45-.206041 1.010136-.07829 2.041947.758587 2.768846l10.999526 9.' +
          '558207c1.237403 1.074792 3.097442.93397 4.172233-.303435 1.07479' +
          '1-1.237404.933968-3.097442-.303435-4.172233l-10.999525-9.558208c' +
          '-.673698-.585164-1.497704-.758413-2.313693-.644799 1.993122-2.52' +
          '0544 3.375716-5.56674 3.375716-9.027197 0-8.1654399-6.627024-14.' +
          '7924639-14.792464-14.7924639zm-.07586 3.1860692c6.281108.0000002' +
          ' 11.378818 5.0977107 11.378818 11.3788187s-5.09771 11.378818-11.' +
          '378818 11.378818-11.3788184-5.09771-11.3788184-11.378818c.000000' +
          '2-6.281108 5.0977104-11.3788187 11.3788184-11.3788187z" fill="#d' +
          'cdcdc" transform="matrix(.665377 0 0 .665377 15.98645 17.90835)"' +
          '/><path d="m39.507004 41.57769c-.478672-2.273187 1.39733-4.81142' +
          '2 3.584053-4.788375 0 0-10.760367-9.258111-10.760367-9.258111-2.' +
          '944791-.05671-4.269502 2.272616-3.776814 4.599922z" fill="url(#c' +
          ')" transform="matrix(.665377 0 0 .665377 15.98645 17.90835)"/></' +
          'g><circle cx="17.500893" cy="18.920233" fill="none" r="11.048544' +
          '" stroke="url(#d)" stroke-linecap="round" stroke-miterlimit="10"' +
          ' stroke-width="1.20643" transform="matrix(.82888874 0 0 .8288887' +
          '4 13.707304 13.798294)"/><rect height="4.440478" rx="3.211203" r' +
          'y="2.837393" style="opacity:.433155;fill:none;stroke:#fff;stroke' +
          '-width:1.50295;stroke-linecap:round;stroke-miterlimit:10" transf' +
          'orm="matrix(.50101957 .43784268 -.43176447 .50626673 15.98645 17' +
          '.90835)" width="19.048439" x="40.373337" y=".140861"/><circle cx' +
          '="17.589281" cy="18.478292" r="8.308505" style="fill-rule:evenod' +
          'd;stroke:#3063a3;stroke-width:1.07457;stroke-linecap:round;strok' +
          'e-miterlimit:10;fill:url(#e)" transform="matrix(.93060559 0 0 .9' +
          '3060559 11.844919 12.386414)"/><path d="m18.156915 7.3966938c-5.' +
          '20759 0-9.4245469 4.2169572-9.4245469 9.4245472 0 1.503975.42030' +
          '72 2.887773 1.0471719 4.149903 1.25238.461613 2.582757.775683 3.' +
          '994767.775683 6.170955 0 11.099282-4.861637 11.480106-10.937129-' +
          '1.730964-2.0455312-4.210039-3.4130042-7.097498-3.4130042z" fill=' +
          '"url(#f)" fill-rule="evenodd" opacity=".834225" transform="matri' +
          'x(.665377 0 0 .665377 15.98645 17.90835)"/></g></svg>'
      end>
    Left = 215
    Top = 10
  end
end
