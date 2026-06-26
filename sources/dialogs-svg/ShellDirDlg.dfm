object ShellDirDialog: TShellDirDialog
  Left = 340
  Top = 259
  BorderIcons = [biSystemMenu]
  Caption = 'Select directory'
  ClientHeight = 444
  ClientWidth = 801
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poDesigned
  OnActivate = FormActivate
  OnAfterMonitorDpiChanged = FormAfterMonitorDpiChanged
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter: TSplitter
    Left = 408
    Top = 0
    Height = 444
    Align = alRight
    MinSize = 1
    OnMoved = FormResize
    ExplicitLeft = 380
    ExplicitTop = 340
    ExplicitHeight = 100
  end
  object PanelLeft: TPanel
    Left = 0
    Top = 0
    Width = 408
    Height = 444
    Align = alClient
    TabOrder = 0
    object ShellTreeView: TShellTreeView
      Left = 97
      Top = 1
      Width = 310
      Height = 339
      ObjectTypes = [otFolders]
      Root = 'rfMyComputer'
      ShellListView = ShellListView
      UseShellImages = True
      Align = alClient
      AutoRefresh = False
      ExpandOnLoad = False
      HideSelection = False
      Indent = 19
      ParentColor = False
      PopupMenu = PopupMenu
      RightClickSelect = True
      ShowRoot = False
      TabOrder = 0
      OnClick = ShellTreeViewClick
      OnMouseDown = ShellTreeViewMouseDown
      OnChange = ShellTreeViewChange
    end
    object panRoot: TPanel
      Left = 1
      Top = 1
      Width = 96
      Height = 339
      Align = alLeft
      BevelInner = bvLowered
      BevelOuter = bvNone
      Ctl3D = True
      ParentBackground = False
      ParentCtl3D = False
      TabOrder = 1
      DesignSize = (
        96
        339)
      object spbNetwork: TJrSpeedButton
        Tag = 103
        Left = 2
        Top = 247
        Width = 94
        Height = 81
        Anchors = [akLeft, akTop, akRight]
        GroupIndex = 1
        Caption = 'Network'
        Flat = True
        Images = SVGImgLst48
        ImageIndex = 3
        Spacing = 0
        OnClick = spbNetworkClick
      end
      object spbComputer: TJrSpeedButton
        Tag = 102
        Left = 2
        Top = 162
        Width = 94
        Height = 81
        Anchors = [akLeft, akTop, akRight]
        GroupIndex = 1
        Caption = 'Computer'
        Flat = True
        Images = SVGImgLst48
        ImageIndex = 0
        Spacing = 0
        OnClick = spbComputerClick
      end
      object spbDesktop: TJrSpeedButton
        Tag = 100
        Left = 2
        Top = 2
        Width = 94
        Height = 81
        Anchors = [akLeft, akTop, akRight]
        GroupIndex = 1
        Caption = 'Desktop'
        Flat = True
        Images = SVGImgLst48
        ImageIndex = 1
        Spacing = 0
        OnClick = spbDesktopClick
      end
      object spbMyFiles: TJrSpeedButton
        Tag = 101
        Left = 2
        Top = 82
        Width = 94
        Height = 81
        Anchors = [akLeft, akTop, akRight]
        GroupIndex = 1
        Caption = 'Documents'
        Flat = True
        Images = SVGImgLst48
        ImageIndex = 2
        Spacing = 0
        OnClick = spbMyFilesClick
      end
    end
    object Panel1: TPanel
      Left = 1
      Top = 340
      Width = 406
      Height = 103
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 2
      DesignSize = (
        406
        103)
      object Label1: TLabel
        Left = 5
        Top = 23
        Width = 48
        Height = 13
        Anchors = [akLeft, akBottom]
        Caption = 'Directory:'
        ExplicitTop = 13
      end
      object spbHome: TJrSpeedButton
        Tag = 200
        Left = 297
        Top = 5
        Width = 31
        Height = 31
        Hint = 'Go to default folder'
        Anchors = [akRight, akBottom]
        Flat = True
        Images = SVGImgLst24
        ImageIndex = 8
        Layout = blGlyphLeft
        ParentShowHint = False
        ShowHint = True
        OnClick = spbHomeClick
      end
      object spbUp: TJrSpeedButton
        Tag = 201
        Left = 367
        Top = 5
        Width = 31
        Height = 31
        Hint = 'One folder up'
        Anchors = [akRight, akBottom]
        Flat = True
        Images = SVGImgLst24
        ImageIndex = 7
        Layout = blGlyphLeft
        ParentShowHint = False
        ShowHint = True
        OnClick = spbUpClick
      end
      object spbNew: TJrSpeedButton
        Tag = 202
        Left = 332
        Top = 5
        Width = 31
        Height = 31
        Hint = 'Create new folder'
        Anchors = [akRight, akBottom]
        Flat = True
        Images = SVGImgLst24
        ImageIndex = 6
        Layout = blGlyphLeft
        ParentShowHint = False
        ShowHint = True
        OnClick = spbNewClick
      end
      object cbxFiles: TCheckBox
        Left = 95
        Top = 8
        Width = 196
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Show files'
        TabOrder = 0
        OnClick = cbxFilesClick
      end
      object cbxSelectedDir: TComboBox
        Left = 5
        Top = 40
        Width = 393
        Height = 21
        Hint = 'Directory'
        Anchors = [akLeft, akRight, akBottom]
        TabOrder = 1
        OnChange = cbxSelectedDirChange
        OnCloseUp = cbxSelectedDirCloseUp
      end
      object laVolHint: TStaticText
        Left = 10
        Top = 75
        Width = 4
        Height = 4
        Anchors = [akLeft, akBottom]
        TabOrder = 4
      end
      object btbOK: TJrButton
        Left = 222
        Top = 67
        Width = 71
        Height = 31
        Anchors = [akRight, akBottom]
        Caption = 'OK'
        Default = True
        Images = SVGImgLst24
        ImageIndex = 4
        Layout = blGlyphLeft
        ModalResult = 1
        TabOrder = 2
        OnClick = btbOKClick
      end
      object btbCancel: TJrButton
        Left = 297
        Top = 67
        Width = 101
        Height = 31
        Anchors = [akRight, akBottom]
        Cancel = True
        Caption = 'Cancel'
        Images = SVGImgLst24
        ImageIndex = 5
        Layout = blGlyphLeft
        ModalResult = 2
        TabOrder = 3
      end
    end
  end
  object PanelRight: TPanel
    Left = 411
    Top = 0
    Width = 390
    Height = 444
    Align = alRight
    TabOrder = 1
    object ShellListView: TShellListView
      Left = 1
      Top = 1
      Width = 388
      Height = 442
      ObjectTypes = [otNonFolders]
      Root = 'rfMyComputer'
      ShellTreeView = ShellTreeView
      Sorted = True
      Align = alClient
      ReadOnly = False
      HideSelection = False
      TabOrder = 0
      ViewStyle = vsReport
    end
  end
  object PopupMenu: TPopupMenu
    OnPopup = PopupMenuPopup
    Left = 461
    Top = 164
    object itmCreate: TMenuItem
      Caption = 'Create folder'
      OnClick = spbNewClick
    end
    object itmDelete: TMenuItem
      Caption = 'Delete folder'
      OnClick = itmDeleteClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object itmUpdate: TMenuItem
      Caption = 'Update'
      OnClick = itmUpdateClick
    end
    object cancel1: TMenuItem
      Caption = 'Cancel'
    end
  end
  object SVGImgColl: TSVGIconImageCollection
    SVGIconItems = <
      item
        IconName = 'computer'
        SVGText = 
          '<?xml version="1.0" encoding="UTF-8" standalone="no"?>'#10'<!-- Crea' +
          'ted with Inkscape (http://www.inkscape.org/) -->'#10#10'<svg'#10'   width=' +
          '"48"'#10'   height="48"'#10'   viewBox="0 0 48 48"'#10'   version="1.1"'#10'   i' +
          'd="svg1"'#10'   inkscape:version="1.4 (86a8ad7, 2024-10-11)"'#10'   sodi' +
          'podi:docname="computer.svg"'#10'   xmlns:inkscape="http://www.inksca' +
          'pe.org/namespaces/inkscape"'#10'   xmlns:sodipodi="http://sodipodi.s' +
          'ourceforge.net/DTD/sodipodi-0.dtd"'#10'   xmlns:xlink="http://www.w3' +
          '.org/1999/xlink"'#10'   xmlns="http://www.w3.org/2000/svg"'#10'   xmlns:' +
          'svg="http://www.w3.org/2000/svg">'#10'  <sodipodi:namedview'#10'     id=' +
          '"namedview1"'#10'     pagecolor="#ffffff"'#10'     bordercolor="#666666"' +
          #10'     borderopacity="1.0"'#10'     inkscape:showpageshadow="2"'#10'     ' +
          'inkscape:pageopacity="0.0"'#10'     inkscape:pagecheckerboard="0"'#10'  ' +
          '   inkscape:deskcolor="#d1d1d1"'#10'     inkscape:document-units="px' +
          '"'#10'     showgrid="true"'#10'     showguides="true"'#10'     inkscape:zoom' +
          '="19.4375"'#10'     inkscape:cx="23.974277"'#10'     inkscape:cy="24"'#10'  ' +
          '   inkscape:current-layer="layer1-8">'#10'    <inkscape:grid'#10'       ' +
          'id="grid1"'#10'       units="px"'#10'       originx="0"'#10'       originy="' +
          '0"'#10'       spacingx="1"'#10'       spacingy="1"'#10'       empcolor="#000' +
          '0ff"'#10'       empopacity="0.25098039"'#10'       color="#0000ff"'#10'     ' +
          '  opacity="0.1254902"'#10'       empspacing="5"'#10'       enabled="true' +
          '"'#10'       visible="true" />'#10'    <sodipodi:guide'#10'       position="' +
          '7.2540193,52.527331"'#10'       orientation="0,-1"'#10'       id="guide1' +
          '"'#10'       inkscape:locked="false" />'#10'  </sodipodi:namedview>'#10'  <d' +
          'efs'#10'     id="defs1">'#10'    <linearGradient'#10'       gradientTransfor' +
          'm="matrix(1,0,0,0.992781,0,-2.718035)"'#10'       gradientUnits="use' +
          'rSpaceOnUse"'#10'       id="linearGradient2308"'#10'       inkscape:coll' +
          'ect="always"'#10'       x1="31.743324"'#10'       x2="31.86105"'#10'       x' +
          'link:href="#linearGradient5137"'#10'       y1="37.842293"'#10'       y2=' +
          '"43.82579" />'#10'    <linearGradient'#10'       id="linearGradient5137"' +
          '>'#10'      <stop'#10'         id="stop5139"'#10'         offset="0"'#10'       ' +
          '  style="stop-color:#d6d6d4;stop-opacity:1" />'#10'      <stop'#10'     ' +
          '    id="stop5141"'#10'         offset="1"'#10'         style="stop-color' +
          ':#b8b8b2;stop-opacity:1" />'#10'    </linearGradient>'#10'    <linearGra' +
          'dient'#10'       gradientTransform="matrix(0.999303,0,0,0.998527,0.0' +
          '0306125,-2.971316)"'#10'       gradientUnits="userSpaceOnUse"'#10'      ' +
          ' id="linearGradient5147"'#10'       inkscape:collect="always"'#10'      ' +
          ' x1="17.247635"'#10'       x2="39.904388"'#10'       xlink:href="#linear' +
          'Gradient5137"'#10'       y1="6.3760414"'#10'       y2="38.876041" />'#10'   ' +
          ' <radialGradient'#10'       cx="24.006104"'#10'       cy="32.997028"'#10'   ' +
          '    fx="24.006104"'#10'       fy="32.997028"'#10'       gradientTransfor' +
          'm="matrix(1.232634,0,0,0.778392,-5.590582,-0.847446)"'#10'       gra' +
          'dientUnits="userSpaceOnUse"'#10'       id="radialGradient5239"'#10'     ' +
          '  inkscape:collect="always"'#10'       r="19.00016"'#10'       xlink:hre' +
          'f="#linearGradient5233" />'#10'    <linearGradient'#10'       id="linear' +
          'Gradient5233">'#10'      <stop'#10'         id="stop5235"'#10'         offse' +
          't="0"'#10'         style="stop-color:#678fba;stop-opacity:1" />'#10'    ' +
          '  <stop'#10'         id="stop5237"'#10'         offset="1"'#10'         styl' +
          'e="stop-color:#2d5785;stop-opacity:1" />'#10'    </linearGradient>'#10' ' +
          '   <linearGradient'#10'       gradientTransform="translate(-0.023529' +
          '4,-3.039216)"'#10'       gradientUnits="userSpaceOnUse"'#10'       id="l' +
          'inearGradient6246"'#10'       inkscape:collect="always"'#10'       x1="2' +
          '0.156862"'#10'       x2="20.156862"'#10'       xlink:href="#linearGradie' +
          'nt410"'#10'       y1="5.0996137"'#10'       y2="26.039215" />'#10'    <linea' +
          'rGradient'#10'       inkscape:collect="always"'#10'       id="linearGrad' +
          'ient410">'#10'      <stop'#10'         id="stop6242-1"'#10'         offset="' +
          '0"'#10'         style="stop-color:#e6e6e6;stop-opacity:1" />'#10'      <' +
          'stop'#10'         id="stop6244-1"'#10'         offset="1"'#10'         style' +
          '="stop-color:#e6e6e6;stop-opacity:0" />'#10'    </linearGradient>'#10'  ' +
          '  <linearGradient'#10'       gradientTransform="matrix(1,0,0,0.75304' +
          '4,-48,12.25251)"'#10'       gradientUnits="userSpaceOnUse"'#10'       id' +
          '="linearGradient3442"'#10'       inkscape:collect="always"'#10'       x1' +
          '="26.5"'#10'       x2="26.5"'#10'       xlink:href="#linearGradient3899"' +
          #10'       y1="43.249905"'#10'       y2="35.75" />'#10'    <linearGradient'#10 +
          '       id="linearGradient3899"'#10'       inkscape:collect="always">' +
          #10'      <stop'#10'         id="stop3901"'#10'         offset="0"'#10'        ' +
          ' style="stop-color:#d6d6d4" />'#10'      <stop'#10'         id="stop3903' +
          '"'#10'         offset="1"'#10'         style="stop-color:#bec2ba" />'#10'   ' +
          ' </linearGradient>'#10'    <linearGradient'#10'       gradientTransform=' +
          '"matrix(1,0,0,0.833169,-48,19.08245)"'#10'       gradientUnits="user' +
          'SpaceOnUse"'#10'       id="linearGradient3320"'#10'       inkscape:colle' +
          'ct="always"'#10'       x1="16.396038"'#10'       x2="16.396038"'#10'       x' +
          'link:href="#linearGradient1558"'#10'       y1="19.659277"'#10'       y2=' +
          '"32.448051" />'#10'    <linearGradient'#10'       id="linearGradient1558' +
          '"'#10'       inkscape:collect="always">'#10'      <stop'#10'         id="sto' +
          'p1560"'#10'         offset="0"'#10'         style="stop-color:#969a92;st' +
          'op-opacity:1" />'#10'      <stop'#10'         id="stop1562"'#10'         off' +
          'set="1"'#10'         style="stop-color:#d6d6d4" />'#10'    </linearGradi' +
          'ent>'#10'  </defs>'#10'  <g'#10'     id="layer1-8"'#10'     inkscape:groupmode="' +
          'layer"'#10'     inkscape:label="Computer"'#10'     style="display:inline' +
          '"'#10'     transform="matrix(0.97310799,0,0,0.97310799,0.76704674,1.' +
          '1232001)">'#10'    <g'#10'       id="g4339"'#10'       transform="translate(' +
          '-25)">'#10'      <path'#10'         d="M 65.675044,41.213388 C 57.656634' +
          ',30.125 72.913568,39.404152 71.161327,35 69.172021,30 52.551603,' +
          '36.027728 53.480248,30.116116"'#10'         id="path4172"'#10'         s' +
          'odipodi:nodetypes="czz"'#10'         style="display:inline;fill:none' +
          ';fill-opacity:1;fill-rule:evenodd;stroke:#678fba;stroke-width:1;' +
          'stroke-linecap:butt;stroke-linejoin:miter;stroke-miterlimit:4;st' +
          'roke-dasharray:none;stroke-opacity:1" />'#10'    </g>'#10'    <g'#10'       ' +
          'id="g2302"'#10'       transform="matrix(0.811017,0,0,0.811017,4.5360' +
          '63,4.144784)">'#10'      <path'#10'         d="m 14.375479,32.558794 c 0' +
          ',0 1.216876,4.898976 -3.856329,4.944966 -2.4302757,0.02175 -1.93' +
          '24777,4.006021 -1.9324777,4.006021 l 30.8464667,-0.03115 c 0,0 0' +
          '.418438,-3.867241 -2.022217,-3.912581 -4.987467,-0.09147 -3.8105' +
          '29,-5.06955 -3.810529,-5.06955 z"'#10'         id="path1359"'#10'       ' +
          '  sodipodi:nodetypes="csccscc"'#10'         style="color:#000000;dis' +
          'play:inline;overflow:visible;visibility:visible;opacity:1;fill:u' +
          'rl(#linearGradient2308);fill-opacity:1;fill-rule:evenodd;stroke:' +
          '#7a7c78;stroke-width:1.23302;stroke-linecap:butt;stroke-linejoin' +
          ':miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffs' +
          'et:0;stroke-opacity:1;marker-start:none;marker-mid:none;marker-e' +
          'nd:none" />'#10'    </g>'#10'    <path'#10'       d="M 4.8882799,0.5019965 H' +
          ' 42.990539 c 2.023707,0 3.498537,1.4255519 3.498537,3.6208005 l ' +
          '0.01094,25.165237 C 46.500019,30.977609 45.97204,31.5 44.466781,' +
          '31.5 L 3.5326624,31.481093 C 2.3542134,31.452343 1.5154744,30.98' +
          '7161 1.4996519,29.464764 L 1.5148181,3.935329 c 0,-1.7712136 1.5' +
          '383348,-3.4333325 3.3734618,-3.4333325 z"'#10'       id="rect5040"'#10' ' +
          '      sodipodi:nodetypes="ccccccccc"'#10'       style="display:inlin' +
          'e;fill:url(#linearGradient5147);fill-opacity:1;fill-rule:evenodd' +
          ';stroke:#7a7c78;stroke-width:1;stroke-linecap:butt;stroke-linejo' +
          'in:miter;stroke-miterlimit:4;stroke-opacity:1" />'#10'    <rect'#10'    ' +
          '   height="23"'#10'       id="rect9208"'#10'       style="display:inline' +
          ';fill:url(#radialGradient5239);fill-opacity:1;fill-rule:evenodd;' +
          'stroke:#1d437a;stroke-width:1px;stroke-linecap:butt;stroke-linej' +
          'oin:miter;stroke-opacity:1"'#10'       width="37"'#10'       x="5.5"'#10'   ' +
          '    y="4.5" />'#10'    <path'#10'       d="M 6,4.9921565 V 23 C 22.44444' +
          '5,21.645751 28.598693,12.887581 41.968627,11.972548 l 1e-6,-7.01' +
          '1764 z"'#10'       id="path4073"'#10'       sodipodi:nodetypes="ccccc"'#10' ' +
          '      style="display:inline;opacity:0.75;fill:url(#linearGradien' +
          't6246);fill-opacity:1;fill-rule:evenodd;stroke:none;stroke-width' +
          ':1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"' +
          ' />'#10'    <g'#10'       id="g3316"'#10'       transform="translate(48)">'#10' ' +
          '     <path'#10'         d="m -42.6875,40.49166 c -0.967886,0 -1.8477' +
          '63,0.285348 -2.4375,1.247229 l -1.8125,2.918046 c -0.808653,1.05' +
          '4721 -0.156385,1.859077 2.59375,1.859077 h 40.4375 c 2.750135,0 ' +
          '3.402403,-0.804356 2.59375,-1.859077 L -3.125,41.738889 C -3.714' +
          '737,40.777008 -4.594614,40.49166 -5.5625,40.49166 Z"'#10'         id' +
          '="rect2024"'#10'         sodipodi:nodetypes="ccccccccc"'#10'         sty' +
          'le="fill:url(#linearGradient3442);fill-opacity:1;stroke:#7a7c78;' +
          'stroke-width:1;stroke-linejoin:round;stroke-miterlimit:4;stroke-' +
          'opacity:1" />'#10'      <path'#10'         d="m 5.3125,38.5 c -0.7068869' +
          ',0 -1.0666848,0.07419 -1.5292969,1.076172 a 1.0001,1.0001 0 0 1 ' +
          '-0.00195,0.0039 l -1.8125,3.875 a 1.0001,1.0001 0 0 1 -0.041016,' +
          '0.07617 c -0.1455375,0.252075 -0.1851561,0.437195 -0.1875,0.5097' +
          '66 -0.00234,0.07257 -0.01383,0.03574 0.021484,0.07422 C 1.832347' +
          '6,44.192192 2.4106476,44.5 3.65625,44.5 h 40.4375 c 1.245602,0 1' +
          '.823902,-0.307808 1.894531,-0.384766 0.03531,-0.03848 0.02383,-0' +
          '.0016 0.02148,-0.07422 -0.0023,-0.07257 -0.04196,-0.257691 -0.18' +
          '75,-0.509766 a 1.0001,1.0001 0 0 1 -0.04102,-0.07617 l -1.8125,-' +
          '3.875 a 1.0001,1.0001 0 0 1 -0.002,-0.0039 C 43.504185,38.57419 ' +
          '43.144387,38.5 42.4375,38.5 Z"'#10'         id="path2920"'#10'         i' +
          'nkscape:original="M 5.3125 37.5 C 4.344614 37.5 3.4647369 37.878' +
          '926 2.875 39.15625 L 1.0625 43.03125 C 0.253847 44.43186 0.90611' +
          '493 45.5 3.65625 45.5 L 44.09375 45.5 C 46.843885 45.5 47.496153' +
          ' 44.43186 46.6875 43.03125 L 44.875 39.15625 C 44.285263 37.8789' +
          '26 43.405386 37.5 42.4375 37.5 L 5.3125 37.5 z "'#10'         inksca' +
          'pe:radius="-1"'#10'         sodipodi:type="inkscape:offset"'#10'        ' +
          ' style="display:inline;fill:none;fill-opacity:0.46875;stroke:#e6' +
          'e6e6;stroke-width:1.22475;stroke-linejoin:round;stroke-miterlimi' +
          't:4;stroke-opacity:1"'#10'         transform="matrix(1,0,0,0.666668,' +
          '-48,15.83327)" />'#10'    </g>'#10'    <g'#10'       id="g3252"'#10'       trans' +
          'form="translate(48)"'#10'       style="display:inline">'#10'      <path'#10 +
          '         d="m -41.565234,39.500963 c -0.003,0.0031 0.0028,0.0235' +
          '9 0,0.026 -0.01467,0.0041 -0.05022,0.02021 -0.06523,0.02601 -0.0' +
          '05,0.0021 -0.02762,-0.0023 -0.03262,0 -0.005,0.0025 -0.02769,0.0' +
          '233 -0.03262,0.026 -0.0097,0.0058 -0.02321,0.01943 -0.03262,0.02' +
          '6 -0.0046,0.0035 -0.02815,0.02233 -0.03262,0.02601 -0.0043,0.003' +
          '9 -0.02844,0.02193 -0.03262,0.02601 -0.06487,0.075 -0.133186,0.2' +
          '05396 -0.228319,0.364081 l -2.22755,3.952895 c 0,0.0049 -2.28e-4' +
          ',0.02105 0,0.026 4.53e-4,0.005 -6.74e-4,0.02104 0,0.026 8.91e-4,' +
          '0.005 -0.0011,0.02106 0,0.02601 0.0026,0.0099 0.02917,0.04222 0.' +
          '03262,0.05201 0.0019,0.0049 -0.0021,0.02117 0,0.02601 0.0069,0.0' +
          '1439 0.02403,0.03804 0.03262,0.05201 0.0091,0.0138 0.02197,0.038' +
          '81 0.03262,0.05201 0.0037,0.0043 0.02875,0.02176 0.03262,0.026 0' +
          '.004,0.0042 0.02844,0.02194 0.03262,0.02601 0.02595,0.02385 0.06' +
          '703,0.05818 0.09785,0.07802 0.0053,0.0032 0.02724,0.02296 0.0326' +
          '2,0.02601 0.0165,0.0079 0.04736,0.01884 0.06523,0.026 0.120228,0' +
          '.04402 0.279283,0.07802 0.42402,0.07802 h 23.114643 l -0.09785,-' +
          '4.993121 -20.789242,-6e-6 c -0.06851,0 -0.143762,-0.0019 -0.1957' +
          '01,0 -0.01215,5.7e-5 -0.05203,5.11e-4 -0.06523,0 -0.004,2.5e-5 -' +
          '0.02896,-2.14e-4 -0.03262,0 -0.0028,0.0024 -0.02957,-0.0031 -0.0' +
          '3262,0 z m 23.039809,6e-6 0.09785,1.664375 h 5.642726 l -0.26093' +
          '5,-1.664375 z m 7.436655,0 1.17421,4.993121 h 5.218705 c 0.14473' +
          '6,0 0.303792,-0.034 0.42402,-0.07802 0.017877,-0.0072 0.048735,-' +
          '0.0181 0.065233,-0.026 0.00538,-0.003 0.027357,-0.02283 0.032617' +
          ',-0.02601 0.030821,-0.01983 0.071897,-0.05416 0.097851,-0.07802 ' +
          '0.00418,-0.0041 0.028592,-0.02185 0.032616,-0.02601 0.00387,-0.0' +
          '042 0.028906,-0.02168 0.032618,-0.026 0.010648,-0.0132 0.023496,' +
          '-0.03821 0.032617,-0.05201 0.00859,-0.01397 0.025688,-0.03762 0.' +
          '032617,-0.05201 0.00212,-0.0048 -0.00192,-0.02114 0,-0.02601 0.0' +
          '0345,-0.0098 0.029987,-0.04214 0.032617,-0.05201 0.0011,-0.005 -' +
          '8.92e-4,-0.02105 0,-0.02601 6.73e-4,-0.005 -4.54e-4,-0.02104 0,-' +
          '0.026 2.29e-4,-0.005 0,-0.02106 0,-0.026 l -1.826015,-3.9529 C -' +
          '5.834197,39.862396 -5.902512,39.732002 -5.967383,39.656999 -5.97' +
          '1557,39.652925 -5.99567,39.634868 -6,39.630993 c -0.00447,-0.003' +
          '7 -0.028021,-0.02252 -0.032617,-0.02601 -0.00941,-0.0066 -0.0228' +
          '76,-0.02021 -0.032617,-0.026 -0.00493,-0.0027 -0.027643,-0.0235 ' +
          '-0.032616,-0.026 -0.005,-0.0023 -0.027608,0.0021 -0.032617,0 -0.' +
          '015014,-0.0058 -0.050564,-0.02192 -0.065234,-0.02601 -0.016117,-' +
          '0.0031 -0.041567,-0.02056 -0.065235,-0.026 -0.011875,-0.0025 -0.' +
          '054353,0.0014 -0.065233,0 -0.05194,-0.0019 -0.127187,0 -0.195701' +
          ',0 z m -6.131978,3.328747 -1.043741,1.664374 h 6.001511 l -1.304' +
          '677,-1.664374 z"'#10'         id="path2308"'#10'         sodipodi:nodety' +
          'pes="csssssssccssssssssssssccccssscccccccccssssssssssssccsssssss' +
          'ssccccccc"'#10'         style="color:#000000;display:inline;overflow' +
          ':visible;visibility:visible;fill:url(#linearGradient3320);fill-o' +
          'pacity:1;fill-rule:nonzero;stroke:#858980;stroke-width:0.999999;' +
          'stroke-linecap:square;stroke-linejoin:round;stroke-miterlimit:4;' +
          'stroke-dashoffset:0;stroke-opacity:1;marker-start:none;marker-mi' +
          'd:none;marker-end:none" />'#10'    </g>'#10'  </g>'#10'</svg>'#10
      end
      item
        IconName = 'desktop'
        SVGText = 
          '<?xml version="1.0" encoding="UTF-8" standalone="no"?>'#10'<!-- Crea' +
          'ted with Inkscape (http://www.inkscape.org/) -->'#10#10'<svg'#10'   width=' +
          '"48"'#10'   height="48"'#10'   viewBox="0 0 48 48"'#10'   version="1.1"'#10'   i' +
          'd="svg1"'#10'   inkscape:version="1.4 (86a8ad7, 2024-10-11)"'#10'   sodi' +
          'podi:docname="desktop.svg"'#10'   xmlns:inkscape="http://www.inkscap' +
          'e.org/namespaces/inkscape"'#10'   xmlns:sodipodi="http://sodipodi.so' +
          'urceforge.net/DTD/sodipodi-0.dtd"'#10'   xmlns:xlink="http://www.w3.' +
          'org/1999/xlink"'#10'   xmlns="http://www.w3.org/2000/svg"'#10'   xmlns:s' +
          'vg="http://www.w3.org/2000/svg">'#10'  <sodipodi:namedview'#10'     id="' +
          'namedview1"'#10'     pagecolor="#ffffff"'#10'     bordercolor="#666666"'#10 +
          '     borderopacity="1.0"'#10'     inkscape:showpageshadow="2"'#10'     i' +
          'nkscape:pageopacity="0.0"'#10'     inkscape:pagecheckerboard="0"'#10'   ' +
          '  inkscape:deskcolor="#d1d1d1"'#10'     inkscape:document-units="px"' +
          #10'     showgrid="true"'#10'     showguides="true"'#10'     inkscape:zoom=' +
          '"19.4375"'#10'     inkscape:cx="23.974277"'#10'     inkscape:cy="24"'#10'   ' +
          '  inkscape:current-layer="layer1">'#10'    <inkscape:grid'#10'       id=' +
          '"grid1"'#10'       units="px"'#10'       originx="0"'#10'       originy="0"'#10 +
          '       spacingx="1"'#10'       spacingy="1"'#10'       empcolor="#0000ff' +
          '"'#10'       empopacity="0.25098039"'#10'       color="#0000ff"'#10'       o' +
          'pacity="0.1254902"'#10'       empspacing="5"'#10'       enabled="true"'#10' ' +
          '      visible="true" />'#10'    <sodipodi:guide'#10'       position="7.2' +
          '540193,52.527331"'#10'       orientation="0,-1"'#10'       id="guide1"'#10' ' +
          '      inkscape:locked="false" />'#10'  </sodipodi:namedview>'#10'  <defs' +
          #10'     id="defs1">'#10'    <linearGradient'#10'       id="linearGradient1' +
          '5"'#10'       inkscape:collect="always">'#10'      <stop'#10'         style=' +
          '"stop-color:#ffffff;stop-opacity:1;"'#10'         offset="0"'#10'       ' +
          '  id="stop14" />'#10'      <stop'#10'         style="stop-color:#ffffff;' +
          'stop-opacity:0.2;"'#10'         offset="1"'#10'         id="stop15" />'#10' ' +
          '   </linearGradient>'#10'    <linearGradient'#10'       id="linearGradie' +
          'nt5"'#10'       inkscape:collect="always">'#10'      <stop'#10'         styl' +
          'e="stop-color:#ffffff;stop-opacity:1;"'#10'         offset="0"'#10'     ' +
          '    id="stop5" />'#10'      <stop'#10'         style="stop-color:#ffffff' +
          ';stop-opacity:0.2;"'#10'         offset="1"'#10'         id="stop6" />'#10' ' +
          '   </linearGradient>'#10'    <linearGradient'#10'       id="linearGradie' +
          'nt2"'#10'       x1="0.5"'#10'       x2="0.5"'#10'       y1="0"'#10'       y2="1"' +
          '>'#10'      <stop'#10'         offset="0"'#10'         stop-color="#FFF"'#10'   ' +
          '      id="stop1"'#10'         style="stop-color:#ffffff;stop-opacity' +
          ':1;" />'#10'      <stop'#10'         offset="100%"'#10'         stop-color="' +
          '#FFF"'#10'         stop-opacity=".6"'#10'         id="stop2" />'#10'    </li' +
          'nearGradient>'#10'    <radialGradient'#10'       id="deepin-toggle-deskt' +
          'op-f"'#10'       cx="28.146683"'#10'       cy="31.120411"'#10'       r="57.2' +
          '97852"'#10'       fx="28.146683"'#10'       fy="31.120411"'#10'       gradie' +
          'ntTransform="matrix(0.55367834,1.3664361e-4,-1.2699918e-4,0.5141' +
          '2163,2.7390667,2.3086878)"'#10'       gradientUnits="userSpaceOnUse"' +
          '>'#10'      <stop'#10'         offset="0%"'#10'         stop-color="#6BB1DD"' +
          #10'         id="stop7" />'#10'      <stop'#10'         offset="0.13584685"' +
          #10'         stop-color="#60A0CE"'#10'         id="stop8" />'#10'      <sto' +
          'p'#10'         offset="100%"'#10'         stop-color="#32639A"'#10'         ' +
          'id="stop9" />'#10'    </radialGradient>'#10'    <filter'#10'       id="deepi' +
          'n-toggle-desktop-k"'#10'       width="1"'#10'       height="1.0833333"'#10' ' +
          '      x="0"'#10'       y="0"'#10'       filterUnits="objectBoundingBox">' +
          #10'      <feOffset'#10'         dy="1"'#10'         in="SourceAlpha"'#10'     ' +
          '    result="shadowOffsetOuter1"'#10'         id="feOffset13" />'#10'    ' +
          '  <feColorMatrix'#10'         in="shadowOffsetOuter1"'#10'         value' +
          's="0 0 0 0 0   0 0 0 0 0   0 0 0 0 0  0 0 0 0.1 0"'#10'         id="' +
          'feColorMatrix13" />'#10'    </filter>'#10'    <linearGradient'#10'       id=' +
          '"deepin-toggle-desktop-j"'#10'       x1="0.5"'#10'       x2="0.5"'#10'      ' +
          ' y1="0"'#10'       y2="1">'#10'      <stop'#10'         offset="0%"'#10'        ' +
          ' stop-color="#FFF"'#10'         id="stop12" />'#10'      <stop'#10'         ' +
          'offset="100%"'#10'         stop-color="#FFF"'#10'         stop-opacity="' +
          '.6"'#10'         id="stop13" />'#10'    </linearGradient>'#10'    <filter'#10'  ' +
          '     id="deepin-toggle-desktop-h"'#10'       width="1"'#10'       height' +
          '="1.0833333"'#10'       x="0"'#10'       y="0"'#10'       filterUnits="objec' +
          'tBoundingBox">'#10'      <feOffset'#10'         dy="1"'#10'         in="Sour' +
          'ceAlpha"'#10'         result="shadowOffsetOuter1"'#10'         id="feOff' +
          'set12" />'#10'      <feColorMatrix'#10'         in="shadowOffsetOuter1"'#10 +
          '         values="0 0 0 0 0   0 0 0 0 0   0 0 0 0 0  0 0 0 0.1 0"' +
          #10'         id="feColorMatrix12" />'#10'    </filter>'#10'    <linearGradi' +
          'ent'#10'       id="deepin-toggle-desktop-g"'#10'       x1="38.925964"'#10'  ' +
          '     x2="38.925964"'#10'       y1="2.0433881"'#10'       y2="75.527992"'#10 +
          '       gradientTransform="matrix(0.55248101,0,0,0.48544351,2.736' +
          '8674,2.8286611)"'#10'       gradientUnits="userSpaceOnUse">'#10'      <s' +
          'top'#10'         offset="0%"'#10'         stop-color="#005392"'#10'         ' +
          'stop-opacity=".557"'#10'         id="stop10" />'#10'      <stop'#10'        ' +
          ' offset="100%"'#10'         stop-color="#002D5B"'#10'         stop-opaci' +
          'ty=".855"'#10'         id="stop11" />'#10'    </linearGradient>'#10'    <lin' +
          'earGradient'#10'       inkscape:collect="always"'#10'       xlink:href="' +
          '#linearGradient5"'#10'       id="linearGradient6"'#10'       x1="13.8204' +
          '87"'#10'       y1="32.911709"'#10'       x2="20.17952"'#10'       y2="43.088' +
          '291"'#10'       gradientUnits="userSpaceOnUse" />'#10'    <filter'#10'      ' +
          ' id="deepin-toggle-desktop-k-7"'#10'       width="1"'#10'       height="' +
          '1.0833333"'#10'       x="0"'#10'       y="0"'#10'       filterUnits="objectB' +
          'oundingBox">'#10'      <feOffset'#10'         dy="1"'#10'         in="Source' +
          'Alpha"'#10'         result="shadowOffsetOuter1"'#10'         id="feOffse' +
          't13-6" />'#10'      <feColorMatrix'#10'         in="shadowOffsetOuter1"'#10 +
          '         values="0 0 0 0 0   0 0 0 0 0   0 0 0 0 0  0 0 0 0.1 0"' +
          #10'         id="feColorMatrix13-1" />'#10'    </filter>'#10'    <linearGra' +
          'dient'#10'       inkscape:collect="always"'#10'       xlink:href="#linea' +
          'rGradient15"'#10'       id="linearGradient7"'#10'       gradientUnits="u' +
          'serSpaceOnUse"'#10'       x1="13.820487"'#10'       y1="32.911709"'#10'     ' +
          '  x2="20.17952"'#10'       y2="43.088291" />'#10'  </defs>'#10'  <g'#10'     ink' +
          'scape:label="Desktop"'#10'     inkscape:groupmode="layer"'#10'     id="l' +
          'ayer1"'#10'     transform="matrix(1.0116888,0,0,1.0121586,-0.5334536' +
          '4,0.46513852)">'#10'    <rect'#10'       style="color:#000000;overflow:v' +
          'isible;fill:#cccccc;stroke:#999999;stroke-width:0.982445;stroke-' +
          'linecap:round;stroke-linejoin:round;stroke-dasharray:none;paint-' +
          'order:fill markers stroke"'#10'       id="rect1"'#10'       width="42.50' +
          '8961"'#10'       height="40.513256"'#10'       x="2.9955199"'#10'       y="2' +
          '.9955199"'#10'       rx="3.9537847"'#10'       ry="2.9639623" />'#10'    <pa' +
          'th'#10'       fill="url(#deepin-toggle-desktop-f)"'#10'       fill-rule=' +
          '"nonzero"'#10'       d="M 6.3211767,4.92378 H 42.164271 c 0.848382,0' +
          ' 1.536133,0.703512 1.536133,1.571339 V 38.969455 H 4.7850442 V 6' +
          '.495119 c 0,-0.867827 0.6877499,-1.571339 1.5361325,-1.571339 z"' +
          #10'       id="path15"'#10'       style="display:inline;fill:url(#deepi' +
          'n-toggle-desktop-f);stroke-width:0.517879" />'#10'    <path'#10'       f' +
          'ill="url(#deepin-toggle-desktop-g)"'#10'       fill-rule="nonzero"'#10' ' +
          '      d="M 42.164271,4.4 H 6.3211767 C 5.19,4.4 4.273,5.338017 4' +
          '.273,6.495119 V 39.493235 H 44.212448 V 6.495119 C 44.212448,5.3' +
          '38017 43.295448,4.4 42.164271,4.4 Z m 0,0.52378 c 0.848382,0 1.5' +
          '36133,0.703512 1.536133,1.571339 V 38.969455 H 4.7850442 V 6.495' +
          '119 c 0,-0.867827 0.6877499,-1.571339 1.5361325,-1.571339 z"'#10'   ' +
          '    id="path16"'#10'       style="display:inline;fill:url(#deepin-to' +
          'ggle-desktop-g);stroke-width:0.517879" />'#10'    <path'#10'       fill=' +
          '"#ffffff"'#10'       d="m 5.2970884,32.553498 c 0,-0.217146 0.173501' +
          '2,-0.393178 0.3851589,-0.393178 H 42.8032 c 0.212718,0 0.385159,' +
          '0.174667 0.385159,0.393178 v 5.499 c 0,0.217146 -0.173501,0.3931' +
          '78 -0.385159,0.393178 H 5.6822473 c -0.2127174,0 -0.3851589,-0.1' +
          '74667 -0.3851589,-0.393178 z"'#10'       opacity="0.334"'#10'       id="' +
          'path17"'#10'       style="display:inline;stroke-width:0.517879" />'#10' ' +
          '   <g'#10'       fill-rule="nonzero"'#10'       id="g20"'#10'       style="d' +
          'isplay:inline"'#10'       transform="matrix(0.5120442,0,0,0.52377962' +
          ',2.7368673,2.8286611)">'#10'      <path'#10'         id="use19"'#10'        ' +
          ' d="M 11,33.495545 C 11,32.669578 11.679555,32 12.495545,32 h 9.' +
          '00891 C 22.330422,32 23,32.679555 23,33.495545 v 9.00891 C 23,43' +
          '.330422 22.320446,44 21.504455,44 h -9.00891 C 11.669578,44 11,4' +
          '3.320445 11,42.504455 Z"'#10'         style="fill:#000000;filter:url' +
          '(#deepin-toggle-desktop-k)" />'#10'      <path'#10'         id="use20"'#10' ' +
          '        d="M 11,33.495545 C 11,32.669578 11.679555,32 12.495545,' +
          '32 h 9.00891 C 22.330422,32 23,32.679555 23,33.495545 v 9.00891 ' +
          'C 23,43.330422 22.320446,44 21.504455,44 h -9.00891 C 11.669578,' +
          '44 11,43.320445 11,42.504455 Z"'#10'         style="fill:url(#linear' +
          'Gradient6);font-variation-settings:normal;opacity:1;vector-effec' +
          't:none;fill-opacity:1;stroke-width:1.00000001;stroke-linecap:but' +
          't;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:non' +
          'e;stroke-dashoffset:0;stroke-opacity:1;-inkscape-stroke:none;sto' +
          'p-color:#000000;stop-opacity:1" />'#10'    </g>'#10'    <g'#10'       fill-r' +
          'ule="nonzero"'#10'       id="g20-2"'#10'       style="display:inline"'#10'  ' +
          '     transform="matrix(0.5120442,0,0,0.52377962,2.7368673,-8.170' +
          '7109)">'#10'      <path'#10'         id="use19-2"'#10'         d="M 11,33.49' +
          '5545 C 11,32.669578 11.679555,32 12.495545,32 h 9.00891 C 22.330' +
          '422,32 23,32.679555 23,33.495545 v 9.00891 C 23,43.330422 22.320' +
          '446,44 21.504455,44 h -9.00891 C 11.669578,44 11,43.320445 11,42' +
          '.504455 Z"'#10'         style="fill:#000000;filter:url(#deepin-toggl' +
          'e-desktop-k-7)" />'#10'      <path'#10'         id="use20-1"'#10'         d=' +
          '"M 11,33.495545 C 11,32.669578 11.679555,32 12.495545,32 h 9.008' +
          '91 C 22.330422,32 23,32.679555 23,33.495545 v 9.00891 C 23,43.33' +
          '0422 22.320446,44 21.504455,44 h -9.00891 C 11.669578,44 11,43.3' +
          '20445 11,42.504455 Z"'#10'         style="font-variation-settings:no' +
          'rmal;opacity:1;vector-effect:none;fill:url(#linearGradient7);fil' +
          'l-opacity:1;stroke-width:1;stroke-linecap:butt;stroke-linejoin:m' +
          'iter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset' +
          ':0;stroke-opacity:1;-inkscape-stroke:none;stop-color:#000000;sto' +
          'p-opacity:1" />'#10'    </g>'#10'  </g>'#10'</svg>'#10
      end
      item
        IconName = 'documents'
        SVGText = 
          '<?xml version="1.0" encoding="UTF-8" standalone="no"?>'#10'<!-- Crea' +
          'ted with Inkscape (http://www.inkscape.org/) -->'#10#10'<svg'#10'   height' +
          '="48.000000px"'#10'   id="svg97"'#10'   inkscape:version="1.4 (86a8ad7, ' +
          '2024-10-11)"'#10'   sodipodi:docname="documents.svg"'#10'   sodipodi:ver' +
          'sion="0.32"'#10'   width="48.000000px"'#10'   version="1.1"'#10'   xmlns:ink' +
          'scape="http://www.inkscape.org/namespaces/inkscape"'#10'   xmlns:sod' +
          'ipodi="http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd"'#10'   xm' +
          'lns:xlink="http://www.w3.org/1999/xlink"'#10'   xmlns="http://www.w3' +
          '.org/2000/svg"'#10'   xmlns:svg="http://www.w3.org/2000/svg">'#10'  <sod' +
          'ipodi:namedview'#10'     bordercolor="#666666"'#10'     borderopacity="0' +
          '.10196078"'#10'     fill="#729fcf"'#10'     id="base"'#10'     inkscape:curr' +
          'ent-layer="svg97"'#10'     inkscape:cx="24.026172"'#10'     inkscape:cy=' +
          '"24"'#10'     inkscape:document-units="px"'#10'     inkscape:grid-bbox="' +
          'true"'#10'     inkscape:pageopacity="0.0"'#10'     inkscape:pageshadow="' +
          '2"'#10'     inkscape:showpageshadow="false"'#10'     inkscape:window-hei' +
          'ght="1177"'#10'     inkscape:window-width="1841"'#10'     inkscape:windo' +
          'w-x="-8"'#10'     inkscape:window-y="-8"'#10'     inkscape:zoom="19.1041' +
          '67"'#10'     pagecolor="#ffffff"'#10'     showgrid="true"'#10'     stroke="#' +
          '3465a4"'#10'     inkscape:pagecheckerboard="0"'#10'     inkscape:deskcol' +
          'or="#d1d1d1"'#10'     inkscape:window-maximized="1">'#10'    <inkscape:g' +
          'rid'#10'       enabled="true"'#10'       id="grid2646"'#10'       type="xygr' +
          'id"'#10'       visible="true"'#10'       originx="0"'#10'       originy="0"'#10 +
          '       spacingy="1"'#10'       spacingx="1"'#10'       units="px" />'#10'  <' +
          '/sodipodi:namedview>'#10'  <defs'#10'     id="defs3">'#10'    <linearGradien' +
          't'#10'       id="linearGradient5048">'#10'      <stop'#10'         id="stop5' +
          '050"'#10'         offset="0"'#10'         style="stop-color:#0a0a0a;stop' +
          '-opacity:0" />'#10'      <stop'#10'         id="stop5056"'#10'         offse' +
          't="0.5"'#10'         style="stop-color:#0a0a0a;stop-opacity:1" />'#10'  ' +
          '    <stop'#10'         id="stop5052"'#10'         offset="1"'#10'         st' +
          'yle="stop-color:#0a0a0a;stop-opacity:0" />'#10'    </linearGradient>' +
          #10'    <linearGradient'#10'       id="linearGradient9766">'#10'      <stop' +
          #10'         id="stop9768"'#10'         offset="0"'#10'         style="stop' +
          '-color:#f8ce3c;stop-opacity:1" />'#10'      <stop'#10'         id="stop9' +
          '770"'#10'         offset="1"'#10'         style="stop-color:#f9dc53;stop' +
          '-opacity:1" />'#10'    </linearGradient>'#10'    <linearGradient'#10'       ' +
          'id="linearGradient3096">'#10'      <stop'#10'         id="stop3098"'#10'    ' +
          '     offset="0"'#10'         style="stop-color:#4e3030;stop-opacity:' +
          '1" />'#10'      <stop'#10'         id="stop3100"'#10'         offset="1.0000' +
          '000"'#10'         style="stop-color:#965d5d;stop-opacity:1.0000000" ' +
          '/>'#10'    </linearGradient>'#10'    <linearGradient'#10'       id="linearGr' +
          'adient319"'#10'       inkscape:collect="always">'#10'      <stop'#10'       ' +
          '  id="stop320"'#10'         offset="0"'#10'         style="stop-color:#f' +
          'fffff;stop-opacity:1" />'#10'      <stop'#10'         id="stop321"'#10'     ' +
          '    offset="1"'#10'         style="stop-color:#ffffff;stop-opacity:0' +
          '" />'#10'    </linearGradient>'#10'    <linearGradient'#10'       id="linear' +
          'Gradient1789">'#10'      <stop'#10'         id="stop1790"'#10'         offse' +
          't="0.0000000"'#10'         style="stop-color:#1f1313;stop-opacity:1.' +
          '0000000" />'#10'      <stop'#10'         id="stop1791"'#10'         offset="' +
          '1.0000000"'#10'         style="stop-color:#d6b0b0;stop-opacity:1.000' +
          '0000" />'#10'    </linearGradient>'#10'    <radialGradient'#10'       cx="20' +
          '.706017"'#10'       cy="37.517986"'#10'       fx="20.706017"'#10'       fy="' +
          '37.517986"'#10'       gradientTransform="matrix(-1.1867791,-0.027296' +
          '95,-0.19989555,1.1888345,54.498168,-0.16357931)"'#10'       gradient' +
          'Units="userSpaceOnUse"'#10'       id="radialGradient238"'#10'       inks' +
          'cape:collect="always"'#10'       r="30.905205"'#10'       xlink:href="#l' +
          'inearGradient1789" />'#10'    <linearGradient'#10'       gradientTransfo' +
          'rm="matrix(1.317489,0,0,0.816256,-0.879573,-1.318166)"'#10'       gr' +
          'adientUnits="userSpaceOnUse"'#10'       id="linearGradient322"'#10'     ' +
          '  inkscape:collect="always"'#10'       x1="13.035696"'#10'       x2="12.' +
          '853771"'#10'       xlink:href="#linearGradient319"'#10'       y1="32.567' +
          '184"'#10'       y2="46.689312" />'#10'    <linearGradient'#10'       gradien' +
          'tUnits="userSpaceOnUse"'#10'       id="linearGradient3104"'#10'       in' +
          'kscape:collect="always"'#10'       x1="18.112709"'#10'       x2="15.5148' +
          '89"'#10'       xlink:href="#linearGradient3096"'#10'       y1="31.367750' +
          '"'#10'       y2="6.1802502"'#10'       gradientTransform="matrix(-1.1248' +
          '856,0,0,0.99824126,50.479877,6.9491902)" />'#10'    <linearGradient'#10 +
          '       gradientUnits="userSpaceOnUse"'#10'       id="linearGradient9' +
          '772"'#10'       inkscape:collect="always"'#10'       x1="22.175976"'#10'    ' +
          '   x2="22.065331"'#10'       xlink:href="#linearGradient9766"'#10'      ' +
          ' y1="36.987999"'#10'       y2="32.050499" />'#10'    <linearGradient'#10'   ' +
          '    inkscape:collect="always"'#10'       xlink:href="#linearGradient' +
          '5529"'#10'       id="linearGradient11"'#10'       gradientUnits="userSpa' +
          'ceOnUse"'#10'       gradientTransform="matrix(0.9754464,0,0,1.060267' +
          '7,1.2620322,0.9029023)"'#10'       x1="11"'#10'       y1="17"'#10'       x2=' +
          '"11"'#10'       y2="-3" />'#10'    <linearGradient'#10'       id="linearGrad' +
          'ient5529"'#10'       inkscape:collect="always">'#10'      <stop'#10'        ' +
          ' id="stop5531"'#10'         offset="0"'#10'         style="stop-color:#f' +
          '0f0ee;stop-opacity:1" />'#10'      <stop'#10'         id="stop5533"'#10'    ' +
          '     offset="1"'#10'         style="stop-color:#dedede;stop-opacity:' +
          '1" />'#10'    </linearGradient>'#10'    <linearGradient'#10'       inkscape:' +
          'collect="always"'#10'       xlink:href="#linearGradient5529"'#10'       ' +
          'id="linearGradient10"'#10'       gradientUnits="userSpaceOnUse"'#10'    ' +
          '   gradientTransform="matrix(0.9754464,0,0,1.0602677,1.2620322,0' +
          '.9029023)"'#10'       x1="11"'#10'       y1="17"'#10'       x2="11"'#10'       y' +
          '2="-3" />'#10'  </defs>'#10'  <path'#10'     d="m 45.393393,45.568567 c -0.0' +
          '2452,0.415572 -0.517341,0.831144 -0.985637,0.831144 H 9.168442 c' +
          ' -0.468293,0 -0.912071,-0.415572 -0.887553,-0.831144 L 9.33428,1' +
          '8.389716 c 0.02452,-0.41557 0.517332,-0.831151 0.985626,-0.83115' +
          '1 H 25.24812 c 0.545634,0 1.388641,-0.315034 1.576689,-1.104686 ' +
          'l 0.687745,-2.887984 c 0.174885,-0.734379 0.99239,-1.03606 1.460' +
          '684,-1.03606 h 16.624527 c 0.468305,0 0.912081,0.415571 0.887563' +
          ',0.831143 z"'#10'     id="path216"'#10'     sodipodi:nodetypes="ccccccss' +
          'ssccc"'#10'     style="display:inline;fill:url(#radialGradient238);f' +
          'ill-opacity:1;fill-rule:nonzero;stroke:url(#linearGradient3104);' +
          'stroke-width:1.05967;stroke-linecap:round;stroke-linejoin:round;' +
          'stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1"'#10'    ' +
          ' inkscape:label="Folder-back" />'#10'  <g'#10'     id="g10"'#10'     transfo' +
          'rm="matrix(1.4736843,0,0,1.4736843,14.046413,-0.23684282)"'#10'     ' +
          'style="display:inline"'#10'     inkscape:label="Page">'#10'    <rect'#10'   ' +
          '    height="16.964285"'#10'       id="rect4"'#10'       rx="1.0178571"'#10' ' +
          '      ry="1.0178571"'#10'       style="color:#000000;display:inline;' +
          'overflow:visible;visibility:visible;opacity:1;fill:url(#linearGr' +
          'adient10);fill-opacity:1;fill-rule:nonzero;stroke:#888a85;stroke' +
          '-width:0.678571;stroke-linecap:butt;stroke-linejoin:miter;stroke' +
          '-miterlimit:10.433;stroke-dasharray:none;stroke-dashoffset:0;str' +
          'oke-opacity:1;marker:none;marker-start:none;marker-mid:none;mark' +
          'er-end:none;enable-background:accumulate"'#10'       width="15.60714' +
          '2"'#10'       x="3.7006481"'#10'       y="3.5535717" />'#10'    <rect'#10'      ' +
          ' height="15.607142"'#10'       id="rect5"'#10'       rx="0.33928567"'#10'   ' +
          '    ry="0.33928564"'#10'       style="color:#000000;display:inline;o' +
          'verflow:visible;visibility:visible;opacity:1;fill:none;fill-opac' +
          'ity:1;fill-rule:nonzero;stroke:#ffffff;stroke-width:0.678571;str' +
          'oke-linecap:butt;stroke-linejoin:miter;stroke-miterlimit:10.433;' +
          'stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1;marke' +
          'r:none;marker-start:none;marker-mid:none;marker-end:none;enable-' +
          'background:accumulate"'#10'       width="14.129219"'#10'       x="4.5"'#10' ' +
          '      y="4.2321429" />'#10'    <rect'#10'       height="1.002435"'#10'      ' +
          ' id="rect6"'#10'       style="color:#000000;display:inline;overflow:' +
          'visible;visibility:visible;opacity:1;fill:#91938e;fill-opacity:1' +
          ';fill-rule:nonzero;stroke:none;stroke-width:1.21543;stroke-linec' +
          'ap:butt;stroke-linejoin:miter;stroke-miterlimit:10.433;stroke-da' +
          'sharray:none;stroke-dashoffset:0;stroke-opacity:1;marker:none;ma' +
          'rker-start:none;marker-mid:none;marker-end:none;enable-backgroun' +
          'd:accumulate"'#10'       width="10.857142"'#10'       x="5.8905835"'#10'    ' +
          '   y="5.8206172"'#10'       inkscape:tile-cx="23.538462"'#10'       inks' +
          'cape:tile-cy="9.75"'#10'       inkscape:tile-w="27.076924"'#10'       in' +
          'kscape:tile-h="2.4999999"'#10'       inkscape:tile-x0="10"'#10'       in' +
          'kscape:tile-y0="8.5" />'#10'    <use'#10'       x="0"'#10'       y="0"'#10'     ' +
          '  inkscape:tiled-clone-of="#rect3"'#10'       xlink:href="#rect3"'#10'  ' +
          '     id="use6"'#10'       transform="matrix(1.0340909,0,0,1.6,-0.200' +
          '8152,-3.6928573)" />'#10'    <use'#10'       x="0"'#10'       y="0"'#10'       i' +
          'nkscape:tiled-clone-of="#rect3"'#10'       xlink:href="#rect3"'#10'     ' +
          '  id="use7"'#10'       transform="matrix(1.0340909,0,0,1.6,-0.200815' +
          '85,-0.88603969)" />'#10'    <use'#10'       x="0"'#10'       y="0"'#10'       in' +
          'kscape:tiled-clone-of="#rect3"'#10'       xlink:href="#rect3"'#10'      ' +
          ' id="use8"'#10'       transform="matrix(1.0340909,0,0,1.6,-0.2008158' +
          '5,1.9207783)" />'#10'    <use'#10'       x="0"'#10'       y="0"'#10'       inksc' +
          'ape:tiled-clone-of="#rect3"'#10'       xlink:href="#rect3"'#10'       id' +
          '="use9"'#10'       transform="matrix(1.0340909,0,0,1.6,-0.20081585,4' +
          '.7275963)" />'#10'    <use'#10'       x="0"'#10'       y="0"'#10'       inkscape' +
          ':tiled-clone-of="#rect3"'#10'       xlink:href="#rect3"'#10'       id="u' +
          'se10"'#10'       transform="matrix(0.73863634,0,0,1.6,1.5395839,7.53' +
          '44143)" />'#10'  </g>'#10'  <g'#10'     id="g11"'#10'     transform="matrix(1.47' +
          '36843,0,0,1.4736843,7.0464127,-3.7368431)"'#10'     style="display:i' +
          'nline"'#10'     inkscape:label="Page">'#10'    <rect'#10'       height="16.9' +
          '64285"'#10'       id="rect1"'#10'       rx="1.0178571"'#10'       ry="1.0178' +
          '571"'#10'       style="color:#000000;display:inline;overflow:visible' +
          ';visibility:visible;opacity:1;fill:url(#linearGradient11);fill-o' +
          'pacity:1;fill-rule:nonzero;stroke:#888a85;stroke-width:0.678571;' +
          'stroke-linecap:butt;stroke-linejoin:miter;stroke-miterlimit:10.4' +
          '33;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1;ma' +
          'rker:none;marker-start:none;marker-mid:none;marker-end:none;enab' +
          'le-background:accumulate"'#10'       width="15.607142"'#10'       x="3.7' +
          '006481"'#10'       y="3.5535717" />'#10'    <rect'#10'       height="15.6071' +
          '42"'#10'       id="rect2"'#10'       rx="0.33928567"'#10'       ry="0.339285' +
          '64"'#10'       style="color:#000000;display:inline;overflow:visible;' +
          'visibility:visible;opacity:1;fill:none;fill-opacity:1;fill-rule:' +
          'nonzero;stroke:#ffffff;stroke-width:0.678571;stroke-linecap:butt' +
          ';stroke-linejoin:miter;stroke-miterlimit:10.433;stroke-dasharray' +
          ':none;stroke-dashoffset:0;stroke-opacity:1;marker:none;marker-st' +
          'art:none;marker-mid:none;marker-end:none;enable-background:accum' +
          'ulate"'#10'       width="14.129219"'#10'       x="4.5"'#10'       y="4.23214' +
          '29" />'#10'    <rect'#10'       height="1.002435"'#10'       id="rect3"'#10'    ' +
          '   style="color:#000000;display:inline;overflow:visible;visibili' +
          'ty:visible;opacity:1;fill:#91938e;fill-opacity:1;fill-rule:nonze' +
          'ro;stroke:none;stroke-width:1.21543;stroke-linecap:butt;stroke-l' +
          'inejoin:miter;stroke-miterlimit:10.433;stroke-dasharray:none;str' +
          'oke-dashoffset:0;stroke-opacity:1;marker:none;marker-start:none;' +
          'marker-mid:none;marker-end:none;enable-background:accumulate"'#10'  ' +
          '     width="10.857142"'#10'       x="5.8905835"'#10'       y="5.8206172"' +
          #10'       inkscape:tile-cx="23.538462"'#10'       inkscape:tile-cy="9.' +
          '75"'#10'       inkscape:tile-w="27.076924"'#10'       inkscape:tile-h="2' +
          '.4999999"'#10'       inkscape:tile-x0="10"'#10'       inkscape:tile-y0="' +
          '8.5" />'#10'    <use'#10'       x="0"'#10'       y="0"'#10'       inkscape:tiled' +
          '-clone-of="#rect3"'#10'       xlink:href="#rect3"'#10'       id="use16"'#10 +
          '       transform="matrix(1.0340909,0,0,1.6,-0.2008152,-3.6928573' +
          ')" />'#10'    <use'#10'       x="0"'#10'       y="0"'#10'       inkscape:tiled-c' +
          'lone-of="#rect3"'#10'       xlink:href="#rect3"'#10'       id="use1"'#10'   ' +
          '    transform="matrix(1.0340909,0,0,1.6,-0.20081585,-0.88603969)' +
          '" />'#10'    <use'#10'       x="0"'#10'       y="0"'#10'       inkscape:tiled-cl' +
          'one-of="#rect3"'#10'       xlink:href="#rect3"'#10'       id="use2"'#10'    ' +
          '   transform="matrix(1.0340909,0,0,1.6,-0.20081585,1.9207783)" /' +
          '>'#10'    <use'#10'       x="0"'#10'       y="0"'#10'       inkscape:tiled-clone' +
          '-of="#rect3"'#10'       xlink:href="#rect3"'#10'       id="use4"'#10'       ' +
          'transform="matrix(1.0340909,0,0,1.6,-0.20081585,4.7275963)" />'#10' ' +
          '   <use'#10'       x="0"'#10'       y="0"'#10'       inkscape:tiled-clone-of' +
          '="#rect3"'#10'       xlink:href="#rect3"'#10'       id="use5"'#10'       tra' +
          'nsform="matrix(0.73863634,0,0,1.6,1.5395839,7.5344143)" />'#10'  </g' +
          '>'#10'  <g'#10'     id="layer1"'#10'     inkscape:groupmode="layer"'#10'     ink' +
          'scape:label="Folder"'#10'     transform="matrix(-1.1248856,0,0,0.998' +
          '24126,50.479878,6.9491902)">'#10'    <g'#10'       id="g220"'#10'       inks' +
          'cape:export-filename="/home/jimmac/ximian_art/icons/nautilus/sus' +
          'e93/gnome-fs-directory.png"'#10'       inkscape:export-xdpi="74.8000' +
          '03"'#10'       inkscape:export-ydpi="74.800003"'#10'       style="displa' +
          'y:inline;fill:#ffffff;fill-opacity:0.757062;fill-rule:nonzero;st' +
          'roke:none;stroke-width:0.999465;stroke-miterlimit:4"'#10'       tran' +
          'sform="matrix(1.1109385,0,0.05816673,1.040764,-11.678838,2.59446' +
          '05)" />'#10'    <path'#10'       d="m 39.783532,39.51062 c 1.143894,-0.0' +
          '4406 1.963076,-1.096299 2.047035,-2.321005 0.791787,-11.548687 1' +
          '.65936,-21.231949 1.65936,-21.231949 0.07215,-0.247484 -0.167911' +
          ',-0.494967 -0.48014,-0.494967 H 8.6386304 c 0,0 -1.8503191,21.86' +
          '6892 -1.8503191,21.866892 -0.1145551,0.982066 -0.4660075,1.80471' +
          '8 -1.5498358,2.183713 z"'#10'       id="path233"'#10'       inkscape:exp' +
          'ort-filename="/home/jimmac/ximian_art/icons/nautilus/suse93/gnom' +
          'e-fs-directory.png"'#10'       inkscape:export-xdpi="74.800003"'#10'    ' +
          '   inkscape:export-ydpi="74.800003"'#10'       sodipodi:nodetypes="c' +
          'scccscc"'#10'       style="color:#000000;display:inline;visibility:v' +
          'isible;fill:url(#linearGradient9772);fill-opacity:1;fill-rule:no' +
          'nzero;stroke:#bea41b;stroke-width:1;stroke-linecap:butt;stroke-l' +
          'inejoin:round;stroke-miterlimit:4;stroke-dasharray:none;stroke-d' +
          'ashoffset:0;stroke-opacity:1;marker-start:none;marker-mid:none;m' +
          'arker-end:none" />'#10'    <path'#10'       d="m 9.6202444,16.463921 32.' +
          '7910986,0.06481 -1.574046,20.001979 c -0.08432,1.071511 -0.45067' +
          '8,1.428215 -1.872656,1.428215 -1.871502,0 -28.677968,-0.03241 -3' +
          '1.394742,-0.03241 0.2335983,-0.320811 0.3337557,-0.988623 0.3350' +
          '963,-1.004612 z"'#10'       id="path304"'#10'       sodipodi:nodetypes="' +
          'ccsscsc"'#10'       style="display:inline;opacity:0.465909;fill:none' +
          ';fill-opacity:1;fill-rule:evenodd;stroke:url(#linearGradient322)' +
          ';stroke-width:1px;stroke-linecap:round;stroke-linejoin:miter;str' +
          'oke-opacity:1" />'#10'    <path'#10'       d="M 9.6202481,16.223182 8.45' +
          '36014,31.866453 c 0,0 8.2961546,-4.148078 18.6663476,-4.148078 1' +
          '0.370193,0 15.55529,-11.495193 15.55529,-11.495193 z"'#10'       id=' +
          '"path323"'#10'       sodipodi:nodetypes="ccccc"'#10'       style="displa' +
          'y:inline;fill:#ffffff;fill-opacity:0.0892857;fill-rule:evenodd;s' +
          'troke:none;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:' +
          'miter;stroke-opacity:1" />'#10'  </g>'#10'</svg>'#10
      end
      item
        IconName = 'net'
        SVGText = 
          '<?xml version="1.0" encoding="UTF-8" standalone="no"?>'#10'<!-- Crea' +
          'ted with Inkscape (http://www.inkscape.org/) -->'#10#10'<svg'#10'   width=' +
          '"48"'#10'   height="48"'#10'   viewBox="0 0 48 48"'#10'   version="1.1"'#10'   i' +
          'd="svg1"'#10'   inkscape:version="1.4 (86a8ad7, 2024-10-11)"'#10'   sodi' +
          'podi:docname="net.svg"'#10'   xmlns:inkscape="http://www.inkscape.or' +
          'g/namespaces/inkscape"'#10'   xmlns:sodipodi="http://sodipodi.source' +
          'forge.net/DTD/sodipodi-0.dtd"'#10'   xmlns:xlink="http://www.w3.org/' +
          '1999/xlink"'#10'   xmlns="http://www.w3.org/2000/svg"'#10'   xmlns:svg="' +
          'http://www.w3.org/2000/svg">'#10'  <sodipodi:namedview'#10'     id="name' +
          'dview1"'#10'     pagecolor="#ffffff"'#10'     bordercolor="#666666"'#10'    ' +
          ' borderopacity="1.0"'#10'     inkscape:showpageshadow="2"'#10'     inksc' +
          'ape:pageopacity="0.0"'#10'     inkscape:pagecheckerboard="0"'#10'     in' +
          'kscape:deskcolor="#d1d1d1"'#10'     inkscape:document-units="px"'#10'   ' +
          '  showgrid="true"'#10'     showguides="true"'#10'     inkscape:zoom="14.' +
          '407621"'#10'     inkscape:cx="24.015069"'#10'     inkscape:cy="23.980365' +
          '"'#10'     inkscape:current-layer="svg1">'#10'    <inkscape:grid'#10'       ' +
          'id="grid1"'#10'       units="px"'#10'       originx="0"'#10'       originy="' +
          '0"'#10'       spacingx="1"'#10'       spacingy="1"'#10'       empcolor="#000' +
          '0ff"'#10'       empopacity="0.25098039"'#10'       color="#0000ff"'#10'     ' +
          '  opacity="0.1254902"'#10'       empspacing="5"'#10'       enabled="true' +
          '"'#10'       visible="true" />'#10'  </sodipodi:namedview>'#10'  <defs'#10'     ' +
          'id="defs1">'#10'    <linearGradient'#10'       gradientTransform="matrix' +
          '(1,0,0,0.992781,0,-2.718035)"'#10'       gradientUnits="userSpaceOnU' +
          'se"'#10'       id="linearGradient2308"'#10'       inkscape:collect="alwa' +
          'ys"'#10'       x1="31.743324"'#10'       x2="31.86105"'#10'       xlink:href' +
          '="#linearGradient5137"'#10'       y1="37.842293"'#10'       y2="43.82579' +
          '" />'#10'    <linearGradient'#10'       id="linearGradient5137">'#10'      <' +
          'stop'#10'         id="stop5139"'#10'         offset="0"'#10'         style="' +
          'stop-color:#d6d6d4;stop-opacity:1" />'#10'      <stop'#10'         id="s' +
          'top5141"'#10'         offset="1"'#10'         style="stop-color:#b8b8b2;' +
          'stop-opacity:1" />'#10'    </linearGradient>'#10'    <linearGradient'#10'   ' +
          '    gradientTransform="matrix(0.999303,0,0,0.998527,0.00306125,-' +
          '2.971316)"'#10'       gradientUnits="userSpaceOnUse"'#10'       id="line' +
          'arGradient5147"'#10'       inkscape:collect="always"'#10'       x1="17.2' +
          '47635"'#10'       x2="39.904388"'#10'       xlink:href="#linearGradient5' +
          '137"'#10'       y1="6.3760414"'#10'       y2="38.876041" />'#10'    <radialG' +
          'radient'#10'       cx="24.006104"'#10'       cy="32.997028"'#10'       fx="2' +
          '4.006104"'#10'       fy="32.997028"'#10'       gradientTransform="matrix' +
          '(1.232634,0,0,0.778392,-5.590582,-0.847446)"'#10'       gradientUnit' +
          's="userSpaceOnUse"'#10'       id="radialGradient5239"'#10'       inkscap' +
          'e:collect="always"'#10'       r="19.00016"'#10'       xlink:href="#linea' +
          'rGradient5233" />'#10'    <linearGradient'#10'       id="linearGradient5' +
          '233">'#10'      <stop'#10'         id="stop5235"'#10'         offset="0"'#10'   ' +
          '      style="stop-color:#678fba;stop-opacity:1" />'#10'      <stop'#10' ' +
          '        id="stop5237"'#10'         offset="1"'#10'         style="stop-c' +
          'olor:#2d5785;stop-opacity:1" />'#10'    </linearGradient>'#10'    <linea' +
          'rGradient'#10'       gradientTransform="translate(-0.0235294,-3.0392' +
          '16)"'#10'       gradientUnits="userSpaceOnUse"'#10'       id="linearGrad' +
          'ient6246"'#10'       inkscape:collect="always"'#10'       x1="20.156862"' +
          #10'       x2="20.156862"'#10'       xlink:href="#linearGradient410"'#10'  ' +
          '     y1="5.0996137"'#10'       y2="26.039215" />'#10'    <linearGradient' +
          #10'       id="linearGradient3042">'#10'      <stop'#10'         id="stop30' +
          '44"'#10'         offset="0"'#10'         style="stop-color:black;stop-op' +
          'acity:0;" />'#10'      <stop'#10'         id="stop3050"'#10'         offset=' +
          '"0.5"'#10'         style="stop-color:black;stop-opacity:1;" />'#10'     ' +
          ' <stop'#10'         id="stop3046"'#10'         offset="1"'#10'         style' +
          '="stop-color:black;stop-opacity:0;" />'#10'    </linearGradient>'#10'   ' +
          ' <linearGradient'#10'       gradientTransform="matrix(1,0,0,0.753044' +
          ',-48,12.25251)"'#10'       gradientUnits="userSpaceOnUse"'#10'       id=' +
          '"linearGradient3442"'#10'       inkscape:collect="always"'#10'       x1=' +
          '"26.5"'#10'       x2="26.5"'#10'       xlink:href="#linearGradient3899"'#10 +
          '       y1="43.249905"'#10'       y2="35.75" />'#10'    <linearGradient'#10' ' +
          '      id="linearGradient3899"'#10'       inkscape:collect="always">'#10 +
          '      <stop'#10'         id="stop3901"'#10'         offset="0"'#10'         ' +
          'style="stop-color:#d6d6d4" />'#10'      <stop'#10'         id="stop3903"' +
          #10'         offset="1"'#10'         style="stop-color:#bec2ba" />'#10'    ' +
          '</linearGradient>'#10'    <linearGradient'#10'       gradientTransform="' +
          'matrix(1,0,0,0.833169,-48,19.08245)"'#10'       gradientUnits="userS' +
          'paceOnUse"'#10'       id="linearGradient3320"'#10'       inkscape:collec' +
          't="always"'#10'       x1="16.396038"'#10'       x2="16.396038"'#10'       xl' +
          'ink:href="#linearGradient1558"'#10'       y1="19.659277"'#10'       y2="' +
          '32.448051" />'#10'    <linearGradient'#10'       id="linearGradient1558"' +
          #10'       inkscape:collect="always">'#10'      <stop'#10'         id="stop' +
          '1560"'#10'         offset="0"'#10'         style="stop-color:#969a92;sto' +
          'p-opacity:1" />'#10'      <stop'#10'         id="stop1562"'#10'         offs' +
          'et="1"'#10'         style="stop-color:#d6d6d4" />'#10'    </linearGradie' +
          'nt>'#10'    <linearGradient'#10'       id="linearGradient8647">'#10'      <s' +
          'top'#10'         id="stop8649"'#10'         offset="0"'#10'         style="s' +
          'top-color:#8fb1dc;stop-opacity:1;" />'#10'      <stop'#10'         id="s' +
          'top8651"'#10'         offset="1"'#10'         style="stop-color:#3465a4;' +
          'stop-opacity:1;" />'#10'    </linearGradient>'#10'    <linearGradient'#10'  ' +
          '     id="linearGradient8924">'#10'      <stop'#10'         id="stop8926"' +
          #10'         offset="0"'#10'         style="stop-color:#cee14b" />'#10'    ' +
          '  <stop'#10'         id="stop8928"'#10'         offset="1"'#10'         styl' +
          'e="stop-color:#9db029" />'#10'    </linearGradient>'#10'    <linearGradi' +
          'ent'#10'       inkscape:collect="always"'#10'       id="linearGradient41' +
          '0">'#10'      <stop'#10'         id="stop6242-1"'#10'         offset="0"'#10'   ' +
          '      style="stop-color:#e6e6e6;stop-opacity:1" />'#10'      <stop'#10' ' +
          '        id="stop6244-1"'#10'         offset="1"'#10'         style="stop' +
          '-color:#e6e6e6;stop-opacity:0" />'#10'    </linearGradient>'#10'    <lin' +
          'earGradient'#10'       inkscape:collect="always"'#10'       id="linearGr' +
          'adient9618">'#10'      <stop'#10'         id="stop8742-1"'#10'         offse' +
          't="0"'#10'         style="stop-color:#f3ecec;stop-opacity:1" />'#10'    ' +
          '  <stop'#10'         id="stop8744-1"'#10'         offset="1"'#10'         st' +
          'yle="stop-color:#291a1a;stop-opacity:0" />'#10'    </linearGradient>' +
          #10'    <linearGradient'#10'       id="linearGradient8924-8">'#10'      <st' +
          'op'#10'         id="stop8926-9"'#10'         offset="0"'#10'         style="' +
          'stop-color:#fefaf7" />'#10'      <stop'#10'         id="stop8928-3"'#10'    ' +
          '     offset="1"'#10'         style="stop-color:#d48657" />'#10'    </lin' +
          'earGradient>'#10'    <linearGradient'#10'       id="linearGradient8740"'#10 +
          '       inkscape:collect="always">'#10'      <stop'#10'         id="stop8' +
          '742"'#10'         offset="0"'#10'         style="stop-color:#e8e8e8;stop' +
          '-opacity:1" />'#10'      <stop'#10'         id="stop8744"'#10'         offse' +
          't="1"'#10'         style="stop-color:#232323;stop-opacity:0" />'#10'    ' +
          '</linearGradient>'#10'    <radialGradient'#10'       cx="62.225391"'#10'    ' +
          '   cy="-3.4420195"'#10'       fx="62.225391"'#10'       fy="-3.4420195"'#10 +
          '       gradientTransform="matrix(0.891018,0,0,0.828854,1.579517,' +
          '2.39052)"'#10'       gradientUnits="userSpaceOnUse"'#10'       id="radia' +
          'lGradient12"'#10'       inkscape:collect="always"'#10'       r="10.08121' +
          '6"'#10'       xlink:href="#linearGradient8740" />'#10'    <radialGradien' +
          't'#10'       cx="24.652573"'#10'       cy="18.94449"'#10'       fx="24.65248' +
          '5"'#10'       fy="18.944481"'#10'       gradientTransform="matrix(0.0682' +
          '2876,2.459669,-1.754905,0.04868429,55.12882,-46.82188)"'#10'       g' +
          'radientUnits="userSpaceOnUse"'#10'       id="radialGradient18"'#10'     ' +
          '  inkscape:collect="always"'#10'       r="8.6174498"'#10'       xlink:hr' +
          'ef="#linearGradient8924-8" />'#10'    <radialGradient'#10'       cx="62.' +
          '225391"'#10'       cy="-3.4420195"'#10'       fx="62.225391"'#10'       fy="' +
          '-3.4420195"'#10'       gradientTransform="matrix(0.891018,0,0,0.8288' +
          '54,1.579517,2.39052)"'#10'       gradientUnits="userSpaceOnUse"'#10'    ' +
          '   id="radialGradient20"'#10'       inkscape:collect="always"'#10'      ' +
          ' r="10.081216"'#10'       xlink:href="#linearGradient8740" />'#10'    <l' +
          'inearGradient'#10'       id="linearGradient8930">'#10'      <stop'#10'      ' +
          '   id="stop8932"'#10'         offset="0"'#10'         style="stop-color:' +
          '#fffbfb" />'#10'      <stop'#10'         id="stop8934"'#10'         offset="' +
          '1"'#10'         style="stop-color:#de3900" />'#10'    </linearGradient>'#10 +
          '    <radialGradient'#10'       cx="62.225391"'#10'       cy="-3.4420195"' +
          #10'       fx="62.225391"'#10'       fy="-3.4420195"'#10'       gradientTra' +
          'nsform="matrix(0.891018,0,0,0.828854,1.579517,2.39052)"'#10'       g' +
          'radientUnits="userSpaceOnUse"'#10'       id="radialGradient26"'#10'     ' +
          '  inkscape:collect="always"'#10'       r="10.081216"'#10'       xlink:hr' +
          'ef="#linearGradient8740" />'#10'    <linearGradient'#10'       id="linea' +
          'rGradient8912">'#10'      <stop'#10'         id="stop8914"'#10'         offs' +
          'et="0"'#10'         style="stop-color:#340700" />'#10'      <stop'#10'      ' +
          '   id="stop8916"'#10'         offset="1"'#10'         style="stop-color:' +
          '#fbdad4" />'#10'    </linearGradient>'#10'    <radialGradient'#10'       ink' +
          'scape:collect="always"'#10'       xlink:href="#linearGradient8924-8"' +
          #10'       id="radialGradient30"'#10'       gradientUnits="userSpaceOnU' +
          'se"'#10'       gradientTransform="matrix(0.17026344,0.3209248,-0.393' +
          '04241,0.20852477,20.433504,-4.0602221)"'#10'       cx="21.769932"'#10'  ' +
          '     cy="23.844812"'#10'       fx="21.769844"'#10'       fy="23.844803"'#10 +
          '       r="8.6174498" />'#10'    <radialGradient'#10'       cx="62.225391' +
          '"'#10'       cy="-3.4420195"'#10'       fx="62.225391"'#10'       fy="-3.442' +
          '0195"'#10'       gradientTransform="matrix(0.891018,0,0,0.828854,1.5' +
          '79517,2.39052)"'#10'       gradientUnits="userSpaceOnUse"'#10'       id=' +
          '"radialGradient32"'#10'       inkscape:collect="always"'#10'       r="10' +
          '.081216"'#10'       xlink:href="#linearGradient8740" />'#10'    <linearG' +
          'radient'#10'       id="linearGradient8918">'#10'      <stop'#10'         id=' +
          '"stop8920"'#10'         offset="0"'#10'         style="stop-color:#ffb2a' +
          '3" />'#10'      <stop'#10'         id="stop8922"'#10'         offset="1"'#10'   ' +
          '      style="stop-color:#601600" />'#10'    </linearGradient>'#10'    <r' +
          'adialGradient'#10'       cx="24.652573"'#10'       cy="18.94449"'#10'       ' +
          'fx="24.652485"'#10'       fy="18.944481"'#10'       gradientTransform="m' +
          'atrix(0.06822876,2.459669,-1.754905,0.04868429,55.12882,-46.8218' +
          '8)"'#10'       gradientUnits="userSpaceOnUse"'#10'       id="radialGradi' +
          'ent36"'#10'       inkscape:collect="always"'#10'       r="8.6174498"'#10'   ' +
          '    xlink:href="#linearGradient8924-8" />'#10'    <radialGradient'#10'  ' +
          '     cx="62.225391"'#10'       cy="-3.4420195"'#10'       fx="62.225391"' +
          #10'       fy="-3.4420195"'#10'       gradientTransform="matrix(0.89101' +
          '8,0,0,0.828854,1.579517,2.39052)"'#10'       gradientUnits="userSpac' +
          'eOnUse"'#10'       id="radialGradient38"'#10'       inkscape:collect="al' +
          'ways"'#10'       r="10.081216"'#10'       xlink:href="#linearGradient874' +
          '0" />'#10'    <radialGradient'#10'       cx="62.200352"'#10'       cy="-8.72' +
          '56308"'#10'       fx="62.200352"'#10'       fy="-8.7256308"'#10'       gradi' +
          'entTransform="matrix(1.122354,0,0,1.122379,-7.610472,1.067717)"'#10 +
          '       gradientUnits="userSpaceOnUse"'#10'       id="radialGradient9' +
          '171-8"'#10'       inkscape:collect="always"'#10'       r="9.7552834"'#10'   ' +
          '    xlink:href="#linearGradient8647" />'#10'    <linearGradient'#10'    ' +
          '   gradientUnits="userSpaceOnUse"'#10'       id="linearGradient4879-' +
          '8"'#10'       inkscape:collect="always"'#10'       x1="63.397362"'#10'      ' +
          ' x2="68.910904"'#10'       xlink:href="#linearGradient4873-9"'#10'      ' +
          ' y1="-9.3832779"'#10'       y2="16.839214" />'#10'    <linearGradient'#10'  ' +
          '     id="linearGradient4873-9"'#10'       inkscape:collect="always">' +
          #10'      <stop'#10'         id="stop4875-7"'#10'         offset="0"'#10'      ' +
          '   style="stop-color:#ffffff;stop-opacity:0.75479454;" />'#10'      ' +
          '<stop'#10'         id="stop4877-2"'#10'         offset="1"'#10'         styl' +
          'e="stop-color:#ffffff;stop-opacity:0;" />'#10'    </linearGradient>'#10 +
          '    <radialGradient'#10'       cx="62.225391"'#10'       cy="-3.4420195"' +
          #10'       fx="62.225391"'#10'       fy="-3.4420195"'#10'       gradientTra' +
          'nsform="matrix(1,0,0,0.930233,0,-0.240141)"'#10'       gradientUnits' +
          '="userSpaceOnUse"'#10'       id="radialGradient8760-2"'#10'       inksca' +
          'pe:collect="always"'#10'       r="10.081216"'#10'       xlink:href="#lin' +
          'earGradient9618" />'#10'    <radialGradient'#10'       inkscape:collect=' +
          '"always"'#10'       xlink:href="#linearGradient8924-8"'#10'       id="ra' +
          'dialGradient2-8"'#10'       gradientUnits="userSpaceOnUse"'#10'       gr' +
          'adientTransform="matrix(0.05810032,0.27758753,-0.2018663,0.04225' +
          '153,29.479003,4.2270229)"'#10'       cx="23.341396"'#10'       cy="16.66' +
          '8016"'#10'       fx="23.341309"'#10'       fy="16.668007"'#10'       r="8.61' +
          '74498" />'#10'    <radialGradient'#10'       inkscape:collect="always"'#10' ' +
          '      xlink:href="#linearGradient8740"'#10'       id="radialGradient' +
          '12-8"'#10'       gradientUnits="userSpaceOnUse"'#10'       gradientTrans' +
          'form="matrix(0.891018,0,0,0.828854,1.579517,2.39052)"'#10'       cx=' +
          '"62.225391"'#10'       cy="-3.4420195"'#10'       fx="62.225391"'#10'       ' +
          'fy="-3.4420195"'#10'       r="10.081216" />'#10'    <radialGradient'#10'    ' +
          '   cx="62.225391"'#10'       cy="-3.4420195"'#10'       fx="62.225391"'#10' ' +
          '      fy="-3.4420195"'#10'       gradientTransform="matrix(1,0,0,0.9' +
          '30233,0,-0.240141)"'#10'       gradientUnits="userSpaceOnUse"'#10'      ' +
          ' id="radialGradient8766-6"'#10'       inkscape:collect="always"'#10'    ' +
          '   r="10.081216"'#10'       xlink:href="#linearGradient8740" />'#10'    ' +
          '<radialGradient'#10'       inkscape:collect="always"'#10'       xlink:hr' +
          'ef="#linearGradient8924-8"'#10'       id="radialGradient3-1"'#10'       ' +
          'gradientUnits="userSpaceOnUse"'#10'       gradientTransform="matrix(' +
          '0.0609926,0.42355167,-0.43609993,0.06279964,33.978965,-0.2131085' +
          ')"'#10'       cx="22.99729"'#10'       cy="16.162762"'#10'       fx="22.9972' +
          '02"'#10'       fy="16.162752"'#10'       r="8.6174498" />'#10'    <radialGra' +
          'dient'#10'       inkscape:collect="always"'#10'       xlink:href="#linea' +
          'rGradient8740"'#10'       id="radialGradient13"'#10'       gradientUnits' +
          '="userSpaceOnUse"'#10'       gradientTransform="matrix(0.891018,0,0,' +
          '0.828854,1.579517,2.39052)"'#10'       cx="62.225391"'#10'       cy="-3.' +
          '4420195"'#10'       fx="62.225391"'#10'       fy="-3.4420195"'#10'       r="' +
          '10.081216" />'#10'    <radialGradient'#10'       cx="62.225391"'#10'       c' +
          'y="-3.4420195"'#10'       fx="62.225391"'#10'       fy="-3.4420195"'#10'    ' +
          '   gradientTransform="matrix(1,0,0,0.930233,0,-0.240141)"'#10'      ' +
          ' gradientUnits="userSpaceOnUse"'#10'       id="radialGradient8774-2"' +
          #10'       inkscape:collect="always"'#10'       r="10.081216"'#10'       xl' +
          'ink:href="#linearGradient9618" />'#10'    <radialGradient'#10'       cx=' +
          '"62.225391"'#10'       cy="-3.4420195"'#10'       fx="62.225391"'#10'       ' +
          'fy="-3.4420195"'#10'       gradientTransform="matrix(1,0,0,0.930233,' +
          '0,-0.240141)"'#10'       gradientUnits="userSpaceOnUse"'#10'       id="r' +
          'adialGradient8782-4"'#10'       inkscape:collect="always"'#10'       r="' +
          '10.081216"'#10'       xlink:href="#linearGradient8740" />'#10'    <radia' +
          'lGradient'#10'       inkscape:collect="always"'#10'       xlink:href="#l' +
          'inearGradient8924-8"'#10'       id="radialGradient14"'#10'       gradien' +
          'tUnits="userSpaceOnUse"'#10'       gradientTransform="matrix(0.06822' +
          '876,2.459669,-1.754905,0.04868429,55.12882,-46.82188)"'#10'       cx' +
          '="24.652573"'#10'       cy="18.94449"'#10'       fx="24.652485"'#10'       f' +
          'y="18.944481"'#10'       r="8.6174498" />'#10'    <radialGradient'#10'      ' +
          ' inkscape:collect="always"'#10'       xlink:href="#linearGradient874' +
          '0"'#10'       id="radialGradient15"'#10'       gradientUnits="userSpaceO' +
          'nUse"'#10'       gradientTransform="matrix(0.891018,0,0,0.828854,1.5' +
          '79517,2.39052)"'#10'       cx="62.225391"'#10'       cy="-3.4420195"'#10'   ' +
          '    fx="62.225391"'#10'       fy="-3.4420195"'#10'       r="10.081216" /' +
          '>'#10'    <radialGradient'#10'       cx="24.652573"'#10'       cy="18.94449"' +
          #10'       fx="24.652485"'#10'       fy="18.944481"'#10'       gradientTran' +
          'sform="matrix(0.07657394,2.760516,-1.969551,0.05463895,60.09901,' +
          '-55.47179)"'#10'       gradientUnits="userSpaceOnUse"'#10'       id="rad' +
          'ialGradient9185-2"'#10'       inkscape:collect="always"'#10'       r="8.' +
          '6174498"'#10'       xlink:href="#linearGradient8924-8" />'#10'    <radia' +
          'lGradient'#10'       cx="62.225391"'#10'       cy="-3.4420195"'#10'       fx' +
          '="62.225391"'#10'       fy="-3.4420195"'#10'       gradientTransform="ma' +
          'trix(1,0,0,0.930233,0,-0.240141)"'#10'       gradientUnits="userSpac' +
          'eOnUse"'#10'       id="radialGradient8812-5"'#10'       inkscape:collect' +
          '="always"'#10'       r="10.081216"'#10'       xlink:href="#linearGradien' +
          't8740" />'#10'    <radialGradient'#10'       inkscape:collect="always"'#10' ' +
          '      xlink:href="#linearGradient8924-8"'#10'       id="radialGradie' +
          'nt16"'#10'       gradientUnits="userSpaceOnUse"'#10'       gradientTrans' +
          'form="matrix(0.06822876,2.459669,-1.754905,0.04868429,55.12882,-' +
          '46.82188)"'#10'       cx="24.652573"'#10'       cy="18.94449"'#10'       fx=' +
          '"24.652485"'#10'       fy="18.944481"'#10'       r="8.6174498" />'#10'    <r' +
          'adialGradient'#10'       inkscape:collect="always"'#10'       xlink:href' +
          '="#linearGradient8740"'#10'       id="radialGradient17"'#10'       gradi' +
          'entUnits="userSpaceOnUse"'#10'       gradientTransform="matrix(0.891' +
          '018,0,0,0.828854,1.579517,2.39052)"'#10'       cx="62.225391"'#10'      ' +
          ' cy="-3.4420195"'#10'       fx="62.225391"'#10'       fy="-3.4420195"'#10'  ' +
          '     r="10.081216" />'#10'    <radialGradient'#10'       cx="25.135374"'#10 +
          '       cy="14.542349"'#10'       fx="25.135332"'#10'       fy="14.542329' +
          '"'#10'       gradientTransform="matrix(0.159592,5.753335,-0.8072,0.0' +
          '223703,32.87305,-131.6974)"'#10'       gradientUnits="userSpaceOnUse' +
          '"'#10'       id="radialGradient9191-8"'#10'       inkscape:collect="alwa' +
          'ys"'#10'       r="4.1347499"'#10'       xlink:href="#linearGradient8930"' +
          ' />'#10'    <radialGradient'#10'       cx="62.225391"'#10'       cy="-3.4420' +
          '195"'#10'       fx="62.225391"'#10'       fy="-3.4420195"'#10'       gradien' +
          'tTransform="matrix(1,0,0,0.930233,0,-0.240141)"'#10'       gradientU' +
          'nits="userSpaceOnUse"'#10'       id="radialGradient8816-6"'#10'       in' +
          'kscape:collect="always"'#10'       r="10.081216"'#10'       xlink:href="' +
          '#linearGradient9618" />'#10'    <radialGradient'#10'       inkscape:coll' +
          'ect="always"'#10'       xlink:href="#linearGradient8924-8"'#10'       id' +
          '="radialGradient1-5"'#10'       gradientUnits="userSpaceOnUse"'#10'     ' +
          '  gradientTransform="matrix(-0.01411022,1.4125753,-1.7554926,-0.' +
          '01753277,56.744175,-24.939229)"'#10'       cx="19.47043"'#10'       cy="' +
          '22.494011"'#10'       fx="19.470343"'#10'       fy="22.494001"'#10'       r=' +
          '"8.6174498" />'#10'    <radialGradient'#10'       inkscape:collect="alwa' +
          'ys"'#10'       xlink:href="#linearGradient8740"'#10'       id="radialGra' +
          'dient18-3"'#10'       gradientUnits="userSpaceOnUse"'#10'       gradient' +
          'Transform="matrix(0.891018,0,0,0.828854,1.579517,2.39052)"'#10'     ' +
          '  cx="62.225391"'#10'       cy="-3.4420195"'#10'       fx="62.225391"'#10'  ' +
          '     fy="-3.4420195"'#10'       r="10.081216" />'#10'    <radialGradient' +
          #10'       cx="24.478569"'#10'       cy="17.573915"'#10'       fx="24.47853' +
          '9"'#10'       fy="17.573889"'#10'       gradientTransform="matrix(0.2220' +
          '34,8.004376,-0.597156,0.01656095,29.5454,-182.3268)"'#10'       grad' +
          'ientUnits="userSpaceOnUse"'#10'       id="radialGradient9197-9"'#10'    ' +
          '   inkscape:collect="always"'#10'       r="2.9719501"'#10'       xlink:h' +
          'ref="#linearGradient8912" />'#10'    <radialGradient'#10'       cx="62.2' +
          '25391"'#10'       cy="-3.4420195"'#10'       fx="62.225391"'#10'       fy="-' +
          '3.4420195"'#10'       gradientTransform="matrix(1,0,0,0.930233,0,-0.' +
          '240141)"'#10'       gradientUnits="userSpaceOnUse"'#10'       id="radial' +
          'Gradient8820-6"'#10'       inkscape:collect="always"'#10'       r="10.08' +
          '1216"'#10'       xlink:href="#linearGradient9618" />'#10'    <radialGrad' +
          'ient'#10'       inkscape:collect="always"'#10'       xlink:href="#linear' +
          'Gradient8924-8"'#10'       id="radialGradient4-1"'#10'       gradientUni' +
          'ts="userSpaceOnUse"'#10'       gradientTransform="matrix(0.17026344,' +
          '0.3209248,-0.39304241,0.20852477,20.433504,-4.0602221)"'#10'       c' +
          'x="21.769932"'#10'       cy="23.844812"'#10'       fx="21.769844"'#10'      ' +
          ' fy="23.844803"'#10'       r="8.6174498" />'#10'    <radialGradient'#10'    ' +
          '   inkscape:collect="always"'#10'       xlink:href="#linearGradient8' +
          '740"'#10'       id="radialGradient19"'#10'       gradientUnits="userSpac' +
          'eOnUse"'#10'       gradientTransform="matrix(0.891018,0,0,0.828854,1' +
          '.579517,2.39052)"'#10'       cx="62.225391"'#10'       cy="-3.4420195"'#10' ' +
          '      fx="62.225391"'#10'       fy="-3.4420195"'#10'       r="10.081216"' +
          ' />'#10'    <radialGradient'#10'       cx="25.969097"'#10'       cy="17.2578' +
          '54"'#10'       fx="25.968998"'#10'       fy="17.257843"'#10'       gradientT' +
          'ransform="matrix(0.06718136,2.42191,-1.629357,0.0451789,52.36869' +
          ',-50.34012)"'#10'       gradientUnits="userSpaceOnUse"'#10'       id="ra' +
          'dialGradient9203-8"'#10'       inkscape:collect="always"'#10'       r="9' +
          '.8222504"'#10'       xlink:href="#linearGradient8918" />'#10'    <radial' +
          'Gradient'#10'       cx="62.225391"'#10'       cy="-3.4420195"'#10'       fx=' +
          '"62.225391"'#10'       fy="-3.4420195"'#10'       gradientTransform="mat' +
          'rix(1,0,0,0.930233,0,-0.240141)"'#10'       gradientUnits="userSpace' +
          'OnUse"'#10'       id="radialGradient8824-1"'#10'       inkscape:collect=' +
          '"always"'#10'       r="10.081216"'#10'       xlink:href="#linearGradient' +
          '9618" />'#10'    <radialGradient'#10'       cx="24.652573"'#10'       cy="18' +
          '.94449"'#10'       fx="24.652485"'#10'       fy="18.944481"'#10'       gradi' +
          'entTransform="matrix(0.06822876,2.459669,-1.754905,0.04868429,55' +
          '.12882,-46.82188)"'#10'       gradientUnits="userSpaceOnUse"'#10'       ' +
          'id="radialGradient9187-9"'#10'       inkscape:collect="always"'#10'     ' +
          '  r="8.6174498"'#10'       xlink:href="#linearGradient8924-8" />'#10'   ' +
          ' <radialGradient'#10'       cx="62.225391"'#10'       cy="-3.4420195"'#10'  ' +
          '     fx="62.225391"'#10'       fy="-3.4420195"'#10'       gradientTransf' +
          'orm="matrix(0.891018,0,0,0.828854,1.579517,2.39052)"'#10'       grad' +
          'ientUnits="userSpaceOnUse"'#10'       id="radialGradient9189-7"'#10'    ' +
          '   inkscape:collect="always"'#10'       r="10.081216"'#10'       xlink:h' +
          'ref="#linearGradient8740" />'#10'  </defs>'#10'  <g'#10'     id="g8936-2"'#10'  ' +
          '   style="display:inline"'#10'     transform="matrix(1.5809684,0,0,1' +
          '.5809684,-66.336795,21.550849)"'#10'     inkscape:label="World">'#10'   ' +
          ' <path'#10'       d="m 71.455637,-3.5111605 c 0,5.1118327 -4.144037,' +
          '9.255822 -9.255167,9.255822 -5.111598,0 -9.2554,-4.1440362 -9.25' +
          '54,-9.255822 0,-5.1115983 4.143802,-9.2551665 9.2554,-9.2551665 ' +
          '5.11113,0 9.255167,4.1435682 9.255167,9.2551665 z"'#10'       id="pa' +
          'th6495-9"'#10'       style="display:inline;fill:url(#radialGradient9' +
          '171-8);fill-opacity:1;fill-rule:nonzero;stroke:#2e67b7;stroke-wi' +
          'dth:0.465144;stroke-miterlimit:4;stroke-dasharray:none;stroke-op' +
          'acity:1" />'#10'    <path'#10'       d="m 70.945908,-3.5111451 c 0,4.830' +
          '2718 -3.915782,8.7460091 -8.74539,8.7460091 -4.83005,0 -8.745611' +
          ',-3.9157817 -8.745611,-8.7460091 0,-4.8300503 3.915561,-8.745389' +
          '9 8.745611,-8.7453899 4.829608,0 8.74539,3.9153396 8.74539,8.745' +
          '3899 z"'#10'       id="path8655-5"'#10'       style="display:inline;opac' +
          'ity:1;fill:none;fill-opacity:1;fill-rule:nonzero;stroke:url(#lin' +
          'earGradient4879-8);stroke-width:0.465144;stroke-miterlimit:4;str' +
          'oke-dasharray:none;stroke-opacity:1" />'#10'    <g'#10'       id="g6548-' +
          '2"'#10'       style="display:inline;fill:#9b2710;fill-rule:nonzero;s' +
          'troke:url(#radialGradient8760-2);stroke-miterlimit:4"'#10'       tra' +
          'nsform="matrix(0.468894,0,0,0.468894,49.7717,-14.57365)">'#10'      ' +
          '<path'#10'         d="m 28.833,12.7749 -0.291,-0.7412 -0.5098,0.165 ' +
          '0.1465,0.9043 0.6543,-0.3281"'#10'         id="path6550-0"'#10'         ' +
          'style="font-variation-settings:normal;opacity:1;vector-effect:no' +
          'ne;fill:url(#radialGradient2-8);fill-opacity:1;stroke:url(#radia' +
          'lGradient12-8);stroke-width:1;stroke-linecap:butt;stroke-linejoi' +
          'n:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoff' +
          'set:0;stroke-opacity:1;-inkscape-stroke:none;stop-color:#000000;' +
          'stop-opacity:1" />'#10'    </g>'#10'    <g'#10'       id="g6556-0"'#10'       st' +
          'yle="display:inline;fill:#f2dacb;fill-rule:nonzero;stroke:url(#r' +
          'adialGradient8766-6);stroke-miterlimit:4"'#10'       transform="matr' +
          'ix(0.468894,0,0,0.468894,49.94848,-14.57365)">'#10'      <path'#10'     ' +
          '    d="m 29.123,12.6089 -0.1455,0.9883 0.7998,-0.165 0.5811,-0.5' +
          '752 -0.5088,-0.4941 C 29.6787,11.9078 29.4824,11.483 29.2685,11.' +
          '0465 H 28.833 v 0.4932 l 0.29,0.3291 v 0.7402"'#10'         id="path' +
          '6558-3"'#10'         style="font-variation-settings:normal;opacity:1' +
          ';vector-effect:none;fill:url(#radialGradient3-1);fill-opacity:1;' +
          'stroke:url(#radialGradient13);stroke-width:1;stroke-linecap:butt' +
          ';stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none' +
          ';stroke-dashoffset:0;stroke-opacity:1;-inkscape-stroke:none;stop' +
          '-color:#000000;stop-opacity:1" />'#10'    </g>'#10'    <g'#10'       id="g65' +
          '72-9"'#10'       style="display:inline;fill:#f6a08f;fill-rule:nonzer' +
          'o;stroke:url(#radialGradient8774-2);stroke-miterlimit:4"'#10'       ' +
          'transform="matrix(0.468894,0,0,0.468894,50.39042,-14.57365)" />'#10 +
          '    <g'#10'       id="g6608-1"'#10'       style="display:inline;fill:#da' +
          '976f;fill-rule:nonzero;stroke:url(#radialGradient8782-4);stroke-' +
          'miterlimit:4"'#10'       transform="matrix(0.468894,0,0,0.468894,50.' +
          '96494,-14.52946)">'#10'      <path'#10'         d="m 17.4922,7.887132 0.' +
          '3638,-0.3286 0.7271,-0.1646 c 0.498,-0.2422 0.998,-0.4053 1.5264' +
          ',-0.5762 l -0.29,-0.4937 -0.9385,0.1348 -0.4434,0.4419 -0.731,0.' +
          '106 -0.6499,0.3052 -0.3159,0.1528 -0.1929,0.2583 0.9443,0.1641"'#10 +
          '         id="path6610-8"'#10'         style="font-variation-settings' +
          ':normal;opacity:1;vector-effect:none;fill:url(#radialGradient14)' +
          ';fill-opacity:1;stroke:url(#radialGradient15);stroke-width:1;str' +
          'oke-linecap:butt;stroke-linejoin:miter;stroke-miterlimit:4;strok' +
          'e-dasharray:none;stroke-dashoffset:0;stroke-opacity:1;-inkscape-' +
          'stroke:none;stop-color:#000000;stop-opacity:1" />'#10'    </g>'#10'    <' +
          'g'#10'       id="g6564-1"'#10'       style="display:inline;fill:url(#rad' +
          'ialGradient9185-2);fill-opacity:1;fill-rule:nonzero;stroke:url(#' +
          'radialGradient8812-5);stroke-miterlimit:4"'#10'       transform="mat' +
          'rix(0.468894,0,0,0.468894,50.74397,-14.61784)">'#10'      <path'#10'    ' +
          '     d="m 17.943241,27.768799 -0.518573,-1.02672 -0.971477,-0.21' +
          '9726 -0.518127,-1.392215 -1.295183,0.146216 -1.100764,-0.805748 ' +
          '-1.166432,1.025918 v 0.161809 C 12.019842,25.55649 11.586095,25.' +
          '54259 11.271922,25.349417 l -0.259287,-0.732684 v -0.806638 l -0' +
          '.777056,0.07306 c 0.06487,-0.513404 0.129197,-1.025918 0.194509,' +
          '-1.539233 H 9.9762924 L 9.523388,22.930393 9.0695925,23.149672 8' +
          '.4217333,22.784177 8.3568672,21.977538 8.4865103,21.097836 9.458' +
          '4328,20.365152 H 10.23549 l 0.129197,-0.440252 0.971477,0.219279' +
          ' 0.712636,0.880593 0.129643,-1.467061 1.230764,-1.025918 0.45335' +
          ',-1.099872 0.906699,-0.366298 0.518127,-0.732684 1.165541,-0.220' +
          '973 0.583439,-0.878811 c -0.582993,0 -1.165987,0 -1.74898,0 l 1.' +
          '10112,-0.513404 h 0.776612 l 1.101565,-0.367189 0.129643,-0.4385' +
          '59 -0.388929,-0.367188 -0.45335,-0.147018 0.129643,-0.43945 -0.3' +
          '23707,-0.659532 -0.777502,0.292343 0.129643,-0.586022 -0.9067,-0' +
          '.513405 -0.71219,1.245554 0.06442,0.440252 -0.71219,0.294125 -0.' +
          '453796,0.952766 -0.194064,-0.879702 -1.230763,-0.513405 -0.19450' +
          '9,-0.659531 1.619336,-0.953657 0.712636,-0.659532 0.06487,-0.806' +
          '1926 -0.388483,-0.2201702 -0.518127,-0.073509 -0.323707,0.806637' +
          '8 c 0,0 -0.54165,0.106121 -0.680916,0.140514 -1.778561,1.638938 ' +
          '-5.3722151,5.176904 -6.207099,11.856064 0.033057,0.154859 0.6051' +
          '795,1.052827 0.6051795,1.052827 l 1.3600498,0.805747 1.3600499,0' +
          '.367189 0.5834388,0.733486 0.906254,0.659531 0.518127,-0.07306 0' +
          '.388484,0.174907 v 0.118327 l -0.517771,1.392661 -0.388929,0.586' +
          '468 0.129643,0.294125 -0.323707,1.098091 1.165987,2.126592 1.165' +
          '54,1.02672 0.518573,0.732684 -0.06522,1.540125 0.38893,0.878811 ' +
          '-0.38893,1.686341 c 0,0 -0.03047,-0.01043 0.01916,0.158334 0.050' +
          '07,0.168847 2.07527,1.293045 2.204022,1.19735 0.128307,-0.09748 ' +
          '0.237991,-0.182748 0.237991,-0.182748 l -0.129198,-0.365496 0.51' +
          '7771,-0.513404 0.194509,-0.513405 0.84228,-0.294125 0.647413,-1.' +
          '613188 -0.194063,-0.438559 0.452458,-0.659532 0.971923,-0.220972' +
          ' 0.518572,-1.172936 -0.129643,-1.465279 0.777057,-1.099873 0.129' +
          '643,-1.099873 c -1.063341,-0.527304 -2.11795,-1.07029 -3.17336,-' +
          '1.613188"'#10'         id="path6566-9"'#10'         style="display:inlin' +
          'e;fill:url(#radialGradient16);fill-opacity:1;stroke:url(#radialG' +
          'radient17)" />'#10'    </g>'#10'    <g'#10'       id="g6540-5"'#10'       style=' +
          '"display:inline;fill:url(#radialGradient9191-8);fill-opacity:1;f' +
          'ill-rule:nonzero;stroke:url(#radialGradient8816-6);stroke-miterl' +
          'imit:4"'#10'       transform="matrix(0.468894,0,0,0.468894,50.523,-1' +
          '4.44107)">'#10'      <path'#10'         d="m 26.8701,6.6933256 -1.8906,-' +
          '0.7407 -2.1797,0.2466 -2.6904,0.7402 -0.5088,0.4941 1.6719,1.151' +
          '4 v 0.6582 l -0.6543,0.6582 0.873,1.7289984 0.5801,-0.3301 0.728' +
          '5,-1.151399 c 1.123,-0.3471994 2.1299,-0.7406994 3.1973,-1.23439' +
          '94 l 0.873,-2.2212"'#10'         id="path6542-3"'#10'         style="fon' +
          't-variation-settings:normal;opacity:1;vector-effect:none;fill:ur' +
          'l(#radialGradient1-5);fill-opacity:1;stroke:url(#radialGradient1' +
          '8-3);stroke-width:1;stroke-linecap:butt;stroke-linejoin:miter;st' +
          'roke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stro' +
          'ke-opacity:1;-inkscape-stroke:none;stop-color:#000000;stop-opaci' +
          'ty:1" />'#10'    </g>'#10'    <g'#10'       id="g6580-2"'#10'       style="displ' +
          'ay:inline;fill:url(#radialGradient9197-9);fill-opacity:1;fill-ru' +
          'le:nonzero;stroke:url(#radialGradient8820-6);stroke-miterlimit:4' +
          '"'#10'       transform="matrix(0.468894,0,0,0.468894,50.83236,-14.75' +
          '043)">'#10'      <path'#10'         d="m 15.187259,9.6334723 -0.3638,0.9' +
          '047987 h 0.7271 l 0.3638,-0.8227987 c 0.3135,-0.2217 0.6255,-0.4' +
          '448 0.9448,-0.6582 l 0.7271,0.2471 c 0.4844,0.3291 0.9688,0.6582' +
          ' 1.4536,0.9867997 l 0.7275,-0.6576997 -0.8003,-0.3291 -0.3638,-0' +
          '.7407 -1.3809,-0.1646 -0.0728,-0.4116 -0.6543,0.165 -0.2904,0.57' +
          '58 -0.3638,-0.7407 -0.145,0.3291 0.0728,0.8228 -0.5816,0.494"'#10'  ' +
          '       id="path6582-5"'#10'         style="font-variation-settings:n' +
          'ormal;opacity:1;vector-effect:none;fill:url(#radialGradient4-1);' +
          'fill-opacity:1;stroke:url(#radialGradient19);stroke-width:1;stro' +
          'ke-linecap:butt;stroke-linejoin:miter;stroke-miterlimit:4;stroke' +
          '-dasharray:none;stroke-dashoffset:0;stroke-opacity:1;-inkscape-s' +
          'troke:none;stop-color:#000000;stop-opacity:1" />'#10'    </g>'#10'    <g' +
          #10'       id="g6626-2"'#10'       style="display:inline;fill:url(#radi' +
          'alGradient9203-8);fill-opacity:1;fill-rule:nonzero;stroke:url(#r' +
          'adialGradient8824-1);stroke-miterlimit:4"'#10'       transform="matr' +
          'ix(0.468894,0,0,0.468894,50.12526,-14.48526)">'#10'      <path'#10'     ' +
          '    d="m 42.893123,20.729176 c 0,0.241194 0,0 0,0 l -0.500291,0.' +
          '566672 c -0.306657,-0.361377 -0.650957,-0.66528 -1.000583,-0.982' +
          '679 L 40.624781,20.4261 39.923602,19.633475 v 0.980934 l 0.60073' +
          '5,0.454568 0.399848,0.452824 0.534354,-0.604316 c 0.134506,0.251' +
          '936 0.267177,0.503872 0.400765,0.755808 v 0.754982 l -0.601653,0' +
          '.679602 -1.101026,0.755808 -0.83385,0.832105 -0.534354,-0.606152' +
          ' 0.267177,-0.679603 -0.533527,-0.604315 -0.901057,-1.92597 -0.76' +
          '7468,-0.867912 -0.200888,0.225953 0.30124,1.095701 0.566672,0.64' +
          '1959 c 0.323642,0.934293 0.643796,1.82727 1.0688,2.720339 0.6590' +
          '36,0 1.280338,-0.06996 1.934875,-0.15241 v 0.529029 l -0.800704,' +
          '1.964072 -0.734324,0.830269 -0.600735,1.285754 c 0,0.70476 0,1.4' +
          '0952 0,2.114188 l 0.200888,0.832105 -0.333558,0.376618 -0.735242' +
          ',0.45365 -0.767468,0.641959 0.634798,0.717338 -0.867912,0.756727' +
          ' 0.166733,0.489549 -1.301914,1.474064 h -0.866994 l -0.734324,0.' +
          '45365 h -0.468064 v -0.604316 l -0.199052,-1.210468 c -0.258271,' +
          '-0.758562 -0.527192,-1.511708 -0.800705,-2.264853 0,-0.55593 0.0' +
          '3314,-1.106443 0.06638,-1.662282 L 32.716525,30.965452 32.24846,' +
          '30.05806 32.28252,28.811785 31.647722,28.094447 31.965121,27.056' +
          '129 31.44867,26.470176 H 30.5467 l -0.300322,-0.339801 -0.901057' +
          ',0.567131 -0.366702,-0.416466 -0.834768,0.717706 C 27.577179,26.' +
          '356327 27.009588,25.714368 26.44209,25.072409 l -0.667116,-1.586' +
          '995 0.600735,-0.905555 -0.333558,-0.377445 0.733405,-1.738579 c ' +
          '0.602571,-0.749564 1.231952,-1.468647 1.868586,-2.190392 l 1.135' +
          '089,-0.302158 1.267852,-0.150666 0.867912,0.226871 1.234615,1.24' +
          '5357 0.434001,-0.490467 0.599817,-0.07529 1.135089,0.377445 h 0.' +
          '867913 l 0.600735,-0.529029 0.267177,-0.377445 -0.601654,-0.3774' +
          '45 -1.0015,-0.07529 c -0.277919,-0.385524 -0.53619,-0.790789 -0.' +
          '866168,-1.133344 l -0.334476,0.150665 -0.133589,0.982679 -0.6007' +
          '35,-0.679603 -0.13267,-0.756726 -0.667116,-0.527192 h -0.268095 ' +
          'l 0.667942,0.754981 -0.267177,0.679603 -0.533527,0.150666 0.3335' +
          '58,-0.679603 -0.601654,-0.30124 -0.532609,-0.604315 -1.002419,0.' +
          '225952 -0.13267,0.30124 -0.600735,0.378363 -0.333558,0.831187 -0' +
          '.83385,0.415088 -0.36762,-0.415088 h -0.399848 v -1.360124 l 0.8' +
          '67912,-0.45365 H 29.3792 l -0.134506,-0.52811 -0.53261,-0.529029' +
          ' 0.900231,-0.189228 0.500291,-0.565754 0.399847,-0.680521 h 0.73' +
          '5242 l -0.200888,-0.52811 0.468065,-0.302158 v 0.604315 l 1.0005' +
          '82,0.225953 1.000583,-0.830268 0.06721,-0.378363 0.866994,-0.603' +
          '857 c -0.313818,0.03902 -0.627636,0.06767 -0.934293,0.151125 v -' +
          '0.680429 l 0.333558,-0.755441 h -0.333558 l -0.733038,0.679603 -' +
          '0.200888,0.377904 0.200888,0.529488 -0.334476,0.905555 -0.533528' +
          ',-0.302158 -0.466229,-0.52811 -0.735241,0.52811 -0.267177,-1.208' +
          '172 1.267851,-0.830728 V 9.8128376 l 0.801531,-0.5285696 1.26785' +
          '1,-0.302617 0.867912,0.302617 1.601318,0.3021579 -0.399848,0.452' +
          '8241 h -0.867912 l 0.867912,0.906474 0.667117,-0.754982 0.202632' +
          ',-0.3321814 c 0,0 2.558931,2.2934994 4.021335,4.8023004 1.462404' +
          ',2.509628 2.149168,5.46758 2.149168,6.068315 z"'#10'         id="pat' +
          'h6628-5"'#10'         style="font-variation-settings:normal;opacity:' +
          '1;vector-effect:none;fill:url(#radialGradient9187-9);fill-opacit' +
          'y:1;stroke:url(#radialGradient9189-7);stroke-width:1;stroke-line' +
          'cap:butt;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dashar' +
          'ray:none;stroke-dashoffset:0;stroke-opacity:1;-inkscape-stroke:n' +
          'one;stop-color:#000000;stop-opacity:1" />'#10'    </g>'#10'  </g>'#10'  <g'#10' ' +
          '    id="layer1-8"'#10'     inkscape:groupmode="layer"'#10'     inkscape:' +
          'label="Computer"'#10'     style="display:inline"'#10'     transform="mat' +
          'rix(0.63342687,0,0,0.63342687,1.9999999,16.119669)">'#10'    <g'#10'    ' +
          '   id="g4339"'#10'       transform="translate(-25)">'#10'      <path'#10'   ' +
          '      d="M 65.675044,41.213388 C 57.656634,30.125 72.913568,39.4' +
          '04152 71.161327,35 69.172021,30 52.551603,36.027728 53.480248,30' +
          '.116116"'#10'         id="path4172"'#10'         sodipodi:nodetypes="czz' +
          '"'#10'         style="display:inline;fill:none;fill-opacity:1;fill-r' +
          'ule:evenodd;stroke:#678fba;stroke-width:1;stroke-linecap:butt;st' +
          'roke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;st' +
          'roke-opacity:1" />'#10'    </g>'#10'    <g'#10'       id="g2302"'#10'       tran' +
          'sform="matrix(0.811017,0,0,0.811017,4.536063,4.144784)">'#10'      <' +
          'path'#10'         d="m 14.375479,32.558794 c 0,0 1.216876,4.898976 -' +
          '3.856329,4.944966 -2.4302757,0.02175 -1.9324777,4.006021 -1.9324' +
          '777,4.006021 l 30.8464667,-0.03115 c 0,0 0.418438,-3.867241 -2.0' +
          '22217,-3.912581 -4.987467,-0.09147 -3.810529,-5.06955 -3.810529,' +
          '-5.06955 z"'#10'         id="path1359"'#10'         sodipodi:nodetypes="' +
          'csccscc"'#10'         style="color:#000000;display:inline;overflow:v' +
          'isible;visibility:visible;opacity:1;fill:url(#linearGradient2308' +
          ');fill-opacity:1;fill-rule:evenodd;stroke:#7a7c78;stroke-width:1' +
          '.23302;stroke-linecap:butt;stroke-linejoin:miter;stroke-miterlim' +
          'it:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1;' +
          'marker-start:none;marker-end:none;marker-mid:none" />'#10'    </g>'#10' ' +
          '   <path'#10'       d="M 4.8882799,0.5019965 H 42.990539 c 2.023707,' +
          '0 3.498537,1.4255519 3.498537,3.6208005 l 0.01094,25.165237 C 46' +
          '.500019,30.977609 45.97204,31.5 44.466781,31.5 L 3.5326624,31.48' +
          '1093 C 2.3542134,31.452343 1.5154744,30.987161 1.4996519,29.4647' +
          '64 L 1.5148181,3.935329 c 0,-1.7712136 1.5383348,-3.4333325 3.37' +
          '34618,-3.4333325 z"'#10'       id="rect5040"'#10'       sodipodi:nodetyp' +
          'es="ccccccccc"'#10'       style="display:inline;fill:url(#linearGrad' +
          'ient5147);fill-opacity:1;fill-rule:evenodd;stroke:#7a7c78;stroke' +
          '-width:1;stroke-linecap:butt;stroke-linejoin:miter;stroke-miterl' +
          'imit:4;stroke-opacity:1" />'#10'    <rect'#10'       height="23"'#10'       ' +
          'id="rect9208"'#10'       style="display:inline;fill:url(#radialGradi' +
          'ent5239);fill-opacity:1;fill-rule:evenodd;stroke:#1d437a;stroke-' +
          'width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opaci' +
          'ty:1"'#10'       width="37"'#10'       x="5.5"'#10'       y="4.5" />'#10'    <pa' +
          'th'#10'       d="M 6,4.9921565 V 23 C 22.444445,21.645751 28.598693,' +
          '12.887581 41.968627,11.972548 l 1e-6,-7.011764 z"'#10'       id="pat' +
          'h4073"'#10'       sodipodi:nodetypes="ccccc"'#10'       style="display:i' +
          'nline;opacity:0.75;fill:url(#linearGradient6246);fill-opacity:1;' +
          'fill-rule:evenodd;stroke:none;stroke-width:1px;stroke-linecap:bu' +
          'tt;stroke-linejoin:miter;stroke-opacity:1" />'#10'    <g'#10'       id="' +
          'g3316"'#10'       transform="translate(48)">'#10'      <path'#10'         d=' +
          '"m -42.6875,40.49166 c -0.967886,0 -1.847763,0.285348 -2.4375,1.' +
          '247229 l -1.8125,2.918046 c -0.808653,1.054721 -0.156385,1.85907' +
          '7 2.59375,1.859077 h 40.4375 c 2.750135,0 3.402403,-0.804356 2.5' +
          '9375,-1.859077 L -3.125,41.738889 C -3.714737,40.777008 -4.59461' +
          '4,40.49166 -5.5625,40.49166 Z"'#10'         id="rect2024"'#10'         s' +
          'odipodi:nodetypes="ccccccccc"'#10'         style="fill:url(#linearGr' +
          'adient3442);fill-opacity:1;stroke:#7a7c78;stroke-width:1;stroke-' +
          'linejoin:round;stroke-miterlimit:4;stroke-opacity:1" />'#10'      <p' +
          'ath'#10'         d="m 5.3125,38.5 c -0.7068869,0 -1.0666848,0.07419 ' +
          '-1.5292969,1.076172 a 1.0001,1.0001 0 0 1 -0.00195,0.0039 l -1.8' +
          '125,3.875 a 1.0001,1.0001 0 0 1 -0.041016,0.07617 c -0.1455375,0' +
          '.252075 -0.1851561,0.437195 -0.1875,0.509766 -0.00234,0.07257 -0' +
          '.01383,0.03574 0.021484,0.07422 C 1.8323476,44.192192 2.4106476,' +
          '44.5 3.65625,44.5 h 40.4375 c 1.245602,0 1.823902,-0.307808 1.89' +
          '4531,-0.384766 0.03531,-0.03848 0.02383,-0.0016 0.02148,-0.07422' +
          ' -0.0023,-0.07257 -0.04196,-0.257691 -0.1875,-0.509766 a 1.0001,' +
          '1.0001 0 0 1 -0.04102,-0.07617 l -1.8125,-3.875 a 1.0001,1.0001 ' +
          '0 0 1 -0.002,-0.0039 C 43.504185,38.57419 43.144387,38.5 42.4375' +
          ',38.5 Z"'#10'         id="path2920"'#10'         inkscape:original="M 5.' +
          '3125 37.5 C 4.344614 37.5 3.4647369 37.878926 2.875 39.15625 L 1' +
          '.0625 43.03125 C 0.253847 44.43186 0.90611493 45.5 3.65625 45.5 ' +
          'L 44.09375 45.5 C 46.843885 45.5 47.496153 44.43186 46.6875 43.0' +
          '3125 L 44.875 39.15625 C 44.285263 37.878926 43.405386 37.5 42.4' +
          '375 37.5 L 5.3125 37.5 z "'#10'         inkscape:radius="-1"'#10'       ' +
          '  sodipodi:type="inkscape:offset"'#10'         style="display:inline' +
          ';fill:none;fill-opacity:0.46875;stroke:#e6e6e6;stroke-width:1.22' +
          '475;stroke-linejoin:round;stroke-miterlimit:4;stroke-opacity:1"'#10 +
          '         transform="matrix(1,0,0,0.666668,-48,15.83327)" />'#10'    ' +
          '</g>'#10'    <g'#10'       id="g3252"'#10'       transform="translate(48)"'#10' ' +
          '      style="display:inline">'#10'      <path'#10'         d="m -41.5652' +
          '34,39.500963 c -0.003,0.0031 0.0028,0.02359 0,0.026 -0.01467,0.0' +
          '041 -0.05022,0.02021 -0.06523,0.02601 -0.005,0.0021 -0.02762,-0.' +
          '0023 -0.03262,0 -0.005,0.0025 -0.02769,0.0233 -0.03262,0.026 -0.' +
          '0097,0.0058 -0.02321,0.01943 -0.03262,0.026 -0.0046,0.0035 -0.02' +
          '815,0.02233 -0.03262,0.02601 -0.0043,0.0039 -0.02844,0.02193 -0.' +
          '03262,0.02601 -0.06487,0.075 -0.133186,0.205396 -0.228319,0.3640' +
          '81 l -2.22755,3.952895 c 0,0.0049 -2.28e-4,0.02105 0,0.026 4.53e' +
          '-4,0.005 -6.74e-4,0.02104 0,0.026 8.91e-4,0.005 -0.0011,0.02106 ' +
          '0,0.02601 0.0026,0.0099 0.02917,0.04222 0.03262,0.05201 0.0019,0' +
          '.0049 -0.0021,0.02117 0,0.02601 0.0069,0.01439 0.02403,0.03804 0' +
          '.03262,0.05201 0.0091,0.0138 0.02197,0.03881 0.03262,0.05201 0.0' +
          '037,0.0043 0.02875,0.02176 0.03262,0.026 0.004,0.0042 0.02844,0.' +
          '02194 0.03262,0.02601 0.02595,0.02385 0.06703,0.05818 0.09785,0.' +
          '07802 0.0053,0.0032 0.02724,0.02296 0.03262,0.02601 0.0165,0.007' +
          '9 0.04736,0.01884 0.06523,0.026 0.120228,0.04402 0.279283,0.0780' +
          '2 0.42402,0.07802 h 23.114643 l -0.09785,-4.993121 -20.789242,-6' +
          'e-6 c -0.06851,0 -0.143762,-0.0019 -0.195701,0 -0.01215,5.7e-5 -' +
          '0.05203,5.11e-4 -0.06523,0 -0.004,2.5e-5 -0.02896,-2.14e-4 -0.03' +
          '262,0 -0.0028,0.0024 -0.02957,-0.0031 -0.03262,0 z m 23.039809,6' +
          'e-6 0.09785,1.664375 h 5.642726 l -0.260935,-1.664375 z m 7.4366' +
          '55,0 1.17421,4.993121 h 5.218705 c 0.144736,0 0.303792,-0.034 0.' +
          '42402,-0.07802 0.017877,-0.0072 0.048735,-0.0181 0.065233,-0.026' +
          ' 0.00538,-0.003 0.027357,-0.02283 0.032617,-0.02601 0.030821,-0.' +
          '01983 0.071897,-0.05416 0.097851,-0.07802 0.00418,-0.0041 0.0285' +
          '92,-0.02185 0.032616,-0.02601 0.00387,-0.0042 0.028906,-0.02168 ' +
          '0.032618,-0.026 0.010648,-0.0132 0.023496,-0.03821 0.032617,-0.0' +
          '5201 0.00859,-0.01397 0.025688,-0.03762 0.032617,-0.05201 0.0021' +
          '2,-0.0048 -0.00192,-0.02114 0,-0.02601 0.00345,-0.0098 0.029987,' +
          '-0.04214 0.032617,-0.05201 0.0011,-0.005 -8.92e-4,-0.02105 0,-0.' +
          '02601 6.73e-4,-0.005 -4.54e-4,-0.02104 0,-0.026 2.29e-4,-0.005 0' +
          ',-0.02106 0,-0.026 l -1.826015,-3.9529 C -5.834197,39.862396 -5.' +
          '902512,39.732002 -5.967383,39.656999 -5.971557,39.652925 -5.9956' +
          '7,39.634868 -6,39.630993 c -0.00447,-0.0037 -0.028021,-0.02252 -' +
          '0.032617,-0.02601 -0.00941,-0.0066 -0.022876,-0.02021 -0.032617,' +
          '-0.026 -0.00493,-0.0027 -0.027643,-0.0235 -0.032616,-0.026 -0.00' +
          '5,-0.0023 -0.027608,0.0021 -0.032617,0 -0.015014,-0.0058 -0.0505' +
          '64,-0.02192 -0.065234,-0.02601 -0.016117,-0.0031 -0.041567,-0.02' +
          '056 -0.065235,-0.026 -0.011875,-0.0025 -0.054353,0.0014 -0.06523' +
          '3,0 -0.05194,-0.0019 -0.127187,0 -0.195701,0 z m -6.131978,3.328' +
          '747 -1.043741,1.664374 h 6.001511 l -1.304677,-1.664374 z"'#10'     ' +
          '    id="path2308"'#10'         sodipodi:nodetypes="csssssssccsssssss' +
          'sssssccccssscccccccccssssssssssssccsssssssssccccccc"'#10'         st' +
          'yle="color:#000000;display:inline;overflow:visible;visibility:vi' +
          'sible;fill:url(#linearGradient3320);fill-opacity:1;fill-rule:non' +
          'zero;stroke:#858980;stroke-width:0.999999;stroke-linecap:square;' +
          'stroke-linejoin:round;stroke-miterlimit:4;stroke-dashoffset:0;st' +
          'roke-opacity:1;marker-start:none;marker-end:none;marker-mid:none' +
          '" />'#10'    </g>'#10'  </g>'#10'</svg>'#10
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
        IconName = 'folder-new'
        SVGText = 
          '<?xml version="1.0" encoding="UTF-8" standalone="no"?>'#10'<!-- Crea' +
          'ted with Inkscape (http://www.inkscape.org/) -->'#10'<svg id="svg97"' +
          ' height="48" version="1.1" width="48" xmlns="http://www.w3.org/2' +
          '000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" inkscape:exp' +
          'ort-filename="/home/jimmac/Desktop/horlander-style3.png" inkscap' +
          'e:export-xdpi="90.000000" inkscape:export-ydpi="90.000000" inksc' +
          'ape:output_extension="org.inkscape.output.svg.inkscape" inkscape' +
          ':version="1.4 (86a8ad7, 2024-10-11)" sodipodi:docname="folder-ne' +
          'w.svg" sodipodi:version="0.32" xmlns:inkscape="http://www.inksca' +
          'pe.org/namespaces/inkscape" xmlns:sodipodi="http://sodipodi.sour' +
          'ceforge.net/DTD/sodipodi-0.dtd" xmlns:svg="http://www.w3.org/200' +
          '0/svg">'#10' <defs id="defs3">'#10'  <linearGradient id="linearGradient5' +
          '048">'#10'   <stop id="stop5050" offset="0" stop-color="#0a0a0a" sto' +
          'p-opacity="0"/>'#10'   <stop id="stop5056" offset=".5" stop-color="#' +
          '0a0a0a" stop-opacity="1"/>'#10'   <stop id="stop5052" offset="1" sto' +
          'p-color="#0a0a0a" stop-opacity="0"/>'#10'  </linearGradient>'#10'  <line' +
          'arGradient id="linearGradient9766">'#10'   <stop id="stop9768" offse' +
          't="0" stop-color="#f8ce3c" stop-opacity="1"/>'#10'   <stop id="stop9' +
          '770" offset="1" stop-color="#f9dc53" stop-opacity="1"/>'#10'  </line' +
          'arGradient>'#10'  <linearGradient id="linearGradient3096">'#10'   <stop ' +
          'id="stop3098" offset="0" stop-color="#4e3030" stop-opacity="1"/>' +
          #10'   <stop id="stop3100" offset="1" stop-color="#965d5d" stop-opa' +
          'city="1"/>'#10'  </linearGradient>'#10'  <linearGradient id="linearGradi' +
          'ent319" inkscape:collect="always">'#10'   <stop id="stop320" offset=' +
          '"0" stop-color="#ffffff" stop-opacity="1"/>'#10'   <stop id="stop321' +
          '" offset="1" stop-color="#ffffff" stop-opacity="0"/>'#10'  </linearG' +
          'radient>'#10'  <linearGradient id="linearGradient1789">'#10'   <stop id=' +
          '"stop1790" offset="0" stop-color="#1f1313" stop-opacity="1"/>'#10'  ' +
          ' <stop id="stop1791" offset="1" stop-color="#d6b0b0" stop-opacit' +
          'y="1"/>'#10'  </linearGradient>'#10'  <radialGradient id="radialGradient' +
          '238" cx="20.706017" cy="37.517986" fx="20.706017" fy="37.517986"' +
          ' gradientTransform="matrix(-1.055022 -.02734504 -.177703 1.19092' +
          '9 50.912274 -1.715777)" gradientUnits="userSpaceOnUse" r="30.905' +
          '205" xlink:href="#linearGradient1789" inkscape:collect="always"/' +
          '>'#10'  <linearGradient id="linearGradient322" gradientTransform="ma' +
          'trix(1.317489 0 0 .816256 -.879573 -1.318166)" gradientUnits="us' +
          'erSpaceOnUse" x1="13.035696" x2="12.853771" xlink:href="#linearG' +
          'radient319" y1="32.567184" y2="46.689312" inkscape:collect="alwa' +
          'ys"/>'#10'  <linearGradient id="linearGradient3104" gradientTransfor' +
          'm="matrix(-1 0 0 1 47.340097 5.409524)" gradientUnits="userSpace' +
          'OnUse" x1="18.112709" x2="15.514889" xlink:href="#linearGradient' +
          '3096" y1="31.36775" y2="6.18025" inkscape:collect="always"/>'#10'  <' +
          'linearGradient id="linearGradient9772" gradientUnits="userSpaceO' +
          'nUse" x1="22.175976" x2="22.065331" xlink:href="#linearGradient9' +
          '766" y1="36.987999" y2="32.050499" inkscape:collect="always"/>'#10' ' +
          ' <linearGradient id="XMLID_2_" gradientTransform="matrix(.314683' +
          ' 0 0 .314683 4.128264 3.742874)" gradientUnits="userSpaceOnUse" ' +
          'x1="80.223602" x2="48.046001" y1="117.5205" y2="59.7995">'#10'   <st' +
          'op id="stop17" offset="0" stop-color="#cccccc"/>'#10'   <stop id="st' +
          'op19" offset=".9831" stop-color="#ffffff"/>'#10'  </linearGradient>'#10 +
          '  <linearGradient id="linearGradient2329">'#10'   <stop id="stop2331' +
          '" offset="0" stop-color="#000000" stop-opacity=".185567"/>'#10'   <s' +
          'top id="stop2333" offset="1" stop-color="#ffffff" stop-opacity="' +
          '1"/>'#10'  </linearGradient>'#10'  <linearGradient id="linearGradient240' +
          '6">'#10'   <stop id="stop2408" offset="0" stop-color="#7c7e79" stop-' +
          'opacity="1"/>'#10'   <stop id="stop2414" offset=".1724138" stop-colo' +
          'r="#848681" stop-opacity="1"/>'#10'   <stop id="stop2410" offset="1"' +
          ' stop-color="#898c86" stop-opacity="1"/>'#10'  </linearGradient>'#10'  <' +
          'linearGradient id="linearGradient2307">'#10'   <stop id="stop2309" o' +
          'ffset="0" stop-color="#edd400" stop-opacity="1"/>'#10'   <stop id="s' +
          'top2311" offset="1" stop-color="#998800" stop-opacity="1"/>'#10'  </' +
          'linearGradient>'#10'  <linearGradient id="XMLID_39_" gradientTransfo' +
          'rm="matrix(.354101 0 0 .354101 1.638679 -.083649)" gradientUnits' +
          '="userSpaceOnUse" x1="64.387703" x2="64.387703" y1="65.124001" y' +
          '2="35.569">'#10'   <stop id="stop336" offset="0" stop-color="#ffffff' +
          '"/>'#10'   <stop id="stop338" offset=".8539" stop-color="#ff6200"/>'#10 +
          '   <stop id="stop340" offset="1" stop-color="#f25d00"/>'#10'  </line' +
          'arGradient>'#10'  <linearGradient id="XMLID_2_-8" gradientUnits="use' +
          'rSpaceOnUse" x1="28" x2="28" y1="57.5" y2="0">'#10'   <stop id="stop' +
          '12" offset="0" stop-color="#ffca00" stop-opacity="1"/>'#10'   <stop ' +
          'id="stop14" offset="1" stop-color="#ff8200" stop-opacity="1"/>'#10' ' +
          ' </linearGradient>'#10'  <linearGradient id="linearGradient4307-8-8"' +
          ' gradientUnits="userSpaceOnUse" x1="17.488245" x2="-6.23667" xli' +
          'nk:href="#XMLID_2_-8-8" y1="24.410179" y2="-17.332663" inkscape:' +
          'collect="always"/>'#10'  <linearGradient id="XMLID_2_-8-8" gradientU' +
          'nits="userSpaceOnUse" x1="28" x2="28" y1="57.5" y2="0">'#10'   <stop' +
          ' id="stop14-4-2" offset="0" stop-color="#ff8200" stop-opacity="1' +
          '"/>'#10'   <stop id="stop12-2-4" offset="1" stop-color="#ffca00" sto' +
          'p-opacity="1"/>'#10'  </linearGradient>'#10'  <radialGradient id="radial' +
          'Gradient3" cx="24" cy="24" fx="24" fy="24" gradientUnits="userSp' +
          'aceOnUse" r="14.5" xlink:href="#linearGradient3" inkscape:collec' +
          't="always"/>'#10'  <linearGradient id="linearGradient3" gradientUnit' +
          's="userSpaceOnUse" x1="28" x2="28" y1="57.5" y2="0">'#10'   <stop id' +
          '="stop2" offset="0" stop-color="#fff889" stop-opacity="1"/>'#10'   <' +
          'stop id="stop3" offset="1" stop-color="#ffca00" stop-opacity="1"' +
          '/>'#10'  </linearGradient>'#10' </defs>'#10' <path id="path216" d="m 42.8183' +
          '17 44.09694 c -.0218 .416304 -.459905 .832609 -.87621 .832609 H ' +
          '10.615086 c -.416302 0 -.810812 -.416305 -.789016 -.832609 l .93' +
          '6443 -27.226735 c .0218 -.416303 .459897 -.832616 .876201 -.8326' +
          '16 h 13.270873 c .485057 0 1.234473 -.315589 1.401644 -1.106632 ' +
          'l .611391 -2.89307 C 27.078091 11.302213 27.804836 11 28.22114 1' +
          '1 H 43 c .416313 0 .810821 .416305 .789025 .832609 z" style="dis' +
          'play:inline;fill-opacity:1;fill-rule:nonzero;stroke-width:1;stro' +
          'ke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4;strok' +
          'e-dasharray:none;stroke-opacity:1;fill:url(#radialGradient238);s' +
          'troke:url(#linearGradient3104)" sodipodi:nodetypes="ccccccsssscc' +
          'c" inkscape:label="Folder-back"/>'#10' <g id="layer1" display="inlin' +
          'e" transform="matrix(-1 0 0 1 47.340097 5.409524)" inkscape:grou' +
          'pmode="layer" inkscape:label="Folder">'#10'  <g id="g220" style="dis' +
          'play:inline;fill:#ffffff;fill-opacity:.757062;fill-rule:nonzero;' +
          'stroke:none;stroke-width:.999465;stroke-miterlimit:4" transform=' +
          '"matrix(1.1109385 0 .05816673 1.040764 -11.678838 2.594461)" ink' +
          'scape:export-filename="/home/jimmac/ximian_art/icons/nautilus/su' +
          'se93/gnome-fs-directory.png" inkscape:export-xdpi="74.800003" in' +
          'kscape:export-ydpi="74.800003"/>'#10'  <path id="path233" d="m 39.78' +
          '3532 39.51062 c 1.143894 -.04406 1.963076 -1.096299 2.047035 -2.' +
          '321005 c .791787 -11.548687 1.65936 -21.231949 1.65936 -21.23194' +
          '9 c .07215 -.247484 -.167911 -.494967 -.48014 -.494967 H 8.63863' +
          '04 c 0 0 -1.8503191 21.866892 -1.8503191 21.866892 c -.1145551 .' +
          '982066 -.4660075 1.804718 -1.5498358 2.183713 z" style="color:#0' +
          '00000;display:inline;visibility:visible;fill-opacity:1;fill-rule' +
          ':nonzero;stroke:#bea41b;stroke-width:1;stroke-linecap:butt;strok' +
          'e-linejoin:round;stroke-miterlimit:4;stroke-dasharray:none;strok' +
          'e-dashoffset:0;stroke-opacity:1;marker-start:none;marker-mid:non' +
          'e;marker-end:none;fill:url(#linearGradient9772)" inkscape:export' +
          '-filename="/home/jimmac/ximian_art/icons/nautilus/suse93/gnome-f' +
          's-directory.png" inkscape:export-xdpi="74.800003" inkscape:expor' +
          't-ydpi="74.800003" sodipodi:nodetypes="cscccscc"/>'#10'  <path id="p' +
          'ath304" d="m 9.6202444 16.463921 l 32.7910986 .06481 l -1.574046' +
          ' 20.001979 c -.08432 1.071511 -.450678 1.428215 -1.872656 1.4282' +
          '15 c -1.871502 0 -28.677968 -.03241 -31.394742 -.03241 c .233598' +
          '3 -.320811 .3337557 -.988623 .3350963 -1.004612 z" style="displa' +
          'y:inline;opacity:.465909;fill:none;fill-opacity:1;fill-rule:even' +
          'odd;stroke-width:1;stroke-linecap:round;stroke-linejoin:miter;st' +
          'roke-opacity:1;stroke:url(#linearGradient322)" sodipodi:nodetype' +
          's="ccsscsc"/>'#10'  <path id="path323" d="M 9.6202481 16.223182 L 8.' +
          '4536014 31.866453 c 0 0 8.2961546 -4.148078 18.6663476 -4.148078' +
          ' c 10.370193 0 15.55529 -11.495193 15.55529 -11.495193 z" style=' +
          '"display:inline;fill:#ffffff;fill-opacity:.089286;fill-rule:even' +
          'odd;stroke:none;stroke-width:1;stroke-linecap:butt;stroke-linejo' +
          'in:miter;stroke-opacity:1" sodipodi:nodetypes="ccccc"/>'#10' </g>'#10' <' +
          'g id="layer1-5" transform="matrix(.59090909 0 0 .59090909 .81818' +
          '2 .818181)" inkscape:label="Sun" inkscape:groupmode="layer">'#10'  <' +
          'path id="path1-1" d="m 22.822246 20.36205 c -.320226 .359569 -8.' +
          '497099 -2.114621 -8.895568 -1.844331 c -.384288 .260672 -.742616' +
          ' 8.535238 -1.183456 8.681136 c -.457108 .151283 -6.3013936 -6.07' +
          '9865 -6.7816229 -6.045021 c -.4631383 .0336 -4.910743 7.020425 -' +
          '5.36547096 6.926356 c -.47150855 -.09754 -2.41723404 -8.416014 -' +
          '2.85054694 -8.625953 c -.4178917 -.202467 -7.7630402 3.624494 -8' +
          '.1098122 3.315665 c -.359569 -.320226 2.1146212 -8.4971 1.844330' +
          '5 -8.895568 c -.2606713 -.384288 -8.5352375 -.742616 -8.6811355 ' +
          '-1.183456 c -.151283 -.457109 6.079865 -6.3013939 6.045021 -6.78' +
          '16232 c -.0336 -.4631384 -7.020425 -4.91074302 -6.926356 -5.3654' +
          '7098 c .09754 -.47150856 8.4160144 -2.41723402 8.625953 -2.85054' +
          '692 c .2024671 -.4178917 -3.624494 -7.7630409 -3.315665 -8.10981' +
          '19 c .320226 -.359569 8.4970997 2.1146209 8.8955683 1.8443302 c ' +
          '.3842875 -.2606713 .7426159 -8.5352372 1.1834561 -8.6811362 c .4' +
          '571083 -.151282 6.3013934 6.079866 6.7816227 6.045022 c .4631384' +
          ' -.0336 4.9107431 -7.020425 5.365471 -6.926357 c .4715086 .09754' +
          ' 2.4172339 8.4160151 2.8505469 8.6259537 c .417892 .2024671 7.76' +
          '3041 -3.6244947 8.109812 -3.3156647 c .359569 .320226 -2.114621 ' +
          '8.4970993 -1.84433 8.895568 c .260671 .3842875 8.535237 .7426159' +
          ' 8.681136 1.1834561 c .151282 .4571083 -6.079866 6.3013934 -6.04' +
          '5022 6.7816227 c .0336 .4631384 7.020425 4.910743 6.926357 5.365' +
          '471 c -.09754 .4715086 -8.416015 2.4172342 -8.625954 2.8505472 c' +
          ' -.202467 .417891 3.624495 7.76304 3.315665 8.109812 z" style="c' +
          'olor:#000000;overflow:visible;opacity:1;fill-opacity:1;stroke:#d' +
          '58b18;stroke-width:.657155;stroke-linecap:butt;stroke-linejoin:m' +
          'iter;stroke-miterlimit:4;stroke-dasharray:none;stroke-dashoffset' +
          ':0;stroke-opacity:1;stop-color:#000000;stop-opacity:1;fill:url(#' +
          'linearGradient4307-8-8)" transform="matrix(.93863685 0 0 .938636' +
          '85 19.283273 19.332406)" sodipodi:type="star" inkscape:flatsided' +
          '="false" sodipodi:sides="12" sodipodi:cx="5.0250816" sodipodi:cy' +
          '="4.9727373" sodipodi:r1="23.528069" sodipodi:r2="16.208176" sod' +
          'ipodi:arg1="0.71296987" sodipodi:arg2="0.98938479" inkscape:roun' +
          'ded="0.053" inkscape:randomized="0" font-variation-settings="nor' +
          'mal" vector-effect="none" paint-order="fill markers stroke"/>'#10'  ' +
          '<circle id="path2" cx="24" cy="24" r="14.5" style="color:#000000' +
          ';overflow:visible;opacity:1;fill-opacity:1;stroke:none;stroke-wi' +
          'dth:.702853;stroke-linecap:butt;stroke-linejoin:miter;stroke-mit' +
          'erlimit:4;stroke-dasharray:none;stroke-dashoffset:0;stroke-opaci' +
          'ty:1;fill:url(#radialGradient3)" font-variation-settings="normal' +
          '" vector-effect="none" paint-order="fill markers stroke"/>'#10' </g>' +
          #10'</svg>'#10
      end
      item
        IconName = 'folder-up'
        SVGText = 
          '<svg height="48" width="48" xmlns="http://www.w3.org/2000/svg" x' +
          'mlns:xlink="http://www.w3.org/1999/xlink"><radialGradient id="a"' +
          ' cx="20.706017" cy="37.517986" gradientTransform="matrix(-1.0550' +
          '22 -.02734504 -.177703 1.190929 50.912274 -1.715777)" gradientUn' +
          'its="userSpaceOnUse" r="30.905205"><stop offset="0" stop-color="' +
          '#1f1313"/><stop offset="1" stop-color="#d6b0b0"/></radialGradien' +
          't><linearGradient id="b" gradientTransform="matrix(1.317489 0 0 ' +
          '.816256 -.879573 -1.318166)" gradientUnits="userSpaceOnUse" x1="' +
          '13.035696" x2="12.853771" y1="32.567184" y2="46.689312"><stop of' +
          'fset="0" stop-color="#fff"/><stop offset="1" stop-color="#fff" s' +
          'top-opacity="0"/></linearGradient><linearGradient id="c" gradien' +
          'tTransform="matrix(-1 0 0 1 47.340097 5.409524)" gradientUnits="' +
          'userSpaceOnUse" x1="18.112709" x2="15.514889" y1="31.36775" y2="' +
          '6.18025"><stop offset="0" stop-color="#4e3030"/><stop offset="1"' +
          ' stop-color="#965d5d"/></linearGradient><linearGradient id="d" g' +
          'radientUnits="userSpaceOnUse" x1="22.175976" x2="22.065331" y1="' +
          '36.987999" y2="32.050499"><stop offset="0" stop-color="#f8ce3c"/' +
          '><stop offset="1" stop-color="#f9dc53"/></linearGradient><linear' +
          'Gradient id="e" gradientUnits="userSpaceOnUse" x1="6.028616" x2=' +
          '"41.01947" y1="21.99962" y2="21.99962"><stop offset="0" stop-col' +
          'or="#55c000"/><stop offset="1" stop-color="#2f7600"/></linearGra' +
          'dient><path d="m42.818317 44.09694c-.0218.416304-.459905.832609-' +
          '.87621.832609h-31.327021c-.416302 0-.810812-.416305-.789016-.832' +
          '609l.936443-27.226735c.0218-.416303.459897-.832616.876201-.83261' +
          '6h13.270873c.485057 0 1.234473-.315589 1.401644-1.106632l.611391' +
          '-2.89307c.155469-.735674.882214-1.037887 1.298518-1.037887h14.77' +
          '886c.416313 0 .810821.416305.789025.832609z" fill="url(#a)" stro' +
          'ke="url(#c)" stroke-linecap="round" stroke-linejoin="round"/><g ' +
          'transform="matrix(-1 0 0 1 47.340097 5.409524)"><path d="m39.783' +
          '532 39.51062c1.143894-.04406 1.963076-1.096299 2.047035-2.321005' +
          '.791787-11.548687 1.65936-21.231949 1.65936-21.231949.07215-.247' +
          '484-.167911-.494967-.48014-.494967h-34.3711566s-1.8503191 21.866' +
          '892-1.8503191 21.866892c-.1145551.982066-.4660075 1.804718-1.549' +
          '8358 2.183713z" fill="url(#d)" stroke="#bea41b" stroke-linejoin=' +
          '"round"/><path d="m9.6202444 16.463921 32.7910986.06481-1.574046' +
          ' 20.001979c-.08432 1.071511-.450678 1.428215-1.872656 1.428215-1' +
          '.871502 0-28.677968-.03241-31.394742-.03241.2335983-.320811.3337' +
          '557-.988623.3350963-1.004612z" fill="none" opacity=".465909" str' +
          'oke="url(#b)" stroke-linecap="round"/><path d="m9.6202481 16.223' +
          '182-1.1666467 15.643271s8.2961546-4.148078 18.6663476-4.148078 1' +
          '5.55529-11.495193 15.55529-11.495193z" fill="#fff" fill-opacity=' +
          '".089286" fill-rule="evenodd"/></g><g stroke-miterlimit="10" tra' +
          'nsform="matrix(-.7105263 0 0 .7105263 31.297641 -1.368111)"><pat' +
          'h d="m14.519136 38.5 18.005029-.0039v-12.991632l7.995366-.0078-1' +
          '7.144722-19.9974545-16.8462505 19.9980705 7.9958815.0038z" fill=' +
          '"url(#e)" fill-rule="evenodd" stroke="#1b4600" stroke-linecap="r' +
          'ound" stroke-linejoin="round"/><path d="m15.520704 37.496094 16.' +
          '001405.003906v-12.99295l6.816811-.015625-14.954276-17.4525854-14' +
          '.7065267 17.4569424 6.8399007.0052z" fill="none" opacity=".48128' +
          '3" stroke="#bffffe"/></g></svg>'
      end
      item
        IconName = 'folder-home'
        SVGText = 
          '<svg height="48" width="48" xmlns="http://www.w3.org/2000/svg" x' +
          'mlns:xlink="http://www.w3.org/1999/xlink"><linearGradient id="a"' +
          '><stop offset="0" stop-color="#fff"/><stop offset="1" stop-color' +
          '="#fff" stop-opacity="0"/></linearGradient><radialGradient id="b' +
          '" cx="20.706017" cy="37.517986" gradientTransform="matrix(-1.055' +
          '022 -.02734504 -.177703 1.190929 50.912274 -1.715777)" gradientU' +
          'nits="userSpaceOnUse" r="30.905205"><stop offset="0" stop-color=' +
          '"#1f1313"/><stop offset="1" stop-color="#d6b0b0"/></radialGradie' +
          'nt><linearGradient id="c" gradientTransform="matrix(1.317489 0 0' +
          ' .816256 -.879573 -1.318166)" gradientUnits="userSpaceOnUse" x1=' +
          '"13.035696" x2="12.853771" xlink:href="#a" y1="32.567184" y2="46' +
          '.689312"/><linearGradient id="d" gradientTransform="matrix(-1 0 ' +
          '0 1 47.340097 5.409524)" gradientUnits="userSpaceOnUse" x1="18.1' +
          '12709" x2="15.514889" y1="31.36775" y2="6.18025"><stop offset="0' +
          '" stop-color="#4e3030"/><stop offset="1" stop-color="#965d5d"/><' +
          '/linearGradient><linearGradient id="e" gradientUnits="userSpaceO' +
          'nUse" x1="22.175976" x2="22.065331" y1="36.987999" y2="32.050499' +
          '"><stop offset="0" stop-color="#f8ce3c"/><stop offset="1" stop-c' +
          'olor="#f9dc53"/></linearGradient><linearGradient id="f" gradient' +
          'Transform="matrix(.336922 0 0 .166888 17.98288 15.46151)" gradie' +
          'ntUnits="userSpaceOnUse" x1="52.006104" x2="14.049017" y1="166.1' +
          '331" y2="-42.218513"><stop offset="0" stop-color="#ccc"/><stop o' +
          'ffset=".9831" stop-color="#fff"/></linearGradient><linearGradien' +
          't id="g" gradientTransform="matrix(.898789 0 0 1.071914 .478025 ' +
          '-2.080838)" gradientUnits="userSpaceOnUse" x1="17.602522" x2="17' +
          '.682528" y1="26.057423" y2="32.654099"><stop offset="0" stop-opa' +
          'city=".185567"/><stop offset="1" stop-color="#fff"/></linearGrad' +
          'ient><linearGradient id="h" gradientTransform="matrix(.888785 0 ' +
          '0 1.08932 2.41099 -1.524336)" gradientUnits="userSpaceOnUse" x1=' +
          '"17.850183" x2="19.040216" y1="28.939463" y2="41.03223"><stop of' +
          'fset="0" stop-color="#7c7e79"/><stop offset=".1724138" stop-colo' +
          'r="#848681"/><stop offset="1" stop-color="#898c86"/></linearGrad' +
          'ient><radialGradient id="i" cx="20.443665" cy="37.425831" gradie' +
          'ntTransform="matrix(1.125263 0 0 .982744 -3.428678 .731106)" gra' +
          'dientUnits="userSpaceOnUse" r="1.082104" xlink:href="#j"/><linea' +
          'rGradient id="j"><stop offset="0"/><stop offset="1" stop-opacity' +
          '="0"/></linearGradient><radialGradient id="k" cx="19.985598" cy=' +
          '"36.77816" gradientTransform="matrix(1.125263 0 0 .982744 -3.428' +
          '678 .565787)" gradientUnits="userSpaceOnUse" r="1.082104"><stop ' +
          'offset="0" stop-color="#edd400"/><stop offset="1" stop-color="#9' +
          '80"/></radialGradient><linearGradient id="l" gradientUnits="user' +
          'SpaceOnUse" x1="24.43842902" x2="24.43842902" y1="22.9768248781"' +
          ' y2="12.511369469"><stop offset="0" stop-color="#fff"/><stop off' +
          'set=".8539" stop-color="#ff6200"/><stop offset="1" stop-color="#' +
          'f25d00"/></linearGradient><radialGradient id="m" cx="7.532664" c' +
          'y="24.202574" gradientTransform="matrix(4.100086 0 0 4.201322 -2' +
          '5.41506 -78.53967)" gradientUnits="userSpaceOnUse" r="8.245213" ' +
          'xlink:href="#a"/><radialGradient id="n" cx="11.68129" cy="19.554' +
          '111" gradientTransform="matrix(4.100086 0 0 -4.201322 -5.198109 ' +
          '105.3535)" gradientUnits="userSpaceOnUse" r="8.245213" xlink:hre' +
          'f="#j"/><radialGradient id="o" cx="29.913452" cy="30.442923" gra' +
          'dientTransform="matrix(3.751495 0 0 3.147818 -82.00907 -65.70704' +
          ')" gradientUnits="userSpaceOnUse" r="4.001883" xlink:href="#a"/>' +
          '<path d="m42.818317 44.09694c-.0218.416304-.459905.832609-.87621' +
          '.832609h-31.327021c-.416302 0-.810812-.416305-.789016-.832609l.9' +
          '36443-27.226735c.0218-.416303.459897-.832616.876201-.832616h13.2' +
          '70873c.485057 0 1.234473-.315589 1.401644-1.106632l.611391-2.893' +
          '07c.155469-.735674.882214-1.037887 1.298518-1.037887h14.77886c.4' +
          '16313 0 .810821.416305.789025.832609z" fill="url(#b)" stroke="ur' +
          'l(#d)" stroke-linecap="round" stroke-linejoin="round"/><g transf' +
          'orm="matrix(-1 0 0 1 47.340097 5.409524)"><path d="m39.783532 39' +
          '.51062c1.143894-.04406 1.963076-1.096299 2.047035-2.321005.79178' +
          '7-11.548687 1.65936-21.231949 1.65936-21.231949.07215-.247484-.1' +
          '67911-.494967-.48014-.494967h-34.3711566s-1.8503191 21.866892-1.' +
          '8503191 21.866892c-.1145551.982066-.4660075 1.804718-1.5498358 2' +
          '.183713z" fill="url(#e)" stroke="#bea41b" stroke-linejoin="round' +
          '"/><path d="m9.6202444 16.463921 32.7910986.06481-1.574046 20.00' +
          '1979c-.08432 1.071511-.450678 1.428215-1.872656 1.428215-1.87150' +
          '2 0-28.677968-.03241-31.394742-.03241.2335983-.320811.3337557-.9' +
          '88623.3350963-1.004612z" fill="none" opacity=".465909" stroke="u' +
          'rl(#c)" stroke-linecap="round"/><path d="m9.6202481 16.223182-1.' +
          '1666467 15.643271s8.2961546-4.148078 18.6663476-4.148078 15.5552' +
          '9-11.495193 15.55529-11.495193z" fill="#fff" fill-opacity=".0892' +
          '86" fill-rule="evenodd"/></g><g transform="matrix(.55112434 0 0 ' +
          '.55112434 1.432588 .360712)"><path d="m21.619576 8.1833733h5.957' +
          '459c.839732 0 13.886475 15.4353277 13.886475 16.3406587l-.443521' +
          ' 18.496745c0 .905333-.67603 1.634177-1.515762 1.634177h-31.45727' +
          '74c-.8397329 0-1.5157625-.728844-1.5157625-1.634177l.056478-18.4' +
          '96745c0-.905331 14.1921789-16.3406587 15.0319109-16.3406587z" fi' +
          'll="url(#f)" stroke="#757575"/><path d="m46.963575 45.735573h-45' +
          '.3248988v-45.32489746h45.3248988z" fill="none"/><path clip-rule=' +
          '"evenodd" d="m23 29-.04574 15.090942h-11.842795l-.111465-15.0909' +
          '42z" fill="url(#g)" fill-rule="evenodd"/><path d="m21.780459 9.4' +
          '05584h5.559097c.783582 0 13.000869 14.399588 13.000869 15.244172' +
          'l-.347158 18.212311c0 .459259-.143737.653465-.512375.653465l-31.' +
          '3872026.01428c-.3686377 0-.5839636-.07992-.5839636-.45355l.21534' +
          '18-18.426506c0-.844584 13.2718124-15.244172 14.0553914-15.244172' +
          'z" fill="none" opacity=".3125" stroke="#fff"/><g clip-rule="even' +
          'odd" fill-rule="evenodd"><path d="m22 30v14.090942h-9.811029l-.1' +
          '88971-14.090942z" fill="url(#h)"/><path d="m19.576856 36.44767c.' +
          '67279 0 1.216616.474605 1.216616 1.058507 0 .589811-.543826 1.06' +
          '8355-1.216616 1.068355-.672272 0-1.218686-.478544-1.218686-1.068' +
          '355.000515-.583902.546414-1.058507 1.218686-1.058507z" fill="url' +
          '(#i)" opacity=".409091"/><path d="m19.462314 35.932229c.672789 0' +
          ' 1.216615.474605 1.216615 1.058507 0 .589809-.543826 1.068353-1.' +
          '216615 1.068353-.672273 0-1.218687-.478544-1.218687-1.068353.000' +
          '515-.583902.546414-1.058507 1.218687-1.058507z" fill="url(#k)"/>' +
          '</g><path d="m24.447748 11.559337 18.92706 17.169868.494679.3919' +
          '91.403676-.171385-.37287-.761673-.277614-.223436-19.174931-15.57' +
          '2306-19.3895153 15.743335-.2376018.14412-.2167099.706786.4334198' +
          '.129248.384554-.308423z" fill="url(#l)"/><path d="m24.330168 2.2' +
          '713382-21.8817386 18.1013368-.6247289 7.165928 1.9999362 2.06432' +
          '3s20.4073813-17.157285 20.6240933-17.327963l19.63254 17.54326 1.' +
          '898424-2.323997-1.615791-7.111374-19.915173-18.2159732z" fill="#' +
          'ef2929" stroke="#a40000"/><path d="m2.8413446 20.613129-.2915552' +
          ' 6.623365 21.8194296-18.256419-.070328-5.8933307z" fill="url(#m)' +
          '" opacity=".409091"/><path d="m24.483763 8.7509884.09946-5.84110' +
          '17 19.328963 17.6519533 1.491812 6.500812z" fill="url(#n)" opaci' +
          'ty=".136364"/><path d="m27.102228 27.719824h9.039995c.770595 0 1' +
          '.390967.62037 1.390967 1.390967l-.008 9.079221c0 .770596-.596322' +
          ' 1.265969-1.366918 1.265969h-9.056083c-.770597 0-1.390967-.62037' +
          '3-1.390967-1.390969v-8.954221c0-.770597.62037-1.390967 1.390967-' +
          '1.390967z" fill="none" opacity=".318182" stroke="#fff" stroke-wi' +
          'dth=".999999"/><rect fill="#3465a4" height="9.962456" rx=".38128' +
          '2" stroke="#757575" stroke-width=".999999" width="10.001333" x="' +
          '26.507767" y="28.514256"/><path d="m27.107118 34.408261c3.617983' +
          '.331177 5.527724-1.445704 8.868152-1.55274l.02473-3.849491-8.911' +
          '612-.00603z" fill="url(#o)" opacity=".397727"/></g></svg>'
      end>
    Left = 435
    Top = 220
  end
  object SVGImgLst24: TSVGIconVirtualImageList
    Size = 24
    AntiAliasColor = clBackground
    ImageCollection = SVGImgColl
    Left = 436
    Top = 282
  end
  object SVGImgLst48: TSVGIconVirtualImageList
    Size = 48
    ImageCollection = SVGImgColl
    Left = 436
    Top = 337
  end
end
