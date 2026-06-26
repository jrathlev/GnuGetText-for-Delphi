object frmTransMain: TfrmTransMain
  Left = 381
  Top = 208
  Caption = 'Process GnuGetText translations for Delphi'
  ClientHeight = 639
  ClientWidth = 826
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnAfterMonitorDpiChanged = FormAfterMonitorDpiChanged
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object paTop: TPanel
    Left = 0
    Top = 0
    Width = 826
    Height = 346
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      826
      346)
    object paFiles: TPanel
      Left = 5
      Top = 0
      Width = 356
      Height = 246
      BevelOuter = bvNone
      TabOrder = 0
      object Label1: TLabel
        Left = 10
        Top = 10
        Width = 84
        Height = 13
        Caption = 'Project directory:'
      end
      object btProjDir: TJrSpeedButton
        Left = 320
        Top = 18
        Width = 31
        Height = 31
        Hint = 'Select project directory'
        GroupIndex = 31
        Images = imlGlyphs
        ImageIndex = 0
        Layout = blGlyphLeft
        ParentShowHint = False
        ShowHint = True
        OnClick = btProjDirClick
      end
      object rbMask: TRadioButton
        Left = 10
        Top = 100
        Width = 171
        Height = 17
        Caption = 'All files matching to mask'
        Checked = True
        TabOrder = 2
        TabStop = True
        OnClick = rbMaskClick
      end
      object rbFiles: TRadioButton
        Left = 190
        Top = 100
        Width = 161
        Height = 17
        Caption = 'Only the following files'
        TabOrder = 3
        OnClick = rbFilesClick
      end
      object pcMode: TPageControl
        Left = 5
        Top = 120
        Width = 351
        Height = 126
        ActivePage = tsMask
        Style = tsButtons
        TabOrder = 4
        object tsMask: TTabSheet
          Caption = 'tsMask'
          TabVisible = False
          DesignSize = (
            343
            116)
          object btDefMask: TJrSpeedButton
            Left = 310
            Top = 13
            Width = 31
            Height = 27
            Images = imlGlyphs
            ImageIndex = 26
            Layout = blGlyphLeft
            OnClick = btDefMaskClick
          end
          object bbExclude: TJrSpeedButton
            Left = 310
            Top = 81
            Width = 31
            Height = 31
            Anchors = [akTop, akRight]
            Images = imlGlyphs
            ImageIndex = 0
            Layout = blGlyphLeft
            OnClick = bbExcludeClick
          end
          object cbRecurse: TCheckBox
            Left = 2
            Top = 50
            Width = 331
            Height = 18
            Caption = 'Also scan subdirectories'
            TabOrder = 1
            OnClick = cbRecurseClick
          end
          object EditMask: TLabeledEdit
            Left = 0
            Top = 15
            Width = 306
            Height = 21
            Hint = 'Here you can specify which files you want to scan.'
            EditLabel.Width = 44
            EditLabel.Height = 13
            EditLabel.Caption = 'Filemask:'
            ParentShowHint = False
            ShowHint = True
            TabOrder = 0
            Text = '*.pas *.dfm *.inc *.rc *.xfm *.dpr'
          end
          object ExcludeDirs: TLabeledEdit
            Left = 0
            Top = 90
            Width = 306
            Height = 21
            EditLabel.Width = 207
            EditLabel.Height = 13
            EditLabel.Caption = 'Exclude subdirectories (comma separated):'
            TabOrder = 2
          end
        end
        object tsFiles: TTabSheet
          Caption = 'tsFiles'
          ImageIndex = 1
          TabVisible = False
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object Label2: TLabel
            Left = 0
            Top = 10
            Width = 96
            Height = 13
            Caption = 'Files to be scanned:'
          end
          object cbFiles: TComboBox
            Left = 0
            Top = 25
            Width = 336
            Height = 21
            Hint = 'Insert filenames separated by commas'
            ParentShowHint = False
            ShowHint = True
            TabOrder = 0
          end
          object btnNew: TJrButton
            Left = 270
            Top = 50
            Width = 31
            Height = 31
            Hint = 'Create new file list'
            Images = imlGlyphs
            ImageIndex = 19
            Layout = blGlyphLeft
            ParentShowHint = False
            ShowHint = True
            TabOrder = 1
            OnClick = btnNewClick
          end
          object btnEdit: TJrButton
            Left = 305
            Top = 50
            Width = 31
            Height = 31
            Hint = 'Edit file list'
            Images = imlGlyphs
            ImageIndex = 20
            Layout = blGlyphLeft
            ParentShowHint = False
            ShowHint = True
            TabOrder = 2
            OnClick = btnEditClick
          end
        end
      end
      object cbProjDir: TComboBox
        Left = 10
        Top = 25
        Width = 306
        Height = 21
        DropDownCount = 20
        PopupMenu = pmDirectoryList
        TabOrder = 0
        OnCloseUp = cbProjDirCloseUp
      end
      object ProjectName: TLabeledEdit
        Left = 10
        Top = 70
        Width = 341
        Height = 21
        EditLabel.Width = 126
        EditLabel.Height = 13
        EditLabel.Caption = 'Project name and version:'
        TabOrder = 1
      end
    end
    object gbDomain: TGroupBox
      Left = 15
      Top = 255
      Width = 341
      Height = 81
      Caption = 'Text domain'
      TabOrder = 1
      object rbDefault: TRadioButton
        Left = 15
        Top = 20
        Width = 81
        Height = 17
        Caption = 'default'
        Checked = True
        TabOrder = 0
        TabStop = True
        OnClick = rbDefaultClick
      end
      object rbOther: TRadioButton
        Left = 15
        Top = 45
        Width = 81
        Height = 17
        Caption = 'other'
        TabOrder = 1
        OnClick = rbDefaultClick
      end
      object OutputName: TLabeledEdit
        Left = 110
        Top = 40
        Width = 201
        Height = 21
        EditLabel.Width = 31
        EditLabel.Height = 13
        EditLabel.Caption = 'Name:'
        TabOrder = 2
      end
    end
    object gbCopy: TGroupBox
      Left = 365
      Top = 5
      Width = 454
      Height = 106
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Copy mo file to "locale\..\LC_MESSAGES\"'
      TabOrder = 2
      DesignSize = (
        454
        106)
      object btCopyDir: TJrSpeedButton
        Left = 417
        Top = 63
        Width = 31
        Height = 31
        Anchors = [akTop, akRight]
        Images = imlGlyphs
        ImageIndex = 0
        Layout = blGlyphLeft
        OnClick = btCopyDirClick
      end
      object edTargetDir: TLabeledEdit
        Left = 15
        Top = 70
        Width = 399
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 150
        EditLabel.Height = 13
        EditLabel.Caption = 'Folders of project executables:'
        TabOrder = 0
      end
      object Panel1: TPanel
        Left = 10
        Top = 15
        Width = 316
        Height = 31
        BevelOuter = bvNone
        TabOrder = 1
        object rbMulti: TRadioButton
          Left = 175
          Top = 10
          Width = 176
          Height = 17
          Caption = 'In multiple directories'
          TabOrder = 0
          OnClick = rbMultiClick
        end
        object rbSingle: TRadioButton
          Left = 10
          Top = 10
          Width = 146
          Height = 17
          Caption = 'In single directory'
          Checked = True
          TabOrder = 1
          TabStop = True
          OnClick = rbSingleClick
        end
      end
    end
    object pcOptions: TPageControl
      Left = 365
      Top = 120
      Width = 454
      Height = 216
      ActivePage = tsLang
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 3
      OnChange = pcOptionsChange
      object tsLang: TTabSheet
        Caption = 'Languages'
        object Label4: TLabel
          Left = 265
          Top = 97
          Width = 99
          Height = 13
          Caption = 'Available languages:'
        end
        object cbLanguage: TComboBox
          Left = 265
          Top = 114
          Width = 176
          Height = 22
          Style = csOwnerDrawVariable
          DropDownCount = 12
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Courier New'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 2
          OnDrawItem = cbComboBoxDrawItem
        end
        object edLangSubDir: TLabeledEdit
          Left = 265
          Top = 25
          Width = 176
          Height = 21
          EditLabel.Width = 125
          EditLabel.Height = 13
          EditLabel.Caption = 'Translations subdirectory:'
          TabOrder = 1
        end
        object lbLang: TListBox
          Left = 5
          Top = 5
          Width = 181
          Height = 176
          Style = lbOwnerDrawVariable
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Courier New'
          Font.Style = []
          ItemHeight = 14
          MultiSelect = True
          ParentFont = False
          TabOrder = 0
          OnClick = lbLangClick
          OnDblClick = btPoEditClick
          OnDrawItem = lbLangDrawItem
          OnMeasureItem = lbLangMeasureItem
        end
        object btSelNone: TJrButton
          Left = 225
          Top = 115
          Width = 31
          Height = 31
          Hint = 'Deselect all'
          Images = imlGlyphs
          ImageIndex = 6
          Layout = blGlyphLeft
          ParentShowHint = False
          ShowHint = True
          TabOrder = 9
          OnClick = btSelNoneClick
        end
        object btSelAll: TJrButton
          Left = 190
          Top = 115
          Width = 31
          Height = 31
          Hint = 'Select all'
          Images = imlGlyphs
          ImageIndex = 5
          Layout = blGlyphLeft
          ParentShowHint = False
          ShowHint = True
          TabOrder = 8
          OnClick = btSelAllClick
        end
        object bbAdd: TJrButton
          Left = 265
          Top = 145
          Width = 176
          Height = 36
          Hint = 'Add selected language to list'
          Caption = 'Add to list'
          Images = imlGlyphs
          ImageIndex = 9
          Layout = blGlyphLeft
          ParentShowHint = False
          ShowHint = True
          TabOrder = 3
          OnClick = bbAddClick
        end
        object bbRem: TJrButton
          Left = 190
          Top = 80
          Width = 31
          Height = 31
          Hint = 'Remove selected language from list'
          Images = imlGlyphs
          ImageIndex = 3
          Layout = blGlyphLeft
          ParentShowHint = False
          ShowHint = True
          TabOrder = 6
          OnClick = bbRemClick
        end
        object bbUp: TJrButton
          Left = 190
          Top = 5
          Width = 31
          Height = 31
          Hint = 'Move item up'
          Images = imlGlyphs
          ImageIndex = 1
          Layout = blGlyphLeft
          ParentShowHint = False
          ShowHint = True
          TabOrder = 4
          OnClick = bbUpClick
        end
        object bbDown: TJrButton
          Left = 190
          Top = 40
          Width = 31
          Height = 31
          Hint = 'Move item down'
          Images = imlGlyphs
          ImageIndex = 2
          Layout = blGlyphLeft
          ParentShowHint = False
          ShowHint = True
          TabOrder = 5
          OnClick = bbDownClick
        end
        object btStatus: TJrButton
          Left = 225
          Top = 150
          Width = 31
          Height = 31
          Hint = 'Show statitstics of selected translation'
          Images = imlGlyphs
          ImageIndex = 8
          Layout = blGlyphLeft
          ParentShowHint = False
          ShowHint = True
          TabOrder = 11
          OnClick = btStatusClick
        end
        object btUpdate: TJrButton
          Left = 190
          Top = 150
          Width = 31
          Height = 31
          Hint = 'Check for incomplete translations'
          Images = imlGlyphs
          ImageIndex = 7
          Layout = blGlyphLeft
          ParentShowHint = False
          ShowHint = True
          TabOrder = 10
          OnClick = btUpdateClick
        end
        object bbImport: TJrButton
          Left = 225
          Top = 80
          Width = 31
          Height = 31
          Hint = 'Import language list from another project'
          Images = imlGlyphs
          ImageIndex = 4
          Layout = blGlyphLeft
          ParentShowHint = False
          ShowHint = True
          TabOrder = 7
          OnClick = bbImportClick
        end
      end
      object tsOptions: TTabSheet
        Caption = 'Processing'
        ImageIndex = 1
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object cbCreateIgnore: TCheckBox
          Left = 20
          Top = 70
          Width = 356
          Height = 17
          Hint = 
            'Create a new ignore.po file or add likely ignores to existing ig' +
            'nore.po file (default domain)'
          Caption = 'Add likely ignores to ignore.po file'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
        end
        object cbRemoveIgnore: TCheckBox
          Left = 20
          Top = 95
          Width = 356
          Height = 17
          Hint = 'Don'#39't store items from text domain that are present in ignore.po'
          Caption = 'Remove items from text domain present in ignore.po file'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 2
        end
        object cbPoEdit: TCheckBox
          Left = 20
          Top = 120
          Width = 356
          Height = 17
          Hint = 'Open the merged file with the assigned po editor'
          Caption = 'Edit translation immediately after merging'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 3
          OnClick = cbPoEditClick
        end
        object rgEncoding: TRadioGroup
          Left = 15
          Top = 12
          Width = 416
          Height = 51
          Caption = 'Encoding of source files:'
          Columns = 3
          ItemIndex = 0
          Items.Strings = (
            'US-ASCII'
            'ISO-8859-1'
            'UTF-8')
          TabOrder = 0
        end
      end
      object tsMerge: TTabSheet
        Caption = 'Merging'
        ImageIndex = 3
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object cbMergeSimilar: TCheckBox
          Left = 15
          Top = 12
          Width = 371
          Height = 26
          Caption = 'Accept similar strings and mark as Fuzzy'
          TabOrder = 0
          WordWrap = True
        end
        object gbLangMerge: TGroupBox
          Left = 10
          Top = 45
          Width = 426
          Height = 96
          Caption = 'Language-dependent settings'
          TabOrder = 1
          object laMergeLanguage: TLabel
            Left = 35
            Top = 22
            Width = 21
            Height = 13
            Caption = 'xxx'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object cbMergeAutoComments: TCheckBox
            Left = 35
            Top = 45
            Width = 356
            Height = 17
            Caption = 'Retain automatic comments'
            ParentShowHint = False
            ShowHint = False
            TabOrder = 0
            OnClick = MergeOptionClick
          end
          object cbMergeHistory: TCheckBox
            Left = 35
            Top = 70
            Width = 356
            Height = 17
            Caption = 'Retain obsolete translations'
            ParentShowHint = False
            ShowHint = False
            TabOrder = 1
            OnClick = MergeOptionClick
          end
        end
        object edSimLength: TEdit
          Left = 395
          Top = 16
          Width = 25
          Height = 21
          Hint = 'Similarity measure'
          ParentShowHint = False
          ReadOnly = True
          ShowHint = True
          TabOrder = 2
          Text = '5'
        end
        object udSimLength: TUpDown
          Left = 420
          Top = 16
          Width = 16
          Height = 21
          Hint = 'Similarity measure'
          Associate = edSimLength
          Min = 4
          Max = 12
          ParentShowHint = False
          Position = 5
          ShowHint = True
          TabOrder = 3
        end
      end
      object tsSaveOptions: TTabSheet
        Caption = 'Saving'
        ImageIndex = 2
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object cbOverwrite: TCheckBox
          Left = 20
          Top = 40
          Width = 376
          Height = 17
          Caption = 'Overwrite template file'
          TabOrder = 1
        end
        object cbOrder: TCheckBox
          Left = 20
          Top = 15
          Width = 376
          Height = 17
          Caption = 'Sort entries by original strings (msgid)'
          TabOrder = 0
        end
        object gbSaveLanguage: TGroupBox
          Left = 10
          Top = 65
          Width = 426
          Height = 76
          Caption = 'Language-dependent settings'
          TabOrder = 2
          object laSaveLanguage: TLabel
            Left = 35
            Top = 22
            Width = 21
            Height = 13
            Caption = 'xxx'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object sbSetAllBu: TJrSpeedButton
            Left = 390
            Top = 37
            Width = 31
            Height = 31
            Hint = 'Tick for all languages'
            Flat = True
            Images = imlGlyphs
            ImageIndex = 5
            Layout = blGlyphLeft
            ParentShowHint = False
            ShowHint = True
            OnClick = sbSetAllBuClick
          end
          object cbBackup: TCheckBox
            Left = 35
            Top = 45
            Width = 351
            Height = 17
            Caption = 'Create backup file of old translation'
            TabOrder = 0
            OnClick = MergeOptionClick
          end
        end
      end
      object pcExclude: TTabSheet
        Caption = 'Exclude conditions'
        ImageIndex = 4
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        DesignSize = (
          446
          188)
        object lbGroupValues: TLabel
          Left = 225
          Top = 15
          Width = 35
          Height = 13
          Caption = 'Values:'
        end
        object Label3: TLabel
          Left = 10
          Top = 15
          Width = 84
          Height = 13
          Caption = 'Exclusion groups:'
        end
        object lbExcludeValues: TListBox
          Left = 225
          Top = 30
          Width = 181
          Height = 136
          TabStop = False
          Anchors = [akLeft, akTop, akRight]
          ItemHeight = 13
          TabOrder = 0
        end
        object btnEditExclude: TJrButton
          Left = 410
          Top = 135
          Width = 31
          Height = 31
          Hint = 'Edit exclude values'
          Anchors = [akRight, akBottom]
          Images = imlGlyphs
          ImageIndex = 20
          Layout = blGlyphLeft
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          OnClick = btnEditExcludeClick
        end
        object btnClearExclude: TJrButton
          Left = 410
          Top = 100
          Width = 31
          Height = 31
          Hint = 'Delete all values in this group'
          Anchors = [akRight, akBottom]
          Images = imlGlyphs
          ImageIndex = 28
          Layout = blGlyphLeft
          ParentShowHint = False
          ShowHint = True
          TabOrder = 2
          OnClick = btnClearExcludeClick
        end
        object btnSvgExclude: TJrButton
          Left = 410
          Top = 65
          Width = 31
          Height = 31
          Hint = 'Add values for SVGIconImageList'
          Anchors = [akRight, akBottom]
          Images = imlGlyphs
          ImageIndex = 19
          Layout = blGlyphLeft
          ParentShowHint = False
          ShowHint = True
          TabOrder = 3
          OnClick = btnSvgExcludeClick
        end
        object lbExcludeGroups: TListBox
          Left = 10
          Top = 30
          Width = 206
          Height = 136
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ItemHeight = 13
          Items.Strings = (
            'Classes within forms'
            'Class properties within forms'
            'Objects within selected forms'
            'Files'
            'Directories')
          ParentFont = False
          TabOrder = 4
          OnClick = lbExcludeGroupsClick
        end
      end
    end
  end
  object pnStatus: TPanel
    Left = 0
    Top = 346
    Width = 826
    Height = 252
    Align = alClient
    TabOrder = 1
    object meProgress: TMemo
      Left = 1
      Top = 1
      Width = 519
      Height = 250
      TabStop = False
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier New'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 0
      WordWrap = False
    end
    object paButtons: TPanel
      Left = 520
      Top = 1
      Width = 305
      Height = 250
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 1
      object btAssemble: TJrButton
        Left = 5
        Top = 205
        Width = 291
        Height = 36
        Caption = 'Embed translations in executables'
        Images = imlGlyphs
        ImageIndex = 18
        Layout = blGlyphLeft
        TabOrder = 4
        OnClick = btAssembleClick
      end
      object btCopy: TJrButton
        Left = 5
        Top = 160
        Width = 291
        Height = 36
        Caption = 'Copy mo files to binary folder of project'
        Images = imlGlyphs
        ImageIndex = 17
        Layout = blGlyphLeft
        TabOrder = 3
        OnClick = btCopyClick
      end
      object btExtract: TJrButton
        Left = 5
        Top = 5
        Width = 291
        Height = 36
        Caption = 'Create template from project sources'
        Images = imlGlyphs
        ImageIndex = 10
        Layout = blGlyphLeft
        TabOrder = 0
        WordWrap = True
        OnClick = btExtractClick
      end
      object btMerge: TJrButton
        Left = 5
        Top = 50
        Width = 291
        Height = 36
        Caption = 'Merge translation with template'
        Images = imlGlyphs
        ImageIndex = 12
        Layout = blGlyphLeft
        TabOrder = 1
        OnClick = btMergeClick
      end
      object gbEdit: TGroupBox
        Left = 5
        Top = 90
        Width = 291
        Height = 66
        Caption = 'Edit translation'
        TabOrder = 2
        object btEdit: TJrButton
          Left = 119
          Top = 20
          Width = 82
          Height = 36
          Hint = 'Edit translation in text editor'
          Caption = 'As text'
          Images = imlGlyphs
          ImageIndex = 15
          Layout = blGlyphLeft
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          OnClick = btEditClick
        end
        object btEditIgnore: TJrButton
          Left = 204
          Top = 20
          Width = 82
          Height = 36
          Hint = 'Edit ignore.po'
          Caption = 'Ignores'
          Images = imlGlyphs
          ImageIndex = 16
          Layout = blGlyphLeft
          ParentShowHint = False
          ShowHint = True
          TabOrder = 2
          OnClick = btEditIgnoreClick
        end
        object btPoEdit: TJrButton
          Left = 5
          Top = 20
          Width = 111
          Height = 36
          Hint = 'Edit translation (po file)'
          Caption = 'Po editor'
          Images = imlGlyphs
          ImageIndex = 14
          Layout = blGlyphLeft
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          OnClick = btPoEditClick
        end
      end
    end
  end
  object paBottom: TPanel
    Left = 0
    Top = 598
    Width = 826
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      826
      41)
    object laLineNr: TLabel
      Left = 517
      Top = 12
      Width = 3
      Height = 13
      Alignment = taRightJustify
      Anchors = [akRight, akBottom]
    end
    object laProgress: TLabel
      Left = 10
      Top = 12
      Width = 455
      Height = 17
      Anchors = [akLeft, akRight, akBottom]
      AutoSize = False
    end
    object btnClipBoard: TJrButton
      Left = 529
      Top = 5
      Width = 31
      Height = 31
      Hint = 'Copy log to clipboard'
      Anchors = [akRight, akBottom]
      Images = imlGlyphs
      ImageIndex = 21
      Layout = blGlyphLeft
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnClick = btnClipBoardClick
    end
    object btnHelp: TJrButton
      Left = 719
      Top = 5
      Width = 31
      Height = 31
      Hint = 'Show program help'
      Anchors = [akRight, akBottom]
      Images = imlGlyphs
      ImageIndex = 22
      Layout = blGlyphLeft
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      OnClick = btnHelpClick
    end
    object btnInfo: TJrButton
      Left = 754
      Top = 5
      Width = 31
      Height = 31
      Anchors = [akRight, akBottom]
      Images = imlGlyphs
      ImageIndex = 24
      Layout = blGlyphLeft
      TabOrder = 4
      OnClick = btnInfoClick
    end
    object btnManual: TJrButton
      Left = 684
      Top = 5
      Width = 31
      Height = 31
      Hint = 'Show DxGetText manual'
      Anchors = [akRight, akBottom]
      Images = imlGlyphs
      ImageIndex = 23
      Layout = blGlyphLeft
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnClick = btnManualClick
    end
    object EndeBtn: TJrButton
      Left = 789
      Top = 5
      Width = 31
      Height = 31
      Hint = 'Quit program'
      Anchors = [akRight, akBottom]
      Images = imlGlyphs
      ImageIndex = 25
      Layout = blGlyphLeft
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
      OnClick = EndeBtnClick
    end
    object btnDesign: TJrButton
      Left = 649
      Top = 5
      Width = 31
      Height = 31
      Hint = 'Settings for language and design'
      Anchors = [akRight, akBottom]
      Images = imlGlyphs
      ImageIndex = 27
      Layout = blGlyphLeft
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = btnDesignClick
    end
  end
  object OpenDialog: TOpenDialog
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 620
    Top = 190
  end
  object pmDirectoryList: TPopupMenu
    Left = 760
    Top = 25
    object pmiEdit: TMenuItem
      Caption = 'Edit list'
      OnClick = pmiEditClick
    end
    object pmiClear: TMenuItem
      Caption = 'Clear list'
      OnClick = pmiClearClick
    end
    object N21: TMenuItem
      Caption = '-'
    end
    object pmiCancel: TMenuItem
      Caption = 'Cancel'
    end
  end
  object FileOpenDialog: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <>
    Options = [fdoPickFolders, fdoForceFileSystem]
    Left = 655
    Top = 190
  end
  object imlGlyphs: TSVGIconImageList
    Size = 24
    SVGIconItems = <
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
        IconName = 'list-open'
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
          ')" stroke-linecap="round" stroke-linejoin="round"/><g><rect fill' +
          '="#fff" height="28.84615503543" rx="1.73076913209" stroke="#7a7c' +
          '77" stroke-miterlimit="10.433" stroke-width="1.15384552126" widt' +
          'h="26.53846229251" x="13.65384436244" y="2.23076959666"/><g fill' +
          '="#91938e"><path d="m20.69230697327 5.23077033249h16.61538502834' +
          'v2.76923027126h-16.61538502834z"/><path d="m16.53846071619 5.230' +
          '77033249h2.76923027126v2.76923027126h-2.76923027126z"/><path d="' +
          'm20.69230697327 10.42307772875h16.61538502834v2.76923027126h-16.' +
          '61538502834z"/><path d="m16.53846071619 10.42307772875h2.7692302' +
          '7126v2.76923027126h-2.76923027126z"/><path d="m20.69230697327 15' +
          '.26923197875h16.61538502834v2.76923027125h-16.61538502834z"/><pa' +
          'th d="m16.53846071619 15.26923197875h2.76923027126v2.76923027125' +
          'h-2.76923027126z"/><path d="m20.69230697327 20.11538452834h16.61' +
          '538502834v2.76923027126h-16.61538502834z"/><path d="m16.53846071' +
          '619 20.11538452834h2.76923027126v2.76923027126h-2.76923027126z"/' +
          '><path d="m20.69230697327 24.96153877834h16.61538502834v2.769230' +
          '27126h-16.61538502834z"/><path d="m16.53846071619 24.96153877834' +
          'h2.76923027126v2.76923027126h-2.76923027126z"/></g></g><g transf' +
          'orm="matrix(-1 0 0 1 47.525575 5.909523)"><path d="m39.783532 39' +
          '.51062c1.143894-.04406 1.963076-1.096299 2.047035-2.321005.79178' +
          '7-11.548687 1.65936-21.231949 1.65936-21.231949.07215-.247484-.1' +
          '67911-.494967-.48014-.494967h-34.3711566s-1.8503191 21.866892-1.' +
          '8503191 21.866892c-.1145551.982066-.4660075 1.804718-1.5498358 2' +
          '.183713z" fill="url(#d)" stroke="#bea41b" stroke-linejoin="round' +
          '"/><path d="m9.6202444 16.463921 32.7910986.06481-1.574046 20.00' +
          '1979c-.08432 1.071511-.450678 1.428215-1.872656 1.428215-1.87150' +
          '2 0-28.677968-.03241-31.394742-.03241.2335983-.320811.3337557-.9' +
          '88623.3350963-1.004612z" fill="none" opacity=".465909" stroke="u' +
          'rl(#b)" stroke-linecap="round"/><path d="m9.6202481 16.223182-1.' +
          '1666467 15.643271s8.2961546-4.148078 18.6663476-4.148078 15.5552' +
          '9-11.495193 15.55529-11.495193z" fill="#fff" fill-opacity=".0892' +
          '86" fill-rule="evenodd"/></g></svg>'
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
        IconName = 'list-find'
        SVGText = 
          '<svg height="48" width="48" xmlns="http://www.w3.org/2000/svg" x' +
          'mlns:xlink="http://www.w3.org/1999/xlink"><linearGradient id="a"' +
          ' gradientUnits="userSpaceOnUse" x1="27.366341" x2="31.335964" y1' +
          '="26.580296" y2="30.557772"><stop offset="0" stop-color="#8a8a8a' +
          '"/><stop offset="1" stop-color="#484848"/></linearGradient><line' +
          'arGradient id="b" gradientTransform="matrix(1.334593 0 0 1.29129' +
          '2 -6.973842 -7.460658)" gradientUnits="userSpaceOnUse" x1="30.65' +
          '625" x2="33.21875" y1="34" y2="31.0625"><stop offset="0" stop-co' +
          'lor="#d03030"/><stop offset=".5" stop-color="#c93737"/><stop off' +
          'set="1" stop-color="#e38a8a"/></linearGradient><linearGradient i' +
          'd="c" gradientUnits="userSpaceOnUse" x1="18.292673" x2="17.50089' +
          '3" y1="13.602121" y2="25.743469"><stop offset="0" stop-color="#f' +
          'ff"/><stop offset=".5" stop-color="#fff" stop-opacity=".219048"/' +
          '><stop offset="1" stop-color="#fff"/></linearGradient><radialGra' +
          'dient id="d" cx="18.240929" cy="21.817987" gradientUnits="userSp' +
          'aceOnUse" r="8.308505"><stop offset="0" stop-color="#729fcf" sto' +
          'p-opacity=".207843"/><stop offset="1" stop-color="#729fcf" stop-' +
          'opacity=".676191"/></radialGradient><radialGradient id="e" cx="1' +
          '5.414371" cy="13.078408" gradientTransform="matrix(2.592963 0 0 ' +
          '2.252104 -25.05975 -18.941)" gradientUnits="userSpaceOnUse" r="6' +
          '.65625"><stop offset="0" stop-color="#fff"/><stop offset="1" sto' +
          'p-color="#fff" stop-opacity=".247619"/></radialGradient><g><rect' +
          ' fill="#fff" height="41.66666744204" rx="2.49999980091" stroke="' +
          '#7a7c77" stroke-miterlimit="10.433" stroke-width="1.66666571523"' +
          ' width="38.33333355545" x="4.83333242904" y="3.16666710887"/><g ' +
          'fill="#91938e"><path d="m15.00000041473 7.50000140705h24.0000000' +
          '5363v3.99999919023h-24.00000005363z"/><path d="m9.00000040132 7.' +
          '50000140705h3.99999919023v3.99999919023h-3.99999919023z"/><path ' +
          'd="m15.00000041473 15.00000080977h24.00000005363v3.99999919023h-' +
          '24.00000005363z"/><path d="m9.00000040132 15.00000080977h3.99999' +
          '919023v3.99999919023h-3.99999919023z"/><path d="m15.00000041473 ' +
          '22.00000123477h24.00000005363v3.99999919023h-24.00000005363z"/><' +
          'path d="m9.00000040132 22.00000123477h3.99999919023v3.9999991902' +
          '3h-3.99999919023z"/><path d="m15.00000041473 28.99999920363h24.0' +
          '0000005363v3.99999919023h-24.00000005363z"/><path d="m9.00000040' +
          '132 28.99999920363h3.99999919023v3.99999919023h-3.99999919023z"/' +
          '><path d="m15.00000041473 35.99999962863h24.00000005363v3.999999' +
          '19023h-24.00000005363z"/><path d="m9.00000040132 35.99999962863h' +
          '3.99999919023v3.99999919023h-3.99999919023z"/></g></g><g transfo' +
          'rm="matrix(.96934746 0 0 .9693285 1.96861 1.582762)"><g fill-rul' +
          'e="evenodd"><path d="m18.627569 3.1435548c-8.13913 0-14.7448008 ' +
          '6.6056711-14.7448008 14.7448012 0 8.13913 6.6056708 14.744802 14' +
          '.7448008 14.744802 3.479555 0 6.551001-1.384393 9.073723-3.40264' +
          '7-.205377 1.006881-.07803 2.035368.756144 2.759925l10.964084 9.5' +
          '2741c1.233416 1.071329 3.087462.93096 4.15879-.302457 1.071328-1' +
          '.233418.930959-3.087462-.302457-4.15879l-10.964084-9.527411c-.67' +
          '1527-.583279-1.492878-.755969-2.306238-.642722 1.9867-2.512422 3' +
          '.364839-5.548803 3.364839-8.99811 0-8.1391301-6.605671-14.744801' +
          '2-14.744801-14.7448012zm-.07562 1.2261833c7.639459 0 13.291775 4' +
          '.7889505 13.291775 13.2917749 0 8.675113-5.81669 13.291775-13.29' +
          '1775 13.291775-7.302949 0-13.2917734-5.478092-13.2917734-13.2917' +
          '75 0-7.9841069 5.8246384-13.291775 13.2917734-13.2917749z" fill=' +
          '"#dcdcdc" stroke="url(#a)" stroke-linecap="round" stroke-miterli' +
          'mit="10" stroke-width="3.00582" transform="matrix(.665377 0 0 .6' +
          '65377 15.98645 17.90835)"/><path d="m18.602905 3.0803551c-8.1654' +
          '4 0-14.7924642 6.627024-14.7924642 14.7924639 0 8.16544 6.627024' +
          '2 14.792464 14.7924642 14.792464 3.490803 0 6.572177-1.388867 9.' +
          '103055-3.413645-.206041 1.010136-.07829 2.041947.758587 2.768846' +
          'l10.999526 9.558207c1.237403 1.074792 3.097442.93397 4.172233-.3' +
          '03435 1.074791-1.237404.933968-3.097442-.303435-4.172233l-10.999' +
          '525-9.558208c-.673698-.585164-1.497704-.758413-2.313693-.644799 ' +
          '1.993122-2.520544 3.375716-5.56674 3.375716-9.027197 0-8.1654399' +
          '-6.627024-14.7924639-14.792464-14.7924639zm-.07586 3.1860692c6.2' +
          '81108.0000002 11.378818 5.0977107 11.378818 11.3788187s-5.09771 ' +
          '11.378818-11.378818 11.378818-11.3788184-5.09771-11.3788184-11.3' +
          '78818c.0000002-6.281108 5.0977104-11.3788187 11.3788184-11.37881' +
          '87z" fill="#dcdcdc" transform="matrix(.665377 0 0 .665377 15.986' +
          '45 17.90835)"/><path d="m39.507004 41.57769c-.478672-2.273187 1.' +
          '39733-4.811422 3.584053-4.788375 0 0-10.760367-9.258111-10.76036' +
          '7-9.258111-2.944791-.05671-4.269502 2.272616-3.776814 4.599922z"' +
          ' fill="url(#b)" transform="matrix(.665377 0 0 .665377 15.98645 1' +
          '7.90835)"/></g><circle cx="17.500893" cy="18.920233" fill="none"' +
          ' r="11.048544" stroke="url(#c)" stroke-linecap="round" stroke-mi' +
          'terlimit="10" stroke-width="1.20643" transform="matrix(.82888874' +
          ' 0 0 .82888874 13.707304 13.798294)"/><rect height="4.440478" rx' +
          '="3.211203" ry="2.837393" style="opacity:.433155;fill:none;strok' +
          'e:#fff;stroke-width:1.50295;stroke-linecap:round;stroke-miterlim' +
          'it:10" transform="matrix(.50101957 .43784268 -.43176447 .5062667' +
          '3 15.98645 17.90835)" width="19.048439" x="40.373337" y=".140861' +
          '"/><circle cx="17.589281" cy="18.478292" r="8.308505" style="fil' +
          'l-rule:evenodd;stroke:#3063a3;stroke-width:1.07457;stroke-lineca' +
          'p:round;stroke-miterlimit:10;fill:url(#d)" transform="matrix(.93' +
          '060559 0 0 .93060559 11.844919 12.386414)"/><path d="m18.156915 ' +
          '7.3966938c-5.20759 0-9.4245469 4.2169572-9.4245469 9.4245472 0 1' +
          '.503975.4203072 2.887773 1.0471719 4.149903 1.25238.461613 2.582' +
          '757.775683 3.994767.775683 6.170955 0 11.099282-4.861637 11.4801' +
          '06-10.937129-1.730964-2.0455312-4.210039-3.4130042-7.097498-3.41' +
          '30042z" fill="url(#e)" fill-rule="evenodd" opacity=".834225" tra' +
          'nsform="matrix(.665377 0 0 .665377 15.98645 17.90835)"/></g></sv' +
          'g>'
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
        IconName = 'app-run'
        SVGText = 
          '<svg height="48" width="48" xmlns="http://www.w3.org/2000/svg" x' +
          'mlns:xlink="http://www.w3.org/1999/xlink"><radialGradient id="a"' +
          ' cx="31" cy="12" gradientUnits="userSpaceOnUse" r="11.125"><stop' +
          ' offset="0" stop-color="#fafcff"/><stop offset="1" stop-color="#' +
          '80b3ff"/></radialGradient><linearGradient id="b" gradientTransfo' +
          'rm="matrix(.96592585 -.25881896 .25881896 .96592585 -4.11682 6.7' +
          '29649)" gradientUnits="userSpaceOnUse" x1="28.357094" x2="17.732' +
          '124" y1="22.794661" y2="5.187518"><stop offset="0" stop-color="#' +
          'fff"/><stop offset="1" stop-color="#fff" stop-opacity="0"/></lin' +
          'earGradient><linearGradient id="c" gradientUnits="userSpaceOnUse' +
          '" x1="39.322529" x2="10.229713" y1="44.808796" y2=".83299"><stop' +
          ' offset="0" stop-color="#098633"/><stop offset="1" stop-color="#' +
          '8dfcb2"/></linearGradient><g transform="matrix(1.3998729 .375094' +
          '68 -.37509468 1.3998729 -19.391118 -8.922602)"><path d="m31 .875' +
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
          '00048 .00000048 .411765 21.323532 4.176472)"/><path d="m352.2385' +
          '9 522.61906 3.17072 4.68566 5.19675-.67311 1.8461-5.34796 5.8366' +
          '8 1.56393-1.07521 5.55452 4.16396 3.18131 5.08875-2.47254 3.0231' +
          '7 5.23628-4.68565 3.17072.6731 5.19675 5.34797 1.8461-1.56394 5.' +
          '83668-5.55452-1.07521-3.1813 4.16396 2.47254 5.08875-5.23629 3.0' +
          '2317-3.17071-4.68566s-5.19675.67311-5.19675.67311l-1.8461 5.3479' +
          '7-5.83668-1.56394 1.07521-5.55452-4.16397-3.1813-5.08874 2.47254' +
          '-3.02317-5.23629 4.68565-3.17071-.67311-5.19675-5.34796-1.8461 1' +
          '.56393-5.83669 5.55452 1.07522 3.18131-4.16397-2.47254-5.08874z"' +
          ' stroke="#2a65bd" stroke-width="2" transform="matrix(.5295138 -.' +
          '14188275 .14188275 .5295138 -236.54083 -223.97618)"/><ellipse cx' +
          '="360" cy="542.10858" rx="8.040032" ry="8.097341" stroke="#2a65b' +
          'd" stroke-width="2" transform="matrix(.5295138 -.14188275 .14188' +
          '275 .5295138 -236.54083 -223.97618)"/></g></g><g transform="matr' +
          'ix(.6818182 0 0 .6818182 15.636363 16.318181)"><path d="m45.4999' +
          '79 22.999239c0 11.874491-9.626578 21.500741-21.499708 21.500741-' +
          '11.874218 0-21.5002507-9.626359-21.5002507-21.500741 0-11.873949' +
          ' 9.6260327-21.4992185 21.5002507-21.4992185 11.87313 0 21.499708' +
          ' 9.6252695 21.499708 21.4992185z" fill="url(#c)" stroke="#00802b' +
          '" stroke-width="1.00004"/><path d="m44.49904 22.999272c0 11.3221' +
          '9-9.178617 20.500703-20.499249 20.500703-11.321667 0-20.499766-9' +
          '.178619-20.499766-20.500703 0-11.321666 9.178099-20.4992465 20.4' +
          '99766-20.4992465 11.320632 0 20.499249 9.1775805 20.499249 20.49' +
          '92465z" fill="none" opacity=".501788" stroke="#fff" stroke-opaci' +
          'ty=".5" stroke-width="1.00005"/><path d="m26.995171 25.100637-31' +
          '.9029081-8.90567 23.4134651-23.624546z" fill="#fff" stroke="#008' +
          '02a" stroke-dashoffset="1.33" stroke-linecap="round" stroke-line' +
          'join="round" transform="matrix(.64317347 -.64769705 .64769705 .6' +
          '4317347 7.644791 24.584856)"/></g></svg>'
      end
      item
        IconName = 'app-delete'
        SVGText = 
          '<svg height="48" width="48" xmlns="http://www.w3.org/2000/svg" x' +
          'mlns:xlink="http://www.w3.org/1999/xlink"><radialGradient id="a"' +
          ' cx="31" cy="12" gradientUnits="userSpaceOnUse" r="11.125"><stop' +
          ' offset="0" stop-color="#fafcff"/><stop offset="1" stop-color="#' +
          '80b3ff"/></radialGradient><linearGradient id="b" gradientTransfo' +
          'rm="matrix(.96592585 -.25881896 .25881896 .96592585 -4.11682 6.7' +
          '29649)" gradientUnits="userSpaceOnUse" x1="28.357094" x2="17.732' +
          '124" y1="22.794661" y2="5.187518"><stop offset="0" stop-color="#' +
          'fff"/><stop offset="1" stop-color="#fff" stop-opacity="0"/></lin' +
          'earGradient><linearGradient id="c" gradientUnits="userSpaceOnUse' +
          '" x1="13" x2="35.000008" y1="3.947442" y2="42.05257"><stop offse' +
          't="0" stop-color="#e78181"/><stop offset="1" stop-color="#a40000' +
          '"/></linearGradient><g transform="matrix(1.3998729 .37509468 -.3' +
          '7509468 1.3998729 -19.39112 -8.9226)"><path d="m31 .875c-.32102.' +
          '00000002-.583975.0443823-.8125.0625l-.5.03125-.03125.5-.1875 2.6' +
          '5625c-1.060092.2090559-2.041676.6405265-2.90625 1.21875l-2.40625' +
          '-2.03125-1.84375 1.84375 2.03125 2.40625c-.578224.864573-1.00969' +
          '5 1.846158-1.21875 2.90625l-2.65625.1875-.5.03125-.03125.5c-.018' +
          '119.228528-.0625.491482-.0625.8125 0 .321017.04438.583973.0625.8' +
          '125l.03125.5.5.03125 2.65625.1875c.209056 1.060092.640526 2.0416' +
          '76 1.21875 2.90625l-1.71875 2.03125-.3125.375.3125.375c.35567.41' +
          '1929.744319.80058 1.15625 1.15625l.375.3125.375-.3125 2.03125-1.' +
          '71875c.864573.578224 1.846158 1.009695 2.90625 1.21875l.1875 2.6' +
          '5625.03125.5.5.03125c.228528.01812.491482.0625.8125.0625.321018.' +
          '000001.583973-.04438.8125-.0625l.5-.03125.03125-.5.1875-2.65625c' +
          '1.060091-.209056 2.041676-.640526 2.90625-1.21875l2.03125 1.7187' +
          '5.375.3125.375-.3125c.411933-.355671.80058-.74432 1.15625-1.1562' +
          '5l.3125-.375-.3125-.375-1.71875-2.03125c.578224-.864573 1.009695' +
          '-1.846158 1.21875-2.90625l2.65625-.1875.5-.03125.03125-.5c.01811' +
          '7-.228525.0625-.491482.0625-.8125-.000001-.321015-.04438-.583972' +
          '-.0625-.8125l-.03125-.5-.5-.03125-2.65625-.1875c-.209055-1.06009' +
          '25-.640526-2.0416762-1.21875-2.90625l1.71875-2.03125.3125-.375-.' +
          '3125-.375c-.355668-.4119287-.744319-.8005796-1.15625-1.15625l-.3' +
          '75-.3125-.375.3125-2.03125 1.71875c-.864573-.5782237-1.846158-1.' +
          '0096946-2.90625-1.21875l-.1875-2.65625-.03125-.5-.5-.03125c-.228' +
          '527-.01811779-.491484-.06249877-.8125-.0625zm.505086 6.6894357c1' +
          '.932001-.0000024 3.72824 2.7516393 3.728242 4.6836403.000002 1.9' +
          '32002-2.579768 4.037843-4.511769 4.037845-1.932002.000002-3.9711' +
          '57-2.330894-3.971159-4.262895-.000001-1.932002 2.822685-4.458588' +
          '1 4.754686-4.4585903z" fill="url(#a)"/><g fill="none"><circle cx' +
          '="23.5" cy="19" r="8.5" stroke="url(#b)" stroke-linecap="square"' +
          ' stroke-width="2.42857" transform="matrix(.411765 -.00000048 .00' +
          '000048 .411765 21.323532 4.176472)"/><path d="m352.23859 522.619' +
          '06 3.17072 4.68566 5.19675-.67311 1.8461-5.34796 5.83668 1.56393' +
          '-1.07521 5.55452 4.16396 3.18131 5.08875-2.47254 3.02317 5.23628' +
          '-4.68565 3.17072.6731 5.19675 5.34797 1.8461-1.56394 5.83668-5.5' +
          '5452-1.07521-3.1813 4.16396 2.47254 5.08875-5.23629 3.02317-3.17' +
          '071-4.68566s-5.19675.67311-5.19675.67311l-1.8461 5.34797-5.83668' +
          '-1.56394 1.07521-5.55452-4.16397-3.1813-5.08874 2.47254-3.02317-' +
          '5.23629 4.68565-3.17071-.67311-5.19675-5.34796-1.8461 1.56393-5.' +
          '83669 5.55452 1.07522 3.18131-4.16397-2.47254-5.08874z" stroke="' +
          '#2a65bd" stroke-width="2" transform="matrix(.5295138 -.14188275 ' +
          '.14188275 .5295138 -236.54083 -223.97618)"/><ellipse cx="360" cy' +
          '="542.10858" rx="8.040032" ry="8.097341" stroke="#2a65bd" stroke' +
          '-width="2" transform="matrix(.5295138 -.14188275 .14188275 .5295' +
          '138 -236.54083 -223.97618)"/></g></g><g transform="matrix(.68181' +
          '82 0 0 .6818182 15.636364 16.318181)"><path d="m45.499979 22.999' +
          '239c0 11.874491-9.626578 21.500741-21.499708 21.500741-11.874218' +
          ' 0-21.5002507-9.626359-21.5002507-21.500741 0-11.873949 9.626032' +
          '7-21.4992185 21.5002507-21.4992185 11.87313 0 21.499708 9.625269' +
          '5 21.499708 21.4992185z" fill="url(#c)" stroke="#b21a1a" stroke-' +
          'width="1.00004"/><path d="m44.49904 22.999272c0 11.32219-9.17861' +
          '7 20.500703-20.499249 20.500703-11.321667 0-20.499766-9.178619-2' +
          '0.499766-20.500703 0-11.321666 9.178099-20.4992465 20.499766-20.' +
          '4992465 11.320632 0 20.499249 9.1775805 20.499249 20.4992465z" f' +
          'ill="none" opacity=".6" stroke="#fff" stroke-opacity=".75" strok' +
          'e-width="1.00005"/><path d="m16.471763 12-3.471761 3.471761 7.16' +
          '2792 7.126246c.220049.222824.220049.581162 0 .803987l-7.162792 7' +
          '.126245 3.471761 3.471761 7.126245-7.126246c.222824-.22005.58116' +
          '2-.22005.803987 0l7.126246 7.126246 3.471761-3.471761-7.126246-7' +
          '.126245c-.220051-.222825-.220051-.581163 0-.803987l7.126246-7.12' +
          '6246-3.471761-3.471761-7.126246 7.126245c-.222825.220051-.581163' +
          '.220051-.803987 0z" fill="#fff" fill-rule="evenodd"/></g></svg>'
      end
      item
        IconName = 'replace-blue'
        SVGText = 
          '<svg height="48" width="48" xmlns="http://www.w3.org/2000/svg" x' +
          'mlns:xlink="http://www.w3.org/1999/xlink"><linearGradient id="a"' +
          ' gradientUnits="userSpaceOnUse" x1="25.27776876211" x2="24.95671' +
          '959522" y1="-.28571423142" y2="47.88877673545"><stop offset="0" ' +
          'stop-color="#fff"/><stop offset="1" stop-color="#dad3d3" stop-op' +
          'acity="0"/></linearGradient><radialGradient id="b" cx="62.202274' +
          '" cy="-5.713216" gradientTransform="matrix(-7.565785 -.00000062 ' +
          '.0000004 -4.8230546 494.60904 -26.555114)" gradientUnits="userSp' +
          'aceOnUse" r="9.755284"><stop offset="0" stop-color="#4db1ef"/><s' +
          'top offset="1" stop-color="#006cbd"/></radialGradient><linearGra' +
          'dient id="c" gradientTransform="matrix(.6104318 0 0 -.49747444 7' +
          '.790176 24.676459)" gradientUnits="userSpaceOnUse" x1="22.312141' +
          '" x2="22.312141" xlink:href="#d" y1="20.909737" y2="24.184505"/>' +
          '<linearGradient id="d"><stop offset="0" stop-color="#fff"/><stop' +
          ' offset="1" stop-color="#fff"/></linearGradient><linearGradient ' +
          'id="e" gradientTransform="matrix(-.6104318 0 0 .49747444 40.2098' +
          '23 21.323543)" gradientUnits="userSpaceOnUse" x1="22.312141" x2=' +
          '"22.312141" xlink:href="#d" y1="20.909737" y2="24.184505"/><g tr' +
          'ansform="translate(0 1)"><path d="m45.499979 22.999239c0 11.8744' +
          '91-9.626578 21.500741-21.499708 21.500741-11.874218 0-21.5002507' +
          '-9.626359-21.5002507-21.500741 0-11.873949 9.6260327-21.4992185 ' +
          '21.5002507-21.4992185 11.87313 0 21.499708 9.6252695 21.499708 2' +
          '1.4992185z" fill="url(#b)" stroke="#1363a0" stroke-width="1.0000' +
          '4"/><path d="m44.49904 22.999272c0 11.32219-9.178617 20.500703-2' +
          '0.499249 20.500703-11.321667 0-20.499766-9.178619-20.499766-20.5' +
          '00703 0-11.321666 9.178099-20.4992465 20.499766-20.4992465 11.32' +
          '0632 0 20.499249 9.1775805 20.499249 20.4992465z" fill="none" op' +
          'acity=".524823" stroke="url(#a)" stroke-width="1.00005"/><g stro' +
          'ke="#fff" stroke-width=".799858"><path d="m19.265171 6.414529-9.' +
          '687318 7.264651 9.687318 7.605182.02752-4.699321h8.256237c4.0906' +
          '3-.138906 6.773926 1.566437 6.660031 3.632325v.09081 4.949044c9.' +
          '00068-5.123538 3.199501-14.585369-6.660029-14.5066l-8.283758.045' +
          '4z" fill="url(#c)"/><path d="m28.734828 39.585472 9.687318-7.264' +
          '65-9.687318-7.605182-.02752 4.699321h-8.256237c-4.09063.138906-6' +
          '.773926-1.566437-6.660031-3.632325v-.09081-4.949044c-9.0006791 5' +
          '.123538-3.199501 14.585368 6.660029 14.506599l8.283758-.0454z" f' +
          'ill="url(#e)"/></g></g></svg>'
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
        IconName = 'doc-new'
        SVGText = 
          '<svg height="48" viewBox="0 0 48 48" width="48" xmlns="http://ww' +
          'w.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink"><l' +
          'inearGradient id="a" gradientUnits="userSpaceOnUse" x1="17.48824' +
          '5" x2="-6.23667" y1="24.410179" y2="-17.332663"><stop offset="0"' +
          ' stop-color="#ff8200"/><stop offset="1" stop-color="#ffca00"/></' +
          'linearGradient><radialGradient id="b" cx="24" cy="24" gradientUn' +
          'its="userSpaceOnUse" r="14.5"><stop offset="0" stop-color="#fff8' +
          '89"/><stop offset="1" stop-color="#ffca00"/></radialGradient><re' +
          'ct fill="#ececec" height="41.666668" rx="2.5" stroke="#7a7c77" s' +
          'troke-miterlimit="10.433" stroke-width="1.66667" width="38.33333' +
          '6" x="4.833332" y="3.166666"/><rect fill="none" height="38.51105' +
          '1" rx=".863101" stroke="#fff" stroke-miterlimit="10.433" stroke-' +
          'width="1.53495" width="35.178051" x="6.367477" y="4.786501"/><g ' +
          'fill="#91938e"><path d="m10.000001 8h28v4h-28z"/><path d="m10 14' +
          '.999999h28v4h-28z"/><path d="m10 21.999998h28v4h-28z"/><path d="' +
          'm10 28.999998h28v4h-28z"/><path d="m10 36h20v4h-20z"/></g><g tra' +
          'nsform="matrix(.59090909 0 0 .59090909 19.818182 -.181819)"><pat' +
          'h d="m22.822246 20.36205c-.320226.359569-8.497099-2.114621-8.895' +
          '568-1.844331-.384288.260672-.742616 8.535238-1.183456 8.681136-.' +
          '457108.151283-6.3013936-6.079865-6.7816229-6.045021-.4631383.033' +
          '6-4.910743 7.020425-5.36547096 6.926356-.47150855-.09754-2.41723' +
          '404-8.416014-2.85054694-8.625953-.4178917-.202467-7.7630402 3.62' +
          '4494-8.1098122 3.315665-.359569-.320226 2.1146212-8.4971 1.84433' +
          '05-8.895568-.2606713-.384288-8.5352375-.742616-8.6811355-1.18345' +
          '6-.151283-.457109 6.079865-6.3013939 6.045021-6.7816232-.0336-.4' +
          '631384-7.020425-4.91074302-6.926356-5.36547098.09754-.47150856 8' +
          '.4160144-2.41723402 8.625953-2.85054692.2024671-.4178917-3.62449' +
          '4-7.7630409-3.315665-8.1098119.320226-.359569 8.4970997 2.114620' +
          '9 8.8955683 1.8443302.3842875-.2606713.7426159-8.5352372 1.18345' +
          '61-8.6811362.4571083-.151282 6.3013934 6.079866 6.7816227 6.0450' +
          '22.4631384-.0336 4.9107431-7.020425 5.365471-6.926357.4715086.09' +
          '754 2.4172339 8.4160151 2.8505469 8.6259537.417892.2024671 7.763' +
          '041-3.6244947 8.109812-3.3156647.359569.320226-2.114621 8.497099' +
          '3-1.84433 8.895568.260671.3842875 8.535237.7426159 8.681136 1.18' +
          '34561.151282.4571083-6.079866 6.3013934-6.045022 6.7816227.0336.' +
          '4631384 7.020425 4.910743 6.926357 5.365471-.09754.4715086-8.416' +
          '015 2.4172342-8.625954 2.8505472-.202467.417891 3.624495 7.76304' +
          ' 3.315665 8.109812z" fill="url(#a)" stroke="#d58b18" stroke-widt' +
          'h=".657155" transform="matrix(.93863685 0 0 .93863685 19.283273 ' +
          '19.332406)"/><circle cx="24" cy="24" fill="url(#b)" r="14.5"/></' +
          'g></svg>'
      end
      item
        IconName = 'copy-docs'
        SVGText = 
          '<svg height="48" width="48" xmlns="http://www.w3.org/2000/svg" x' +
          'mlns:xlink="http://www.w3.org/1999/xlink"><linearGradient id="a"' +
          ' gradientTransform="matrix(.9754464 0 0 1.0602677 1.262032 .9029' +
          '02)" gradientUnits="userSpaceOnUse" x1="11" x2="11" y1="17" y2="' +
          '-3"><stop offset="0" stop-color="#f0f0ee"/><stop offset="1" stop' +
          '-color="#dedede"/></linearGradient><linearGradient id="b" gradie' +
          'ntUnits="userSpaceOnUse" x1="35.957069" x2="10.955403" y1="38.00' +
          '0122" y2="11.048571"><stop offset="0" stop-color="#df1111"/><sto' +
          'p offset="1" stop-color="#f89f9f"/></linearGradient><g transform' +
          '="matrix(1.5350878 0 0 1.5350878 -4.159986 -3.934212)"><rect fil' +
          'l="url(#a)" height="16.964285" rx="1.017857" stroke="#888a85" st' +
          'roke-miterlimit="10.433" stroke-width=".678571" width="15.607142' +
          '" x="3.700648" y="3.553572"/><rect fill="none" height="15.607142' +
          '" rx=".339286" stroke="#fff" stroke-miterlimit="10.433" stroke-w' +
          'idth=".678571" width="14.129219" x="4.5" y="4.232143"/><path id=' +
          '"c" d="m5.890584 5.820617h10.857142v1.002435h-10.857142z" fill="' +
          '#91938e"/><use transform="matrix(1.0340909 0 0 1.6 -.200815 -3.6' +
          '92857)" xlink:href="#c"/><use transform="matrix(1.0340909 0 0 1.' +
          '6 -.200816 -.88604)" xlink:href="#c"/><use transform="matrix(1.0' +
          '340909 0 0 1.6 -.200816 1.920778)" xlink:href="#c"/><use transfo' +
          'rm="matrix(1.0340909 0 0 1.6 -.200816 4.727596)" xlink:href="#c"' +
          '/><use transform="matrix(.73863634 0 0 1.6 1.539584 7.534414)" x' +
          'link:href="#c"/></g><g transform="matrix(1.5350878 0 0 1.5350878' +
          ' .840014 1.065788)"><rect fill="url(#a)" height="16.964285" rx="' +
          '1.017857" stroke="#888a85" stroke-miterlimit="10.433" stroke-wid' +
          'th=".678571" width="15.607142" x="3.700648" y="3.553572"/><rect ' +
          'fill="none" height="15.607142" rx=".339286" stroke="#fff" stroke' +
          '-miterlimit="10.433" stroke-width=".678571" width="14.129219" x=' +
          '"4.5" y="4.232143"/><path d="m5.890584 5.820617h10.857142v1.0024' +
          '35h-10.857142z" fill="#91938e"/><use transform="matrix(1.0340909' +
          ' 0 0 1.6 -.200815 -3.692857)" xlink:href="#c"/><use transform="m' +
          'atrix(1.0340909 0 0 1.6 -.200816 -.88604)" xlink:href="#c"/><use' +
          ' transform="matrix(1.0340909 0 0 1.6 -.200816 1.920778)" xlink:h' +
          'ref="#c"/><use transform="matrix(1.0340909 0 0 1.6 -.200816 4.72' +
          '7596)" xlink:href="#c"/><use transform="matrix(.73863634 0 0 1.6' +
          ' 1.539584 7.534414)" xlink:href="#c"/></g><g transform="matrix(1' +
          '.5350878 0 0 1.5350878 6.840014 7.065788)"><rect fill="url(#a)" ' +
          'height="16.964285" rx="1.017857" stroke="#888a85" stroke-miterli' +
          'mit="10.433" stroke-width=".678571" width="15.607142" x="3.70064' +
          '8" y="3.553572"/><rect fill="none" height="15.607142" rx=".33928' +
          '6" stroke="#fff" stroke-miterlimit="10.433" stroke-width=".67857' +
          '1" width="14.129219" x="4.5" y="4.232143"/><path d="m5.890584 5.' +
          '820617h10.857142v1.002435h-10.857142z" fill="#91938e"/><use tran' +
          'sform="matrix(1.0340909 0 0 1.6 -.200815 -3.692857)" xlink:href=' +
          '"#c"/><use transform="matrix(1.0340909 0 0 1.6 -.200816 -.88604)' +
          '" xlink:href="#c"/><use transform="matrix(1.0340909 0 0 1.6 -.20' +
          '0816 1.920778)" xlink:href="#c"/><use transform="matrix(1.034090' +
          '9 0 0 1.6 -.200816 4.727596)" xlink:href="#c"/><use transform="m' +
          'atrix(.73863634 0 0 1.6 1.539584 7.534414)" xlink:href="#c"/></g' +
          '><g stroke-miterlimit="10" transform="matrix(0 -.70575829 -.7057' +
          '5139 0 50.526263 51.451791)"><path d="m14.519136 38.5 18.005029-' +
          '.0039v-12.991632l7.995366-.0078-17.144722-19.9974545-16.8462505 ' +
          '19.9980705 7.9958815.0038z" style="fill-rule:evenodd;stroke:#850' +
          '006;stroke-width:1.00555;stroke-linecap:round;stroke-linejoin:ro' +
          'und;fill:url(#b)"/><path d="m15.520704 37.496094 16.001405.00390' +
          '6v-12.99295l6.816811-.015625-14.954276-17.4525854-14.7065267 17.' +
          '4569424 6.8399007.0052z" fill="none" opacity=".481283" stroke="#' +
          'fff"/></g></svg>'
      end
      item
        IconName = 'insert-line'
        SVGText = 
          '<svg height="48" width="48" xmlns="http://www.w3.org/2000/svg" x' +
          'mlns:xlink="http://www.w3.org/1999/xlink"><linearGradient id="a"' +
          ' gradientUnits="userSpaceOnUse" x1="58.757137" x2="1.841824" y1=' +
          '"88.81591" y2="-12.705333"><stop offset="0" stop-color="#bbd6ff"' +
          '/><stop offset="1" stop-color="#0057ae"/></linearGradient><g str' +
          'oke="#2d70c0" transform="matrix(.3518614 0 0 .3518614 1.691099 1' +
          '.48087)"><path d="m78.454694 12.843495h-61.366648a2 2 135 0 0 -2' +
          ' 2v64.208675a2 2 45 0 0 2 2h24.420282a2 2 45 0 1 2 2v7.368113a.6' +
          '8024386.68024386 18.784295 0 0 1.219421.414752l11.771298-15.3026' +
          '87a2.6000001 2.6000001 90 0 0 0-3.170496l-11.771299-15.302688a.6' +
          '8024387.68024387 161.2157 0 0 -1.219421.414752v7.368112a2 2 135 ' +
          '0 1 -2 2h-10.21014a2 2 45 0 1 -2-2v-35.788393a2 2 135 0 1 2-2l47' +
          '.156507.000001a2 2 135 0 0 2-2v-10.210141a2 2 45 0 0 -2-2z" fill' +
          '="url(#a)" fill-rule="evenodd" stroke-width="3.512827" transform' +
          '="matrix(1.3223216 0 0 1.1137521 -16.818757 -10.929685)"/><g fil' +
          'l="#74a6e0" stroke-dashoffset="1.33" stroke-linecap="round" stro' +
          'ke-linejoin="round" stroke-width="4.263042"><path d="m52.964756 ' +
          '30.826008h69.190002v9.507419h-69.190002z"/><path d="m52.964756 1' +
          '10.40279h69.19001v9.507419h-69.19001z"/><path d="m81.187096 50.5' +
          '22266h41.165615v9.903306h-41.165615z"/><path d="m81.187096 70.41' +
          '6458h41.165615v9.903306h-41.165615z"/><path d="m81.187096 90.310' +
          '654h41.165615v9.903306h-41.165615z"/></g></g></svg>'
      end
      item
        IconName = 'list-new'
        SVGText = 
          '<svg height="48" width="48" xmlns="http://www.w3.org/2000/svg" x' +
          'mlns:xlink="http://www.w3.org/1999/xlink"><linearGradient id="a"' +
          ' gradientUnits="userSpaceOnUse" x1="17.488245" x2="-6.23667" y1=' +
          '"24.410179" y2="-17.332663"><stop offset="0" stop-color="#ff8200' +
          '"/><stop offset="1" stop-color="#ffca00"/></linearGradient><radi' +
          'alGradient id="b" cx="24" cy="24" gradientUnits="userSpaceOnUse"' +
          ' r="14.5"><stop offset="0" stop-color="#fff889"/><stop offset="1' +
          '" stop-color="#ffca00"/></radialGradient><rect fill="#fff" heigh' +
          't="41.66666744204" rx="2.49999980091" stroke="#7a7c77" stroke-mi' +
          'terlimit="10.433" stroke-width="1.66666571523" width="38.3333335' +
          '5545" x="4.83333242904" y="3.16666710887"/><g fill="#91938e"><pa' +
          'th d="m15.00000041473 7.50000140705h24.00000005363v3.99999919023' +
          'h-24.00000005363z"/><path d="m9.00000040132 7.50000140705h3.9999' +
          '9919023v3.99999919023h-3.99999919023z"/><path d="m15.00000041473' +
          ' 15.00000080977h24.00000005363v3.99999919023h-24.00000005363z"/>' +
          '<path d="m9.00000040132 15.00000080977h3.99999919023v3.999999190' +
          '23h-3.99999919023z"/><path d="m15.00000041473 22.00000123477h24.' +
          '00000005363v3.99999919023h-24.00000005363z"/><path d="m9.0000004' +
          '0132 22.00000123477h3.99999919023v3.99999919023h-3.99999919023z"' +
          '/><path d="m15.00000041473 28.99999920363h24.00000005363v3.99999' +
          '919023h-24.00000005363z"/><path d="m9.00000040132 28.99999920363' +
          'h3.99999919023v3.99999919023h-3.99999919023z"/><path d="m15.0000' +
          '0041473 35.99999962863h24.00000005363v3.99999919023h-24.00000005' +
          '363z"/><path d="m9.00000040132 35.99999962863h3.99999919023v3.99' +
          '999919023h-3.99999919023z"/></g><g transform="matrix(.59090909 0' +
          ' 0 .59090909 19.818182 -.181819)"><path d="m22.822246 20.36205c-' +
          '.320226.359569-8.497099-2.114621-8.895568-1.844331-.384288.26067' +
          '2-.742616 8.535238-1.183456 8.681136-.457108.151283-6.3013936-6.' +
          '079865-6.7816229-6.045021-.4631383.0336-4.910743 7.020425-5.3654' +
          '7096 6.926356-.47150855-.09754-2.41723404-8.416014-2.85054694-8.' +
          '625953-.4178917-.202467-7.7630402 3.624494-8.1098122 3.315665-.3' +
          '59569-.320226 2.1146212-8.4971 1.8443305-8.895568-.2606713-.3842' +
          '88-8.5352375-.742616-8.6811355-1.183456-.151283-.457109 6.079865' +
          '-6.3013939 6.045021-6.7816232-.0336-.4631384-7.020425-4.91074302' +
          '-6.926356-5.36547098.09754-.47150856 8.4160144-2.41723402 8.6259' +
          '53-2.85054692.2024671-.4178917-3.624494-7.7630409-3.315665-8.109' +
          '8119.320226-.359569 8.4970997 2.1146209 8.8955683 1.8443302.3842' +
          '875-.2606713.7426159-8.5352372 1.1834561-8.6811362.4571083-.1512' +
          '82 6.3013934 6.079866 6.7816227 6.045022.4631384-.0336 4.9107431' +
          '-7.020425 5.365471-6.926357.4715086.09754 2.4172339 8.4160151 2.' +
          '8505469 8.6259537.417892.2024671 7.763041-3.6244947 8.109812-3.3' +
          '156647.359569.320226-2.114621 8.4970993-1.84433 8.895568.260671.' +
          '3842875 8.535237.7426159 8.681136 1.1834561.151282.4571083-6.079' +
          '866 6.3013934-6.045022 6.7816227.0336.4631384 7.020425 4.910743 ' +
          '6.926357 5.365471-.09754.4715086-8.416015 2.4172342-8.625954 2.8' +
          '505472-.202467.417891 3.624495 7.76304 3.315665 8.109812z" fill=' +
          '"url(#a)" stroke="#d58b18" stroke-width=".657155" transform="mat' +
          'rix(.93863685 0 0 .93863685 19.283273 19.332406)"/><circle cx="2' +
          '4" cy="24" fill="url(#b)" r="14.5"/></g></svg>'
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
        IconName = 'chevron-down'
        SVGText = 
          '<svg height="48" viewBox="0 0 48 48" width="48" xmlns="http://ww' +
          'w.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink"><l' +
          'inearGradient id="a" gradientUnits="userSpaceOnUse" x1="5.573668' +
          '" x2="34.058079" y1="10" y2="11.165337"><stop offset="0" stop-co' +
          'lor="#99bfec" stop-opacity=".99537"/><stop offset="1" stop-color' +
          '="#2574ca"/></linearGradient><path d="m9.5128104 24.296521-4.302' +
          '6195-3.820939 16.2633411-10.136733-13.6249615-12.0996396 5.13579' +
          '15-3.2010733 17.927581 15.9205789z" style="stroke:#2a6cbf;stroke' +
          '-width:1.91213;stroke-linecap:round;stroke-linejoin:round;stroke' +
          '-dashoffset:1.33;fill:url(#a)" transform="matrix(.07874668088 .8' +
          '9206451236 -1.19903978969 .10584481434 34.70572286224 9.04373648' +
          '306)"/></svg>'
      end
      item
        IconName = 'menu-dark-s'
        SVGText = 
          '<svg height="48" viewBox="0 0 48 48" width="48" xmlns="http://ww' +
          'w.w3.org/2000/svg"><g fill="#4992d6" stroke="#3a7ab3" stroke-das' +
          'hoffset="1.3467" stroke-width=".940136"><rect height="7.059865" ' +
          'ry=".429871" width="39.059864" x="4.470068" y="34.47007"/><rect ' +
          'height="7.059865" ry=".429871" width="39.059864" x="4.470068" y=' +
          '"20.470068"/><rect height="7.059865" ry=".429871" width="39.0598' +
          '64" x="4.470068" y="6.470068"/></g></svg>'
      end
      item
        IconName = 'list-delete'
        SVGText = 
          '<?xml version="1.0" encoding="UTF-8" standalone="no"?>'#10'<svg'#10'   h' +
          'eight="48"'#10'   width="48"'#10'   version="1.1"'#10'   id="svg27"'#10'   sodip' +
          'odi:docname="list-delete.svg"'#10'   inkscape:version="1.4 (86a8ad7,' +
          ' 2024-10-11)"'#10'   xml:space="preserve"'#10'   xmlns:inkscape="http://' +
          'www.inkscape.org/namespaces/inkscape"'#10'   xmlns:sodipodi="http://' +
          'sodipodi.sourceforge.net/DTD/sodipodi-0.dtd"'#10'   xmlns:xlink="htt' +
          'p://www.w3.org/1999/xlink"'#10'   xmlns="http://www.w3.org/2000/svg"' +
          #10'   xmlns:svg="http://www.w3.org/2000/svg"><defs'#10'     id="defs27' +
          '"><linearGradient'#10'       id="linearGradient3811"><stop'#10'         ' +
          'id="stop3813"'#10'         offset="0"'#10'         style="stop-color:#cc' +
          '0000;stop-opacity:1;" /><stop'#10'         id="stop3815"'#10'         of' +
          'fset="1"'#10'         style="stop-color:#c22f2f;stop-opacity:1" /></' +
          'linearGradient><linearGradient'#10'       id="linearGradient5106"><s' +
          'top'#10'         id="stop5108"'#10'         offset="0"'#10'         style="s' +
          'top-color:#e87d7d;stop-opacity:1;" /><stop'#10'         id="stop5110' +
          '"'#10'         offset="1"'#10'         style="stop-color:#a40000;stop-op' +
          'acity:1" /></linearGradient><linearGradient'#10'       inkscape:coll' +
          'ect="always"'#10'       xlink:href="#linearGradient5106"'#10'       id="' +
          'linearGradient1"'#10'       x1="13.000001"'#10'       y1="3.947439"'#10'    ' +
          '   x2="35.000004"'#10'       y2="42.052559"'#10'       gradientUnits="us' +
          'erSpaceOnUse" /></defs><sodipodi:namedview'#10'     id="namedview27"' +
          #10'     pagecolor="#ffffff"'#10'     bordercolor="#666666"'#10'     border' +
          'opacity="1.0"'#10'     inkscape:showpageshadow="2"'#10'     inkscape:pag' +
          'eopacity="0.0"'#10'     inkscape:pagecheckerboard="0"'#10'     inkscape:' +
          'deskcolor="#d1d1d1"'#10'     inkscape:zoom="10.457435"'#10'     inkscape' +
          ':cx="24.049874"'#10'     inkscape:cy="23.954248"'#10'     inkscape:curre' +
          'nt-layer="svg27" /><linearGradient'#10'     id="a"'#10'     gradientTran' +
          'sform="matrix(.9754464 0 0 1.0602677 1.262032 .902902)"'#10'     gra' +
          'dientUnits="userSpaceOnUse"'#10'     x1="11"'#10'     x2="11"'#10'     y1="1' +
          '7"'#10'     y2="-3"><stop'#10'       offset="0"'#10'       stop-color="#f0f0' +
          'ee"'#10'       id="stop1" /><stop'#10'       offset="1"'#10'       stop-colo' +
          'r="#dedede"'#10'       id="stop2" /></linearGradient><radialGradient' +
          #10'     id="b"'#10'     cx="62.202274"'#10'     cy="-5.713216"'#10'     gradie' +
          'ntTransform="matrix(-7.565785,-6.2e-7,4e-7,-4.8230546,494.60904,' +
          '-26.555114)"'#10'     gradientUnits="userSpaceOnUse"'#10'     r="9.75528' +
          '3"><stop'#10'       offset="0"'#10'       stop-color="#e78181"'#10'       id' +
          '="stop3" /><stop'#10'       offset=".25288007"'#10'       stop-color="#e' +
          '15f5f"'#10'       id="stop4" /><stop'#10'       offset=".68271071"'#10'     ' +
          '  stop-color="#c22f2f"'#10'       id="stop5" /><stop'#10'       offset="' +
          '1"'#10'       stop-color="#a40000"'#10'       id="stop6" /></radialGradi' +
          'ent><linearGradient'#10'     id="c"'#10'     gradientTransform="matrix(1' +
          '.0805272,0,0,1.0805026,-1.508286,-3.350759)"'#10'     gradientUnits=' +
          '"userSpaceOnUse"'#10'     x1="15.046636"'#10'     x2="15.046636"'#10'     y1' +
          '="44.787998"'#10'     y2="3.885126"><stop'#10'       offset="0"'#10'       s' +
          'top-color="#a40000"'#10'       id="stop7" /><stop'#10'       offset="1"'#10 +
          '       stop-color="#c22f2f"'#10'       id="stop8" /></linearGradient' +
          '><linearGradient'#10'     id="d"'#10'     gradientUnits="userSpaceOnUse"' +
          #10'     x1="25.27776876211"'#10'     x2="24.95671959522"'#10'     y1="-.28' +
          '571423142"'#10'     y2="47.88877673545"><stop'#10'       offset="0"'#10'    ' +
          '   stop-color="#fff"'#10'       id="stop9" /><stop'#10'       offset="1"' +
          #10'       stop-color="#fff"'#10'       stop-opacity="0"'#10'       id="sto' +
          'p10" /></linearGradient><g'#10'     id="g5541"'#10'     transform="matri' +
          'x(2.4561405,0,0,2.4561405,-4.2559789,-5.561405)"'#10'     inkscape:l' +
          'abel="List"><rect'#10'       height="16.964285"'#10'       id="rect4950"' +
          #10'       rx="1.0178571"'#10'       ry="1.0178571"'#10'       style="color' +
          ':#000000;display:inline;overflow:visible;visibility:visible;opac' +
          'ity:1;fill:#ffffff;fill-opacity:1;fill-rule:nonzero;stroke:#7a7c' +
          '77;stroke-width:0.678571;stroke-linecap:butt;stroke-linejoin:mit' +
          'er;stroke-miterlimit:10.433;stroke-dasharray:none;stroke-dashoff' +
          'set:0;stroke-opacity:1;marker:none;marker-start:none;marker-mid:' +
          'none;marker-end:none;enable-background:accumulate"'#10'       width=' +
          '"15.607142"'#10'       x="3.7006481"'#10'       y="3.5535717" /><g'#10'     ' +
          '  id="g2"'#10'       transform="translate(-9.4432567e-8,-0.61071438)' +
          '"><rect'#10'         height="1.6285713"'#10'         id="rect5547"'#10'     ' +
          '    style="color:#000000;display:inline;overflow:visible;visibil' +
          'ity:visible;opacity:1;fill:#91938e;fill-opacity:1;fill-rule:nonz' +
          'ero;stroke:none;stroke-width:1.46969;stroke-linecap:butt;stroke-' +
          'linejoin:miter;stroke-miterlimit:10.433;stroke-dasharray:none;st' +
          'roke-dashoffset:0;stroke-opacity:1;marker:none;marker-start:none' +
          ';marker-mid:none;marker-end:none;enable-background:accumulate"'#10' ' +
          '        width="9.7714281"'#10'         x="7.8399339"'#10'         y="5.9' +
          '285717" /><rect'#10'         height="1.6285713"'#10'         id="rect1"'#10 +
          '         style="color:#000000;display:inline;overflow:visible;vi' +
          'sibility:visible;opacity:1;fill:#91938e;fill-opacity:1;fill-rule' +
          ':nonzero;stroke:none;stroke-width:0.599999;stroke-linecap:butt;s' +
          'troke-linejoin:miter;stroke-miterlimit:10.433;stroke-dasharray:n' +
          'one;stroke-dashoffset:0;stroke-opacity:1;marker:none;marker-star' +
          't:none;marker-mid:none;marker-end:none;enable-background:accumul' +
          'ate"'#10'         width="1.6285713"'#10'         x="5.3970771"'#10'         ' +
          'y="5.9285717" /></g><g'#10'       id="g3"'#10'       transform="translat' +
          'e(-3.6715631e-8,1.8321428)"><rect'#10'         height="1.6285713"'#10'  ' +
          '       id="rect2"'#10'         style="color:#000000;display:inline;o' +
          'verflow:visible;visibility:visible;opacity:1;fill:#91938e;fill-o' +
          'pacity:1;fill-rule:nonzero;stroke:none;stroke-width:1.46969;stro' +
          'ke-linecap:butt;stroke-linejoin:miter;stroke-miterlimit:10.433;s' +
          'troke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1;marker' +
          ':none;marker-start:none;marker-mid:none;marker-end:none;enable-b' +
          'ackground:accumulate"'#10'         width="9.7714281"'#10'         x="7.8' +
          '399339"'#10'         y="6.5392857" /><rect'#10'         height="1.628571' +
          '3"'#10'         id="rect3"'#10'         style="color:#000000;display:inl' +
          'ine;overflow:visible;visibility:visible;opacity:1;fill:#91938e;f' +
          'ill-opacity:1;fill-rule:nonzero;stroke:none;stroke-width:0.59999' +
          '9;stroke-linecap:butt;stroke-linejoin:miter;stroke-miterlimit:10' +
          '.433;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1;' +
          'marker:none;marker-start:none;marker-mid:none;marker-end:none;en' +
          'able-background:accumulate"'#10'         width="1.6285713"'#10'         ' +
          'x="5.3970766"'#10'         y="6.5392857" /></g><g'#10'       id="g5"'#10'   ' +
          '    transform="translate(2.1001316e-8,4.2749996)"><rect'#10'        ' +
          ' height="1.6285713"'#10'         id="rect4"'#10'         style="color:#0' +
          '00000;display:inline;overflow:visible;visibility:visible;opacity' +
          ':1;fill:#91938e;fill-opacity:1;fill-rule:nonzero;stroke:none;str' +
          'oke-width:1.46969;stroke-linecap:butt;stroke-linejoin:miter;stro' +
          'ke-miterlimit:10.433;stroke-dasharray:none;stroke-dashoffset:0;s' +
          'troke-opacity:1;marker:none;marker-start:none;marker-mid:none;ma' +
          'rker-end:none;enable-background:accumulate"'#10'         width="9.77' +
          '14281"'#10'         x="7.8399339"'#10'         y="6.9464288" /><rect'#10'   ' +
          '      height="1.6285713"'#10'         id="rect5"'#10'         style="col' +
          'or:#000000;display:inline;overflow:visible;visibility:visible;op' +
          'acity:1;fill:#91938e;fill-opacity:1;fill-rule:nonzero;stroke:non' +
          'e;stroke-width:0.599999;stroke-linecap:butt;stroke-linejoin:mite' +
          'r;stroke-miterlimit:10.433;stroke-dasharray:none;stroke-dashoffs' +
          'et:0;stroke-opacity:1;marker:none;marker-start:none;marker-mid:n' +
          'one;marker-end:none;enable-background:accumulate"'#10'         width' +
          '="1.6285713"'#10'         x="5.3970766"'#10'         y="6.9464288" /></g' +
          '><g'#10'       id="g7"'#10'       transform="translate(2.1001316e-8,6.71' +
          '78567)"><rect'#10'         height="1.6285713"'#10'         id="rect6"'#10'  ' +
          '       style="color:#000000;display:inline;overflow:visible;visi' +
          'bility:visible;opacity:1;fill:#91938e;fill-opacity:1;fill-rule:n' +
          'onzero;stroke:none;stroke-width:1.46969;stroke-linecap:butt;stro' +
          'ke-linejoin:miter;stroke-miterlimit:10.433;stroke-dasharray:none' +
          ';stroke-dashoffset:0;stroke-opacity:1;marker:none;marker-start:n' +
          'one;marker-mid:none;marker-end:none;enable-background:accumulate' +
          '"'#10'         width="9.7714281"'#10'         x="7.8399339"'#10'         y="' +
          '7.3535714" /><rect'#10'         height="1.6285713"'#10'         id="rect' +
          '7"'#10'         style="color:#000000;display:inline;overflow:visible' +
          ';visibility:visible;opacity:1;fill:#91938e;fill-opacity:1;fill-r' +
          'ule:nonzero;stroke:none;stroke-width:0.599999;stroke-linecap:but' +
          't;stroke-linejoin:miter;stroke-miterlimit:10.433;stroke-dasharra' +
          'y:none;stroke-dashoffset:0;stroke-opacity:1;marker:none;marker-s' +
          'tart:none;marker-mid:none;marker-end:none;enable-background:accu' +
          'mulate"'#10'         width="1.6285713"'#10'         x="5.3970766"'#10'      ' +
          '   y="7.3535714" /></g><g'#10'       id="g11"'#10'       transform="tran' +
          'slate(2.1001316e-8,11.196428)"><rect'#10'         height="1.6285713"' +
          #10'         id="rect10"'#10'         style="color:#000000;display:inli' +
          'ne;overflow:visible;visibility:visible;opacity:1;fill:#91938e;fi' +
          'll-opacity:1;fill-rule:nonzero;stroke:none;stroke-width:1.46969;' +
          'stroke-linecap:butt;stroke-linejoin:miter;stroke-miterlimit:10.4' +
          '33;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1;ma' +
          'rker:none;marker-start:none;marker-mid:none;marker-end:none;enab' +
          'le-background:accumulate"'#10'         width="9.7714281"'#10'         x=' +
          '"7.8399339"'#10'         y="5.7249999" /><rect'#10'         height="1.62' +
          '85713"'#10'         id="rect11"'#10'         style="color:#000000;displa' +
          'y:inline;overflow:visible;visibility:visible;opacity:1;fill:#919' +
          '38e;fill-opacity:1;fill-rule:nonzero;stroke:none;stroke-width:0.' +
          '599999;stroke-linecap:butt;stroke-linejoin:miter;stroke-miterlim' +
          'it:10.433;stroke-dasharray:none;stroke-dashoffset:0;stroke-opaci' +
          'ty:1;marker:none;marker-start:none;marker-mid:none;marker-end:no' +
          'ne;enable-background:accumulate"'#10'         width="1.6285713"'#10'    ' +
          '     x="5.3970766"'#10'         y="5.7249999" /></g></g><g'#10'     id="' +
          'layer1-5"'#10'     inkscape:label="Error"'#10'     transform="matrix(0.5' +
          '9090911,0,0,0.5909091,19.818182,0.40909062)"><path'#10'       d="m 4' +
          '5.499979,22.999239 c 0,11.874491 -9.626578,21.500741 -21.499708,' +
          '21.500741 -11.874218,0 -21.5002507,-9.626359 -21.5002507,-21.500' +
          '741 0,-11.873949 9.6260327,-21.4992185 21.5002507,-21.4992185 11' +
          '.87313,0 21.499708,9.6252695 21.499708,21.4992185 z"'#10'       id="' +
          'path6495-4"'#10'       style="fill:url(#linearGradient1);fill-opacit' +
          'y:1;fill-rule:nonzero;stroke:#cc0000;stroke-width:1.00004;stroke' +
          '-miterlimit:4;stroke-dasharray:none;stroke-opacity:1" /><path'#10'  ' +
          '     d="m 44.49904,22.999272 c 0,11.32219 -9.178617,20.500703 -2' +
          '0.499249,20.500703 -11.321667,0 -20.499766,-9.178619 -20.499766,' +
          '-20.500703 0,-11.321666 9.178099,-20.4992465 20.499766,-20.49924' +
          '65 11.320632,0 20.499249,9.1775805 20.499249,20.4992465 z"'#10'     ' +
          '  id="path8655-1"'#10'       style="display:inline;opacity:0.6;fill:' +
          'none;fill-opacity:1;fill-rule:nonzero;stroke:#ffffff;stroke-widt' +
          'h:1.00005;stroke-miterlimit:4;stroke-dasharray:none;stroke-opaci' +
          'ty:0.5" /><path'#10'       d="m 16.471763,12 -3.471761,3.471761 7.16' +
          '2792,7.126246 c 0.220049,0.222824 0.220049,0.581162 0,0.803987 L' +
          ' 13.000002,30.528239 16.471763,34 23.598008,26.873754 c 0.222824' +
          ',-0.22005 0.581162,-0.22005 0.803987,0 L 31.528241,34 35.000002,' +
          '30.528239 27.873756,23.401994 c -0.220051,-0.222825 -0.220051,-0' +
          '.581163 0,-0.803987 L 35.000002,15.471761 31.528241,12 24.401995' +
          ',19.126245 c -0.222825,0.220051 -0.581163,0.220051 -0.803987,0 z' +
          '"'#10'       id="path3256-2"'#10'       style="display:inline;fill:#ffff' +
          'ff;fill-opacity:1;fill-rule:evenodd;stroke:none;stroke-width:1;s' +
          'troke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4;st' +
          'roke-dasharray:none;stroke-opacity:1" /></g></svg>'#10
      end>
    Left = 730
    Top = 190
  end
  object pmMask: TPopupMenu
    AutoHotkeys = maManual
    OwnerDraw = True
    Left = 738
    Top = 141
  end
  object pmSettings: TPopupMenu
    Left = 780
    Top = 130
    object pmiLanguage: TMenuItem
      Caption = 'Desktop language'
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object pmiDesignHeader: TMenuItem
      Caption = 'Button design'
      object pmiDefaultDesign: TMenuItem
        Caption = 'Colored icons'
        Checked = True
        GroupIndex = 1
        RadioItem = True
        OnClick = pmiDefaultDesignClick
      end
      object pmiSimpleDesign: TMenuItem
        Caption = 'Simple icons'
        GroupIndex = 1
        RadioItem = True
        OnClick = pmiSimpleDesignClick
      end
    end
    object pmiDisplay: TMenuItem
      Caption = 'Display mode'
      object pmiDmDefault: TMenuItem
        Caption = 'Default'
        Checked = True
        GroupIndex = 1
        RadioItem = True
        OnClick = pmiDmDefaultClick
      end
      object pmiDmLight: TMenuItem
        Caption = 'Light'
        GroupIndex = 1
        RadioItem = True
        OnClick = pmiDmLightClick
      end
      object pmiDmDark: TMenuItem
        Caption = 'Dark'
        GroupIndex = 1
        RadioItem = True
        OnClick = pmiDmDarkClick
      end
    end
  end
  object imlStat: TSVGIconImageList
    SVGIconItems = <
      item
        IconName = 'b-right'
        SVGText = 
          '<svg height="48" viewBox="0 0 48 48" width="48" xmlns="http://ww' +
          'w.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink"><l' +
          'inearGradient id="a" gradientUnits="userSpaceOnUse" x1=".986122"' +
          ' x2="28.027756" y1="10" y2="10"><stop offset="0" stop-color="#99' +
          'bfec" stop-opacity=".99537"/><stop offset="1" stop-color="#2574c' +
          'a"/></linearGradient><path d="m.98612217 25.612494-.00000008-31.' +
          '2249884 27.04163391 15.6124944z" style="stroke:#2a6cbf;stroke-wi' +
          'dth:1.91213;stroke-linecap:round;stroke-linejoin:round;stroke-da' +
          'shoffset:1.33;fill:url(#a)" transform="matrix(.89553343045 0 0 -' +
          '1.20370239785 10.74356357793 36.03702373969)"/></svg>'
      end>
    Left = 690
    Top = 190
  end
end
