object AdministratorForm: TAdministratorForm
  Width = 640
  Height = 480
  CSSLibrary = cssBootstrap
  ElementFont = efCSS
  OnCreate = WebFormCreate
  object labelCopyright: TWebLabel
    Left = 72
    Top = 40
    Width = 89
    Height = 13
    AutoSize = False
    Caption = 'Copyright'
    ElementID = 'labelCopyright'
    ElementFont = efCSS
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
  end
  object labelSlogan: TWebLabel
    Left = 72
    Top = 59
    Width = 89
    Height = 13
    AutoSize = False
    Caption = 'Slogan'
    ElementID = 'labelSlogan'
    ElementFont = efCSS
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
  end
  object labelName: TWebLabel
    Left = 72
    Top = 78
    Width = 89
    Height = 13
    Cursor = crHandPoint
    AutoSize = False
    Caption = 'Name'
    ElementID = 'labelName'
    ElementFont = efCSS
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
  end
  object spanPhoto: TWebLabel
    Left = 72
    Top = 97
    Width = 41
    Height = 16
    AutoSize = False
    Caption = 'Photo'
    ElementID = 'spanPhoto'
    ElementFont = efCSS
    HeightStyle = ssAuto
    HeightPercent = 100.000000000000000000
    HTMLType = tSPAN
    WidthPercent = 100.000000000000000000
  end
  object spanPhotoBig: TWebLabel
    Left = 72
    Top = 119
    Width = 41
    Height = 16
    AutoSize = False
    Caption = 'PhotoBig'
    ElementID = 'spanPhotoBig'
    ElementFont = efCSS
    HeightStyle = ssAuto
    HeightPercent = 100.000000000000000000
    HTMLType = tSPAN
    WidthPercent = 100.000000000000000000
  end
  object labelNameTitle: TWebLabel
    Left = 72
    Top = 141
    Width = 89
    Height = 13
    AutoSize = False
    Caption = 'Name+Title'
    ElementID = 'labelNameTitle'
    ElementFont = efCSS
    HeightStyle = ssAuto
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
  end
  object labelAppTitle: TWebLabel
    Left = 72
    Top = 160
    Width = 41
    Height = 16
    AutoSize = False
    Caption = 'AppTitle'
    ElementID = 'labelAppTitle'
    ElementFont = efCSS
    HeightStyle = ssAuto
    HeightPercent = 100.000000000000000000
    HTMLType = tSPAN
    WidthPercent = 100.000000000000000000
  end
  object labelLoggedIn: TWebLabel
    Left = 72
    Top = 182
    Width = 76
    Height = 13
    Caption = 'Logged In Since'
    ElementID = 'labelLoggedIn'
    ElementFont = efCSS
    HeightStyle = ssAuto
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
  end
  object btnLogout: TWebButton
    Left = 284
    Top = 97
    Width = 96
    Height = 25
    Caption = 'Logout'
    ChildOrder = 7
    ElementClassName = 'btn btn-danger w-100'
    ElementID = 'btnLogout'
    ElementFont = efCSS
    HeightStyle = ssAuto
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
    OnClick = btnLogoutClick
  end
  object btnActions: TWebButton
    Left = 284
    Top = 66
    Width = 96
    Height = 25
    Caption = 'Actions'
    ChildOrder = 7
    ElementClassName = 'btn btn-secondary w-100'
    ElementID = 'btnActions'
    ElementFont = efCSS
    HeightStyle = ssAuto
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
    OnClick = btnActionsClick
  end
  object btnProfile: TWebButton
    Left = 284
    Top = 35
    Width = 96
    Height = 25
    Caption = 'Profile'
    ChildOrder = 7
    ElementClassName = 'btn btn-primary w-100'
    ElementID = 'btnProfile'
    ElementFont = efCSS
    HeightStyle = ssAuto
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
    OnClick = btnProfileClick
  end
  object menuSidebar: TWebHTMLDiv
    Left = 280
    Top = 135
    Width = 100
    Height = 41
    ElementID = 'menuSidebar'
    ChildOrder = 12
    ElementFont = efCSS
    Role = ''
  end
  object divSubform: TWebHTMLDiv
    Left = 280
    Top = 182
    Width = 100
    Height = 40
    ElementID = 'divSubForm'
    HeightStyle = ssAuto
    ChildOrder = 13
    ElementPosition = epIgnore
    ElementFont = efCSS
    Role = ''
  end
  object btnForward: TWebButton
    Left = 284
    Top = 273
    Width = 96
    Height = 25
    Caption = 'Forward'
    ChildOrder = 7
    ElementClassName = 'btn btn-link nav-history-forward'
    ElementID = 'btnForward'
    ElementFont = efCSS
    HeightStyle = ssAuto
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
    OnClick = btnForwardClick
  end
  object btnBack: TWebButton
    Left = 284
    Top = 242
    Width = 96
    Height = 25
    Caption = 'Back'
    ChildOrder = 7
    ElementClassName = 'btn btn-link nav-history-back'
    ElementID = 'btnBack'
    ElementFont = efCSS
    HeightStyle = ssAuto
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
    OnClick = btnBackClick
  end
  object btnRecord: TWebButton
    Left = 404
    Top = 35
    Width = 96
    Height = 25
    Caption = 'Record Session'
    ChildOrder = 7
    ElementClassName = 'btn btn-warning w-100 text-nowrap'
    ElementID = 'btnRecord'
    ElementFont = efCSS
    HeightStyle = ssAuto
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
    OnClick = btnRecordClick
  end
  object btnPlayback: TWebButton
    Left = 404
    Top = 66
    Width = 96
    Height = 25
    Caption = 'Playback Session'
    ChildOrder = 7
    ElementClassName = 'btn btn-info w-100 text-nowrap'
    ElementID = 'btnPlayback'
    ElementFont = efCSS
    HeightStyle = ssAuto
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
    OnClick = btnPlaybackClick
  end
  object btnRecording: TWebButton
    Left = 404
    Top = 97
    Width = 96
    Height = 25
    Caption = 'Recording'
    ChildOrder = 7
    ElementClassName = 'btn btn-warning d-none'
    ElementID = 'btnRecording'
    ElementFont = efCSS
    HeightStyle = ssAuto
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
    OnClick = btnRecordClick
  end
  object btnChatBot: TWebButton
    Left = 284
    Top = 321
    Width = 96
    Height = 25
    Caption = 'ChatBot'
    ChildOrder = 7
    ElementClassName = 'btn btn-link'
    ElementID = 'btnChatBot'
    ElementFont = efCSS
    HeightStyle = ssAuto
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
    OnClick = btnChatBotClick
  end
end
