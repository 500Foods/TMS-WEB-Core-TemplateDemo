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
  object btnBack: TWebButton
    Left = 296
    Top = 264
    Width = 96
    Height = 25
    Caption = 'Back'
    ChildOrder = 13
    ElementClassName = 'btn btn-link nav-history-back'
    ElementID = 'btnBack'
    ElementFont = efCSS
    HeightStyle = ssAuto
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
    OnClick = btnBackClick
  end
  object btnForward: TWebButton
    Left = 296
    Top = 295
    Width = 96
    Height = 25
    Caption = 'Forward'
    ChildOrder = 13
    ElementClassName = 'btn btn-link nav-history-forward'
    ElementID = 'btnForward'
    ElementFont = efCSS
    HeightStyle = ssAuto
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
    OnClick = btnForwardClick
  end
end
