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
  object labelDashboard: TWebLabel
    Left = 72
    Top = 280
    Width = 82
    Height = 13
    Caption = 'Dashboard Name'
    ElementID = 'labelDashboard'
    ElementFont = efCSS
    HeightStyle = ssAuto
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
  end
  object btnLogout: TWebButton
    Left = 72
    Top = 244
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
    Left = 72
    Top = 213
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
    OnClick = btnLogoutClick
  end
  object btnProfile: TWebButton
    Left = 72
    Top = 182
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
    OnClick = btnLogoutClick
  end
  object tmrAdministratorStart: TWebTimer
    Enabled = False
    Interval = 10
    OnTimer = tmrAdministratorStartTimer
    Left = 568
    Top = 24
  end
end
