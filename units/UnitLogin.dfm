object LoginForm: TLoginForm
  Width = 640
  Height = 480
  CSSLibrary = cssBootstrap
  ElementFont = efCSS
  OnCreate = WebFormCreate
  OnKeyDown = WebFormKeyDown
  object labelLoginTitle: TWebLabel
    Left = 24
    Top = 160
    Width = 67
    Height = 13
    Caption = 'labelLoginTitle'
    ElementID = 'labelLoginTitle'
    ElementFont = efCSS
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
  end
  object editUsername: TWebEdit
    Left = 24
    Top = 16
    Width = 121
    Height = 22
    ElementID = 'editUsername'
    ElementFont = efCSS
    HeightPercent = 100.000000000000000000
    TextHint = 'Username'
    WidthPercent = 100.000000000000000000
    OnChange = editUsernameChange
  end
  object editPassword: TWebEdit
    Left = 24
    Top = 44
    Width = 121
    Height = 22
    ChildOrder = 1
    ElementID = 'editPassword'
    ElementFont = efCSS
    HeightPercent = 100.000000000000000000
    PasswordChar = '*'
    TextHint = 'Password'
    WidthPercent = 100.000000000000000000
    OnChange = editUsernameChange
  end
  object btnLogin: TWebButton
    Left = 24
    Top = 72
    Width = 96
    Height = 25
    Caption = 'Login'
    ChildOrder = 2
    ElementID = 'btnLogin'
    ElementFont = efCSS
    Enabled = False
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
    OnClick = btnLoginClick
  end
  object checkRemember: TWebCheckBox
    Left = 24
    Top = 179
    Width = 113
    Height = 22
    Caption = 'Remember Me'
    Checked = True
    ChildOrder = 4
    ElementID = 'flexCheckDefault'
    ElementFont = efCSS
    HeightStyle = ssAuto
    HeightPercent = 100.000000000000000000
    State = cbChecked
    WidthPercent = 100.000000000000000000
  end
  object divLoginBox: TWebHTMLDiv
    Left = 20
    Top = 207
    Width = 100
    Height = 41
    ElementClassName = 'login-box'
    ElementID = 'loginbox'
    ChildOrder = 5
    ElementFont = efCSS
    Role = ''
  end
  object btnForgot: TWebButton
    Left = 24
    Top = 103
    Width = 96
    Height = 25
    Caption = 'Forgot Password'
    ChildOrder = 2
    ElementID = 'btnForgot'
    ElementFont = efCSS
    Enabled = False
    HeightStyle = ssAuto
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
  end
  object tmrLoginStart: TWebTimer
    Enabled = False
    Interval = 10
    OnTimer = tmrLoginStartTimer
    Left = 40
    Top = 264
  end
end
