object LoginForm: TLoginForm
  Width = 640
  Height = 480
  CSSLibrary = cssBootstrap
  ElementFont = efCSS
  OnCreate = WebFormCreate
  OnKeyDown = WebFormKeyDown
  object labelLoginTitle: TWebLabel
    Left = 24
    Top = 152
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
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
    OnClick = btnLoginClick
  end
end
