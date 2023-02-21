object LoginForm: TLoginForm
  Width = 640
  Height = 480
  object editUsername: TWebEdit
    Left = 24
    Top = 16
    Width = 121
    Height = 22
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
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
  end
end
