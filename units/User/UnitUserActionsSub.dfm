object UserActionsSubForm: TUserActionsSubForm
  Width = 640
  Height = 480
  CSSLibrary = cssBootstrap
  ElementFont = efCSS
  OnCreate = WebFormCreate
  object actionsTitle: TWebLabel
    Left = 71
    Top = 353
    Width = 54
    Height = 13
    Caption = 'actionsTitle'
    ElementID = 'actionsTitle'
    ElementFont = efCSS
    HeightStyle = ssAuto
    HeightPercent = 100.000000000000000000
    HTMLType = tSPAN
    WidthPercent = 100.000000000000000000
  end
  object actionsHistory: TWebLabel
    Left = 71
    Top = 372
    Width = 68
    Height = 13
    Caption = 'actionsHistory'
    ElementID = 'actionsHistory'
    ElementFont = efCSS
    HeightStyle = ssAuto
    HeightPercent = 100.000000000000000000
    HTMLType = tSPAN
    WidthPercent = 100.000000000000000000
  end
end
