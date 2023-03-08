object UserActionsSubForm: TUserActionsSubForm
  Width = 640
  Height = 480
  CSSLibrary = cssBootstrap
  ElementFont = efCSS
  OnCreate = WebFormCreate
  object labelDashboard: TWebLabel
    Left = 72
    Top = 232
    Width = 82
    Height = 13
    Caption = 'Dashboard Name'
    ElementID = 'labelDashboard'
    ElementFont = efCSS
    HeightStyle = ssAuto
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
  end
  object bcDashboards: TWebLabel
    Left = 72
    Top = 251
    Width = 117
    Height = 13
    Caption = 'Dashboards Breadcrumb'
    ElementID = 'bcDashboards'
    ElementFont = efCSS
    HeightStyle = ssAuto
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
  end
  object bcDashboard: TWebLabel
    Left = 72
    Top = 270
    Width = 112
    Height = 13
    Cursor = crHandPoint
    Caption = 'Dashboard Breadcrumb'
    ElementID = 'bcDashboard'
    ElementFont = efCSS
    HeightStyle = ssAuto
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
    OnClick = bcDashboardClick
  end
  object bcCurrent: TWebLabel
    Left = 72
    Top = 289
    Width = 97
    Height = 13
    Caption = 'Current Breadcrumb'
    ElementID = 'bcCurrent'
    ElementFont = efCSS
    HeightStyle = ssAuto
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
  end
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