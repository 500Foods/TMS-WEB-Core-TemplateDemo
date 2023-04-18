object MainForm: TMainForm
  Width = 1228
  Height = 843
  Caption = '3'
  Color = clSilver
  CSSLibrary = cssBootstrap
  ElementFont = efCSS
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'Tahoma'
  Font.Style = []
  ParentFont = False
  OnClick = WebFormClick
  OnCreate = WebFormCreate
  OnHashChange = WebFormHashChange
  OnKeyDown = WebFormKeyDown
  object divHost: TWebHTMLDiv
    Left = 135
    Top = 8
    Width = 1085
    Height = 827
    ElementClassName = 'rounded border bg-white border-dark'
    ElementID = 'divHost'
    HeightStyle = ssAuto
    ElementFont = efCSS
    Role = ''
  end
  object btnShowLog: TWebButton
    Left = 8
    Top = 8
    Width = 121
    Height = 33
    Caption = 'Show Log'
    ChildOrder = 1
    ElementClassName = 'btn btn-light no-print'
    ElementID = 'btnShowLog'
    ElementFont = efCSS
    HeightStyle = ssAuto
    HeightPercent = 100.000000000000000000
    Visible = False
    WidthPercent = 100.000000000000000000
    OnClick = btnShowLogClick
  end
  object divLog: TWebHTMLDiv
    Left = 8
    Top = 423
    Width = 121
    Height = 34
    ElementClassName = 'p-1 rounded border bg-light border-dark overflow-auto no-print'
    ElementID = 'divLog'
    ChildOrder = 2
    ElementFont = efCSS
    Role = ''
    Visible = False
  end
  object btnLoginForm: TWebButton
    Left = 8
    Top = 86
    Width = 121
    Height = 33
    Caption = 'Login Form'
    ChildOrder = 1
    ElementClassName = 'btn btn-light no-print'
    ElementID = 'btnLoginForm'
    ElementFont = efCSS
    HeightStyle = ssAuto
    HeightPercent = 100.000000000000000000
    Visible = False
    WidthPercent = 100.000000000000000000
    OnClick = btnLoginFormClick
  end
  object btnClearForm: TWebButton
    Left = 8
    Top = 47
    Width = 121
    Height = 33
    Caption = 'Clear Form'
    ChildOrder = 1
    ElementClassName = 'btn btn-light no-print'
    ElementID = 'btnClearForm'
    ElementFont = efCSS
    HeightStyle = ssAuto
    HeightPercent = 100.000000000000000000
    Visible = False
    WidthPercent = 100.000000000000000000
    OnClick = btnClearFormClick
  end
  object divToasts: TWebHTMLDiv
    Left = 16
    Top = 184
    Width = 100
    Height = 41
    ElementClassName = 'no-print toast-container'
    ElementID = 'divToasts'
    ChildOrder = 5
    ElementFont = efCSS
    HTML.Strings = (
      '<div class="toast-container">'
      '</div>')
    Role = ''
  end
  object divViewer: TWebHTMLDiv
    Left = 8
    Top = 463
    Width = 121
    Height = 41
    ElementID = 'divViewer'
    ChildOrder = 6
    ElementPosition = epIgnore
    ElementFont = efCSS
    Role = ''
    Visible = False
  end
  object btnViewerClose: TWebButton
    Left = 50
    Top = 510
    Width = 40
    Height = 50
    ChildOrder = 7
    ElementClassName = 'btn btn-link text-decoration-none'
    ElementID = 'btnViewerClose'
    ElementFont = efCSS
    ElementPosition = epIgnore
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
    OnClick = btnViewerCloseClick
  end
  object divViewerImage: TWebHTMLDiv
    Left = 8
    Top = 566
    Width = 121
    Height = 41
    ElementID = 'divViewerImage'
    ChildOrder = 6
    ElementPosition = epIgnore
    ElementFont = efCSS
    Role = ''
    Visible = False
    OnClick = divViewerImageClick
  end
  object XDataConn: TXDataWebConnection
    OnRequest = XDataConnRequest
    Left = 48
    Top = 128
  end
  object tmrJWTRenewal: TWebTimer
    Enabled = False
    OnTimer = tmrJWTRenewalTimer
    Left = 48
    Top = 232
  end
  object tmrJWTRenewalWarning: TWebTimer
    Enabled = False
    OnTimer = tmrJWTRenewalWarningTimer
    Left = 48
    Top = 280
  end
  object WebHttpRequest1: TWebHttpRequest
    Left = 48
    Top = 328
  end
  object tmrCapture: TWebTimer
    Enabled = False
    OnTimer = tmrCaptureTimer
    Left = 48
    Top = 376
  end
end
