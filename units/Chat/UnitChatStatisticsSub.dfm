object ChatStatisticsSubForm: TChatStatisticsSubForm
  Width = 640
  Height = 480
  OnCreate = WebFormCreate
  OnResize = WebFormResize
  object btnChatSend: TWebButton
    Left = 256
    Top = 46
    Width = 96
    Height = 25
    Caption = 'Send'
    Default = True
    ElementClassName = 'btn btn-warning'
    ElementID = 'btnChatSend'
    ElementFont = efCSS
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
    OnClick = btnChatSendClick
  end
  object memoChat: TWebMemo
    Left = 40
    Top = 48
    Width = 185
    Height = 38
    AutoSize = False
    ElementClassName = 
      'fs-6 flex-fill px-2 me-2 border border-1 border-secondary rounde' +
      'd'
    ElementID = 'memoChat'
    ElementFont = efCSS
    ElementPosition = epIgnore
    HeightStyle = ssAuto
    HeightPercent = 100.000000000000000000
    SelLength = 0
    SelStart = 0
    TextHint = 'Enter Message....'
    WantTabs = True
    WidthStyle = ssPercent
    WidthPercent = 100.000000000000000000
    OnKeyDown = memoChatKeyDown
  end
  object WebTimer1: TWebTimer
    Enabled = False
    OnTimer = WebTimer1Timer
    Left = 312
    Top = 160
  end
end
