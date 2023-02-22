object AdministratorForm: TAdministratorForm
  Width = 640
  Height = 480
  OnCreate = WebFormCreate
  object tmrAdministratorStart: TWebTimer
    Enabled = False
    Interval = 10
    OnTimer = tmrAdministratorStartTimer
    Left = 96
    Top = 120
  end
end
