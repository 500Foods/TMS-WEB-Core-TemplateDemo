program Project1;

{$R *.dres}

uses
  Vcl.Forms,
  WEBLib.Forms,
  UnitMain in 'UnitMain.pas' {MainForm: TWebForm} {*.html},
  UnitLogin in 'UnitLogin.pas' {LoginForm: TWebForm} {*.html},
  UnitAdministrator in 'UnitAdministrator.pas' {AdministratorForm: TWebForm} {*.html},
  UnitIcons in 'UnitIcons.pas' {DMIcons: TWebDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TDMIcons, DMIcons);
  Application.Run;
end.
