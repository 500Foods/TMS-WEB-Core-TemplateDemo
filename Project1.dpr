program Project1;

{$R *.dres}

uses
  Vcl.Forms,
  WEBLib.Forms,
  UnitMain in 'units\UnitMain.pas' {MainForm: TWebForm} {*.html},
  UnitLogin in 'units\UnitLogin.pas' {LoginForm: TWebForm} {*.html},
  UnitAdministrator in 'units\Administrator\UnitAdministrator.pas' {AdministratorForm: TWebForm} {*.html},
  UnitIcons in 'units\UnitIcons.pas' {DMIcons: TWebDataModule},
  UnitMenus in 'units\UnitMenus.pas' {DMMenus: TWebDataModule},
  UnitUserProfileSub in 'units\User\UnitUserProfileSub.pas' {UserProfileSubForm: TWebForm} {*.html},
  UnitUserActionsSub in 'units\User\UnitUserActionsSub.pas' {UserActionsSubForm: TWebForm} {*.html},
  UnitAdministratorSub in 'units\Administrator\UnitAdministratorSub.pas' {AdministratorSubForm: TWebForm} {*.html};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TDMIcons, DMIcons);
  Application.CreateForm(TDMMenus, DMMenus);
  Application.Run;
end.
