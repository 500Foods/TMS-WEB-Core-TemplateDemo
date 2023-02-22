program Project1;

{$R *.dres}

uses
  Vcl.Forms,
  WEBLib.Forms,
  UnitMain in 'UnitMain.pas' {MainForm: TWebForm} {*.html},
  UnitLogin in 'UnitLogin.pas' {LoginForm: TWebForm} {*.html},
  UnitAdministrator in 'UnitAdministrator.pas' {AdministratorForm: TWebForm} {*.html};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
