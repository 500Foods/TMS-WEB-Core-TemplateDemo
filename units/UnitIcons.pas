unit UnitIcons;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Modules;

type
  TDMIcons = class(TWebDataModule)
    procedure InitializeIcons(IconSet: String);
  private
    { Private declarations }
  public
    { Public declarations }
    Actions: String;
    Certificate: String;
    Login: String;
    Logout: String;
    Password: String;
    Profile: String;
    Username: String;
  end;

var
  DMIcons: TDMIcons;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

procedure TDMIcons.InitializeIcons(IconSet: String);
begin

  // Set default as FontAwesome 6 Free Icons
  Actions       := '<span class="fa-solid fa-scroll me-2"></span>';
  Certificate   := '<span class="fa-solid fa-certificate me-2"></span>';
  Login         := '<span class="fa-solid fa-right-to-bracket me-2"></span>';
  Logout        := '<span class="fa-solid fa-right-from-bracket me-2"></span>';
  Password      := '<span class="fa-solid fa-lock"></span>';
  Profile       := '<span class="fa-solid fa-user-secret me-2"></span>';
  Username      := '<span class="fa-solid fa-envelope"></span>';

  // Override these icons if we're using FontAwesome6 Pro Duo
  if (Uppercase(IconSet) = 'DUOTONE') then
  begin
    Actions       := '<span class="fa-duotone fa-scroll me-2"></span>';
    Certificate   := '<span class="fa-duotone fa-certificate me-2"></span>';
    Login         := '<span class="fa-duotone fa-right-to-bracket me-2"></span>';
    Logout        := '<span class="fa-duotone fa-right-from-bracket me-2"></span>';
    Password      := '<span class="fa-duotone fa-lock fa-swap-opacity"></span>';
    Profile       := '<span class="fa-duotone fa-user-secret me-2"></span>';
    Username      := '<span class="fa-duotone fa-key fa-swap-opacity"></span>';
  end;

end;

end.
