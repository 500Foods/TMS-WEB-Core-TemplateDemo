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
    Actions_Menu: String;
    Administrator_Menu: String;
    ArrowRight_Nav: String;
    Certificate: String;
    Dashboard_Menu: String;
    Dashboard_Nav: String;
    Database_Menu: String;
    Endpoints_Menu: String;
    FailedLogins_Menu: String;
    IPAllow_Menu: String;
    IPBlock_Menu: String;
    Labels_Menu: String;
    Login: String;
    Logins_Menu: String;
    Logout: String;
    Network_Menu: String;
    Password: String;
    People_Menu: String;
    Profile: String;
    Profile_Menu: String;
    Tokens_Menu: String;
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
  Actions            := '<span class="fa-solid fa-scroll me-2"></span>';
  Actions_Menu       := '<i class="fa-solid fa-scroll me-2 fa-fw"></i>';
  Administrator_Menu := '<i class="fa-solid fa-screwdriver-wrench fa-fw me-2"></i>';
  ArrowRight_Nav     := '<i class="nav-arrow fa-solid fa-angle-right"></i>';
  Certificate        := '<span class="fa-solid fa-certificate me-2"></span>';
  Dashboard_Menu     := '<i class="fa-solid fa-gauge-high me-2 fa-fw"></i>';
  Dashboard_Nav      := '<i class="nav-icon fa-solid fa-gauge-high me-2 fa-fw"></i>';
  Database_Menu      := '<i class="fa-solid fa-database fa-fw me-2"></i>';
  Endpoints_Menu     := '<i class="fa-solid fa-handshake fa-fw me-2"></i>';
  FailedLogins_Menu  := '<i class="fa-solid fa-thumbs-down fa-fw me-2"></i>';
  IPAllow_Menu       := '<i class="fa-solid fa-circle-check fa-fw me-2"></i>';
  IPBlock_Menu       := '<i class="fa-solid fa-circle-xmark fa-fw me-2"></i>';
  Labels_Menu        := '<i class="fa-solid fa-tags fa-fw me-2"></i>';
  Login              := '<span class="fa-solid fa-right-to-bracket me-2"></span>';
  Logins_Menu        := '<i class="fa-solid fa-thumbs-up fa-fw me-2"></i>';
  Logout             := '<span class="fa-solid fa-right-from-bracket me-2"></span>';
  Network_Menu       := '<i class="fa-solid fa-network-wired fa-fw me-2"></i>';
  Password           := '<span class="fa-solid fa-lock"></span>';
  People_Menu        := '<i class="fa-solid fa-people-group fa-fw me-2"></i>';
  Profile            := '<span class="fa-solid fa-user-secret me-2"></span>';
  Profile_Menu       := '<i class="fa-solid fa-user-secret fa-fw me-2"></i>';
  Tokens_Menu        := '<i class="fa-solidfa-shield fa-fw me-2"></i>';
  Username           := '<span class="fa-solid fa-envelope"></span>';

  // Override these icons if we're using FontAwesome6 Pro Duo
  if (Uppercase(IconSet) = 'DUOTONE') then
  begin
    Actions            := '<span class="fa-duotone fa-scroll me-2"></span>';
    Actions_Menu       := '<i class="fa-duotone fa-scroll me-2 fa-fw"></i>';
    Administrator_Menu := '<i class="fa-duotone fa-screwdriver-wrench fa-fw me-2"></i>';
    ArrowRight_Nav     := '<i class="nav-arrow fa-duotone fa-caret-right"></i>';
    Certificate        := '<span class="fa-duotone fa-certificate me-2"></span>';
    Dashboard_Menu     := '<i class="fa-duotone fa-chart-simple me-2 fa-fw"></i>';
    Dashboard_Nav      := '<i class="nav-icon fa-duotone fa-chart-simple me-2 fa-fw"></i>';
    Database_Menu      := '<i class="fa-duotone fa-database fa-fw me-2"></i>';
    Endpoints_Menu     := '<i class="fa-duotone fa-handshake fa-fw me-2"></i>';
    FailedLogins_Menu  := '<i class="fa-duotone fa-thumbs-down fa-fw me-2"></i>';
    IPAllow_Menu       := '<i class="fa-duotone fa-circle-check fa-fw me-2"></i>';
    IPBlock_Menu       := '<i class="fa-duotone fa-circle-xmark fa-fw me-2"></i>';
    Labels_Menu        := '<i class="fa-duotone fa-tags fa-fw me-2"></i>';
    Login              := '<span class="fa-duotone fa-right-to-bracket me-2"></span>';
    Logins_Menu        := '<i class="fa-duotone fa-thumbs-up fa-fw me-2"></i>';
    Logout             := '<span class="fa-duotone fa-right-from-bracket me-2"></span>';
    Network_Menu       := '<i class="fa-duotone fa-network-wired fa-fw me-2"></i>';
    Password           := '<span class="fa-duotone fa-lock fa-swap-opacity"></span>';
    People_Menu        := '<i class="fa-duotone fa-people-group fa-fw me-2"></i>';
    Profile            := '<span class="fa-duotone fa-user-secret me-2"></span>';
    Profile_Menu       := '<i class="fa-duotone fa-user-secret fa-fw me-2"></i>';
    Tokens_Menu        := '<i class="fa-duotone fa-shield fa-fw me-2"></i>';
    Username           := '<span class="fa-duotone fa-key fa-swap-opacity"></span>';
  end;

end;

end.
