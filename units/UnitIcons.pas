unit UnitIcons;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Modules, jsdelphisystem;

type
  TDMIcons = class(TWebDataModule)
    procedure InitializeIcons(IconSet: String);
    function Icon(LookupIcon: String):String;
  private
    { Private declarations }
  public
    { Public declarations }
    Lookup: JSValue;
  end;

var
  DMIcons: TDMIcons;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

function TDMIcons.Icon(LookupIcon: String):String;
begin
  asm
    Result = this.Lookup[LookupIcon];
  end;
end;

procedure TDMIcons.InitializeIcons(IconSet: String);
begin

  asm
    var Icon = [];

    // Set default as FontAwesome 6 Free Icons
    Icon[ "Actions"             ] = '<span class="fa-solid fa-scroll me-2"></span>';
    Icon[ "Actions_Menu"        ] = '<i class="fa-solid fa-scroll me-2 fa-fw"></i>';
    Icon[ "AddressBook"         ] = '<i class="fa-solid fa-address-book fa-fw"></i>';
    Icon[ "Administrator_Menu"  ] = '<i class="fa-solid fa-screwdriver-wrench fa-fw me-2"></i>';
    Icon[ "Anniversary"         ] = '<i class="fa-solid fa-champagne-glasses"></i>';
    Icon[ "ArrowRight_Nav"      ] = '<i class="nav-arrow fa-solid fa-angle-right"></i>';
    Icon[ "Birthday"            ] = '<i class="fa-solid fa-cake-candles"></i>';
    Icon[ "Certificate"         ] = '<span class="fa-solid fa-certificate me-2"></span>';
    Icon[ "Dashboard_Menu"      ] = '<i class="fa-solid fa-gauge-high me-2 fa-fw"></i>';
    Icon[ "Dashboard_Nav"       ] = '<i class="nav-icon fa-solid fa-gauge-high me-2 fa-fw"></i>';
    Icon[ "Database_Menu"       ] = '<i class="fa-solid fa-database fa-fw me-2"></i>';
    Icon[ "EMail"               ] = '<i class="fa-solid fa-envelope fa-fw"></i>';
    Icon[ "Endpoints_Menu"      ] = '<i class="fa-solid fa-handshake fa-fw me-2"></i>';
    Icon[ "FailedLogins_Menu"   ] = '<i class="fa-solid fa-thumbs-down fa-fw me-2"></i>';
    Icon[ "IPAllow_Menu"        ] = '<i class="fa-solid fa-circle-check fa-fw me-2"></i>';
    Icon[ "IPBlock_Menu"        ] = '<i class="fa-solid fa-circle-xmark fa-fw me-2"></i>';
    Icon[ "Labels_Menu"         ] = '<i class="fa-solid fa-tags fa-fw"></i>';
    Icon[ "Login"               ] = '<span class="fa-solid fa-right-to-bracket me-2"></span>';
    Icon[ "Logins_Menu"         ] = '<i class="fa-solid fa-thumbs-up fa-fw me-2"></i>';
    Icon[ "Logout"              ] = '<span class="fa-solid fa-right-from-bracket me-2"></span>';
    Icon[ "Network_Menu"        ] = '<i class="fa-solid fa-network-wired fa-fw me-2"></i>';
    Icon[ "Password"            ] = '<span class="fa-solid fa-lock"></span>';
    Icon[ "People_Menu"         ] = '<i class="fa-solid fa-people-group fa-fw me-2"></i>';
    Icon[ "Photo"               ] = '<i class="fa-solid fa-portrait"></i>';
    Icon[ "Profile"             ] = '<span class="fa-solid fa-user-secret me-2"></span>';
    Icon[ "Profile_Menu"        ] = '<i class="fa-solid fa-user-secret fa-fw me-2"></i>';
    Icon[ "Telephone"           ] = '<i class="fa-solid fa-phone fa-fw"></i>';
    Icon[ "TelephoneHome"       ] = '<i class="fa-solid fa-phone fa-fw"></i>';
    Icon[ "TelephoneMobile"     ] = '<i class="fa-solid fa-mobile fa-fw"></i>';
    Icon[ "Tokens_Menu"         ] = '<i class="fa-solid fa-shield fa-fw me-2"></i>';
    Icon[ "Username"            ] = '<span class="fa-solid fa-envelope"></span>';

    // Override these icons if we're using FontAwesome 6 Pro Duo
    if (IconSet.toUpperCase() == 'DUOTONE') {
      Icon[ "Actions"             ] = '<span class="fa-duotone fa-scroll me-2"></span>';
      Icon[ "Actions_Menu"        ] = '<i class="fa-duotone fa-scroll me-2 fa-fw"></i>';
      Icon[ "AddressBook"         ] = '<i class="fa-duotone fa-address-book fa-fw"></i>';
      Icon[ "Administrator_Menu"  ] = '<i class="fa-duotone fa-screwdriver-wrench fa-fw me-2"></i>';
      Icon[ "Anniversary"         ] = '<i class="fa-duotone fa-champagne-glasses"></i>';
      Icon[ "ArrowRight_Nav"      ] = '<i class="nav-arrow fa-duotone fa-caret-right"></i>';
      Icon[ "Birthday"            ] = '<i class="fa-duotone fa-cake-candles"></i>';
      Icon[ "Certificate"         ] = '<span class="fa-duotone fa-certificate me-2"></span>';
      Icon[ "Dashboard_Menu"      ] = '<i class="fa-duotone fa-chart-simple me-2 fa-fw"></i>';
      Icon[ "Dashboard_Nav"       ] = '<i class="nav-icon fa-duotone fa-chart-simple me-2 fa-fw"></i>';
      Icon[ "Database_Menu"       ] = '<i class="fa-duotone fa-database fa-fw me-2"></i>';
      Icon[ "EMail"               ] = '<i class="fa-duotone fa-envelope fa-swap-opacity fa-fw"></i>';
      Icon[ "Endpoints_Menu"      ] = '<i class="fa-duotone fa-handshake fa-fw me-2"></i>';
      Icon[ "FailedLogins_Menu"   ] = '<i class="fa-duotone fa-thumbs-down fa-fw me-2"></i>';
      Icon[ "IPAllow_Menu"        ] = '<i class="fa-duotone fa-circle-check fa-fw me-2"></i>';
      Icon[ "IPBlock_Menu"        ] = '<i class="fa-duotone fa-circle-xmark fa-fw me-2"></i>';
      Icon[ "Labels_Menu"         ] = '<i class="fa-duotone fa-tags fa-fw me-2"></i>';
      Icon[ "Login"               ] = '<span class="fa-duotone fa-right-to-bracket fa-fw"></span>';
      Icon[ "Logins_Menu"         ] = '<i class="fa-duotone fa-thumbs-up fa-fw me-2"></i>';
      Icon[ "Logout"              ] = '<span class="fa-duotone fa-right-from-bracket me-2"></span>';
      Icon[ "Network_Menu"        ] = '<i class="fa-duotone fa-network-wired fa-fw me-2"></i>';
      Icon[ "Password"            ] = '<span class="fa-duotone fa-lock fa-swap-opacity"></span>';
      Icon[ "People_Menu"         ] = '<i class="fa-duotone fa-people-group fa-fw me-2"></i>';
      Icon[ "Photo"               ] = '<i class="fa-duotone fa-portrait"></i>';
      Icon[ "Profile"             ] = '<span class="fa-duotone fa-user-secret me-2"></span>';
      Icon[ "Profile_Menu"        ] = '<i class="fa-duotone fa-user-secret fa-fw me-2"></i>';
      Icon[ "Telephone"           ] = '<i class="fa-duotone fa-phone-office fa-fw"></i>';
      Icon[ "TelephoneHome"       ] = '<i class="fa-duotone fa-phone-rotary fa-fw"></i>';
      Icon[ "TelephoneMobile"     ] = '<i class="fa-duotone fa-mobile fa-fw"></i>';
      Icon[ "Tokens_Menu"         ] = '<i class="fa-duotone fa-shield fa-fw me-2"></i>';
      Icon[ "Username"            ] = '<span class="fa-duotone fa-key fa-swap-opacity"></span>';
    }


    this.Lookup = Icon;
  end;


end;

end.
