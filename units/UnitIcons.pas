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
    Username: String;
    Password: String;
    Logout: String;
    Profile: String;
    Actions: String;
  end;

var
  DMIcons: TDMIcons;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

procedure TDMIcons.InitializeIcons(IconSet: String);
begin

  // Set default as FontAwesome 6 Free Icons
  Username := '<span class="fa-solid fa-envelope"></span>';
  Password := '<span class="fa-solid fa-lock"></span>';
  Logout   := '<span class="fa-solid fa-right-from-bracket me-2"></span>';
  Profile  := '<span class="fa-solid fa-user-secret me-2"></span>';
  Actions  := '<span class="fa-solid fa-scroll me-2"></span>';

  // Override these icons if we're using FontAwesome6 Pro Duo
  if (Uppercase(IconSet) = 'DUOTONE') then
  begin
    Username := '<span class="fa-duotone fa-key fa-swap-opacity"></span>';
    Password := '<span class="fa-duotone fa-lock fa-swap-opacity"></span>';
    Logout   := '<span class="fa-duotone fa-right-from-bracket me-2"></span>';
    Profile  := '<span class="fa-duotone fa-user-secret me-2"></span>';
    Actions  := '<span class="fa-duotone fa-scroll me-2"></span>';
  end;

end;

end.
