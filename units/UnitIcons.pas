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

  // Override these icons if we're using FontAwesome6 Pro Duo
  if (Uppercase(IconSet) = 'DUOTONE') then
  begin
    Username := '<span class="fa-duotone fa-key fa-swap-opacity"></span>';
    Password := '<span class="fa-duotone fa-lock fa-swap-opacity"></span>';
  end;

end;

end.
