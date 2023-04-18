unit UnitAdministrator;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, WEBLib.ExtCtrls, WEBLib.JSON, Vcl.Controls,
  Vcl.StdCtrls, WEBLib.StdCtrls, WEBLib.WebCtrls;

type
  TAdministratorForm = class(TWebForm)
    labelCopyright: TWebLabel;
    labelSlogan: TWebLabel;
    labelName: TWebLabel;
    spanPhoto: TWebLabel;
    spanPhotoBig: TWebLabel;
    labelNameTitle: TWebLabel;
    labelAppTitle: TWebLabel;
    btnLogout: TWebButton;
    btnActions: TWebButton;
    btnProfile: TWebButton;
    labelLoggedIn: TWebLabel;
    menuSidebar: TWebHTMLDiv;
    divSubform: TWebHTMLDiv;
    btnForward: TWebButton;
    btnBack: TWebButton;
    btnRecord: TWebButton;
    btnPlayback: TWebButton;
    btnRecording: TWebButton;
    btnChatBot: TWebButton;
    procedure btnLogoutClick(Sender: TObject);
    procedure btnProfileClick(Sender: TObject);
    procedure btnActionsClick(Sender: TObject);
    [async] procedure WebFormCreate(Sender: TObject);
    procedure CreateMenu;
    procedure btnBackClick(Sender: TObject);
    procedure btnForwardClick(Sender: TObject);
    procedure btnRecordClick(Sender: TObject);
    procedure btnPlaybackClick(Sender: TObject);
    procedure btnChatBotClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AdministratorForm: TAdministratorForm;

implementation

uses UnitMain, UnitIcons, UnitMenus;

{$R *.dfm}

procedure TAdministratorForm.btnActionsClick(Sender: TObject);
begin
  DMMenus.MenuClicked('Administrator', 'User', 'UserActionsSub', True);
end;

procedure TAdministratorForm.btnBackClick(Sender: TObject);
begin
  MainForm.NavHistoryBack;
end;

procedure TAdministratorForm.btnForwardClick(Sender: TObject);
begin
  MainForm.NavHistoryForward;
end;

procedure TAdministratorForm.btnLogoutClick(Sender: TObject);
begin
  MainForm.Logout('Button');
end;

procedure TAdministratorForm.btnPlaybackClick(Sender: TObject);
begin
  MainForm.PlaybackSession;
end;

procedure TAdministratorForm.btnProfileClick(Sender: TObject);
begin
  DMMenus.MenuClicked('Administrator', 'User', 'UserProfileSub', True);
end;

procedure TAdministratorForm.btnRecordClick(Sender: TObject);
begin
  MainForm.RecordSession;
end;

procedure TAdministratorForm.CreateMenu;
begin
  // Clear menu
  asm menuSidebar.replaceChildren(); end;

  // Add Available Dashboards
  DMMenus.AddDashboards(menuSidebar.ElementID, 'Administrator');

  // Administrator Database Menu
  DMMenus.AddMenuGroup(menuSidebar.ElementID, 'Database', 'Administrator');
  DMMenus.AddMenuItem('Database', 'Endpoints', 'Administrator');
  DMMenus.AddMenuItem('Database', 'Logins', 'Administrator');
  DMMenus.AddMenuItem('Database', 'Failed_Logins', 'Administrator');

  // Administrator Network Menu
  DMMenus.AddMenuGroup(menuSidebar.ElementID, 'Network', 'Administrator');
  DMMenus.AddMenuItem('Network', 'Tokens', 'Administrator');
  DMMenus.AddMenuItem('Network', 'IP_Allow', 'Administrator');
  DMMenus.AddMenuItem('Network', 'IP_Block', 'Administrator');

end;

procedure TAdministratorForm.btnChatBotClick(Sender: TObject);
begin
  DMMenus.MenuClicked('Administrator', 'Chat', 'ChatStatisticsSub', True);
end;

procedure TAdministratorForm.WebFormCreate(Sender: TObject);
var
  ResponseString: String;
  ResponseJSON: TJSONObject;

begin

  // Set linked element values that we already know
  labelLoggedIn.HTML := '<small>Logged in at '+FormatDateTime('hh:nn',Now)+'<small>';

  // Navbar Buttons
  btnChatBot.Caption := DMIcons.Icon('Robot');
  btnChatBot.ElementHandle.setAttribute('title','Chat with '+MainForm.App_ChatBotName);

  // User Menu Buttons
  btnProfile.Caption := DMIcons.Icon('Profile')+'Profile';
  btnActions.Caption := DMIcons.Icon('Actions')+'Actions';
  btnLogout.Caption  := DMIcons.Icon('Logout')+'Logout';

  btnRecord.Caption := DMIcons.Icon('Record')+'Start Recording';
  btnPlayback.Caption := DMIcons.Icon('Playback')+'Playback';


  btnBack.Caption := DMIcons.Icon('Back');
  btnBack.ElementHandle.setAttribute('disabled','');
  btnForward.Caption  := DMIcons.Icon('Forward');
  btnForward.ElementHandle.setAttribute('disabled','');

  CreateMenu();

  // Set our current state as the state we want to go back to
//  MainForm.Position := window.history.length;
//  MainForm.StartPosition := window.history.length;
//  window.history.pushState(MainForm.CaptureState, '', MainForm.URL);
//  MainForm.Position := MainForm.Position + 1;
//  MainForm.UpdateNav;

  asm {

    const SELECTOR_SIDEBAR_WRAPPER = '.sidebar-wrapper'
    const Default = {
      scrollbarTheme: 'os-theme-light',
      scrollbarAutoHide: 'leave'
    }
    if (typeof OverlayScrollbarsGlobal !== 'undefined') {
      if (typeof OverlayScrollbarsGlobal.OverlayScrollbars !== 'undefined') {
        OverlayScrollbarsGlobal.OverlayScrollbars(document.querySelector(SELECTOR_SIDEBAR_WRAPPER), {
          scrollbars: {
            theme: Default.scrollbarTheme,
            autoHide: Default.scrollbarAutoHide,
            clickScroll: true
          }
        })
      }
    }


    const storedTheme = localStorage.getItem('theme')
    const getPreferredTheme = () => {
      if (storedTheme) {
        return storedTheme
      }
      return window.matchMedia('(prefers-color-scheme: dark)').matches ? 'dark' : 'light'
    }
    const setTheme = function (theme) {
      if (theme === 'auto' && window.matchMedia('(prefers-color-scheme: dark)').matches) {
        document.documentElement.setAttribute('data-bs-theme', 'dark')
      } else {
        document.documentElement.setAttribute('data-bs-theme', theme)
      }
    }
    setTheme(getPreferredTheme());
    const showActiveTheme = theme => {
      const activeThemeIcon = document.querySelector('.theme-icon-active i, .theme-icon-active svg')
      var btnToActive = document.querySelector(`[data-bs-theme-value="${theme}"]`)
      if (btnToActive == undefined) {
       theme='dark';
       btnToActive = document.querySelector(`[data-bs-theme-value="${theme}"]`)
      }
      const svgOfActiveBtn = btnToActive.querySelector('i,svg').getAttribute('class')
      document.querySelectorAll('[data-bs-theme-value]').forEach(element => {
        element.classList.remove('active')
      })
      btnToActive.classList.add('active')
      activeThemeIcon.setAttribute('class', svgOfActiveBtn)
    }


    window.matchMedia('(prefers-color-scheme: dark)').addEventListener('change', () => {
      if (storedTheme !== 'light' || storedTheme !== 'dark') {
        setTheme(getPreferredTheme())
      }
    });


    showActiveTheme(getPreferredTheme());
    document.querySelectorAll('[data-bs-theme-value]')
      .forEach(toggle => {
        toggle.addEventListener('click', () => {
          const theme = toggle.getAttribute('data-bs-theme-value')
          localStorage.setItem('theme', theme)
          setTheme(theme)
          showActiveTheme(theme)
        })
      });


  } end;

  ResponseString := await(MainForm.JSONRequest('IDashboardService.AdministratorDashboard',[]));
  if ResponseString <> '' then
  begin
    ResponseJSON := TJSONObject.ParseJSONValue(ResponseString) as TJSONObject;

    labelName.HTML := MainForm.User_FirstName+' '+MainForm.User_LastName;
    labelNameTitle.HTML := MainForm.User_FirstName+' '+MainForm.User_LastName+' - Administrator';
    labelCopyright.HTML := (((ResponseJSON.GetValue('Organization') as TJSONArray).Items[3] as TJSONObject).GetValue('value') as TJSONString).Value;
    labelSlogan.HTML := (((ResponseJSON.GetValue('Organization') as TJSONArray).Items[2] as TJSONObject).GetValue('value') as TJSONString).Value;
    labelAppTitle.HTML := (((ResponseJSON.GetValue('Organization') as TJSONArray).Items[1] as TJSONObject).GetValue('value') as TJSONString).Value;

    MainForm.User_Photo := (ResponseJSON.GetValue('Photo') as TJSONString).Value;
    spanPhoto.HTML := MainForm.User_Photo;
    spanPhoto.ElementHandle.firstElementChild.className := 'user-image rounded-circle shadow';
    spanPhoto.ElementHandle.firstElementChild.setAttribute('alt','User Photo');

    spanPhotoBig.HTML := (ResponseJSON.GetValue('Photo') as TJSONString).Value;
    spanPhotoBig.ElementHandle.firstElementChild.className := 'rounded-circle shadow';
    spanPhotoBig.ElementHandle.firstElementChild.setAttribute('alt','User Photo');
    (spanPhotoBig.ElementHandle.firstElementChild as TJSHTMLElement).style.setProperty('max-width','200px');
  end;

  // Show the form
  DMMenus.MenuClicked('Administrator', 'Dashboard', 'AdministratorSub', False);
  MainForm.divHost.ElementHandle.style.setProperty('opacity','1');
  divSubForm.ElementHandle.style.setProperty('opacity','1');
end;

end.