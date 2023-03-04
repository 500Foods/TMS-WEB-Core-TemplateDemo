unit UnitAdministrator;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, WEBLib.ExtCtrls, WEBLib.JSON, Vcl.Controls,
  Vcl.StdCtrls, WEBLib.StdCtrls, WEBLib.WebCtrls;

type
  TAdministratorForm = class(TWebForm)
    tmrAdministratorStart: TWebTimer;
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
    labelDashboard: TWebLabel;
    procedure WebFormCreate(Sender: TObject);
    [async] procedure tmrAdministratorStartTimer(Sender: TObject);
    procedure btnLogoutClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AdministratorForm: TAdministratorForm;

implementation

uses UnitMain, UnitIcons;

{$R *.dfm}

procedure TAdministratorForm.btnLogoutClick(Sender: TObject);
begin
  MainForm.Logout('Button');
end;

procedure TAdministratorForm.tmrAdministratorStartTimer(Sender: TObject);
var
  ResponseString: String;
  ResponseJSON: TJSONObject;
begin
  tmrAdministratorStart.Enabled := False;
  ResponseString := await(MainForm.JSONRequest('IDashboardService.AdministratorDashboard',[]));
  if ResponseString <> '' then
  begin
    ResponseJSON := TJSONObject.ParseJSONValue(ResponseString) as TJSONObject;

    labelName.HTML := MainForm.User_FirstName+' '+MainForm.User_LastName;
    labelNameTitle.HTML := MainForm.User_FirstName+' '+MainForm.User_LastName+' - Administrator';
    labelCopyright.HTML := (((ResponseJSON.GetValue('Organization') as TJSONArray).Items[3] as TJSONObject).GetValue('value') as TJSONString).Value;
    labelSlogan.HTML := (((ResponseJSON.GetValue('Organization') as TJSONArray).Items[2] as TJSONObject).GetValue('value') as TJSONString).Value;
    labelAppTitle.HTML := (((ResponseJSON.GetValue('Organization') as TJSONArray).Items[1] as TJSONObject).GetValue('value') as TJSONString).Value;

    spanPhoto.HTML := (ResponseJSON.GetValue('Photo') as TJSONString).Value;
    spanPhoto.ElementHandle.firstElementChild.className := 'user-image rounded-circle shadow';
    spanPhoto.ElementHandle.firstElementChild.setAttribute('alt','User Photo');

    spanPhotoBig.HTML := (ResponseJSON.GetValue('Photo') as TJSONString).Value;
    spanPhotoBig.ElementHandle.firstElementChild.className := 'rounded-circle shadow';
    spanPhotoBig.ElementHandle.firstElementChild.setAttribute('alt','User Photo');
    (spanPhotoBig.ElementHandle.firstElementChild as TJSHTMLElement).style.setProperty('max-width','200px');

//    asm
//      console.log(JSON.parse(ResponseString));
//    end;
  end;
end;

procedure TAdministratorForm.WebFormCreate(Sender: TObject);
begin

  labelCopyright.Caption := '';
  labelSlogan.Caption := '';
  labelName.Caption := '';
  labelNameTitle.Caption := '';
  spanPhoto.HTML := '';
  spanPhotoBig.HTML := '';
  labelAppTitle.HTML := '';

  labelDashboard.Caption := 'Administrator Dashboard';

  btnProfile.Caption := DMIcons.Profile+'Profile';
  btnActions.Caption := DMIcons.Actions+'Actions';
  btnLogout.Caption  := DMIcons.Logout+'Logout';


  asm {

    var salesChartCanvas = document.querySelector('#salesChart').getContext('2d');
    var salesChartData = {
      labels: ['January', 'February', 'March', 'April', 'May', 'June', 'July'],
      datasets: [
        {
          label: 'Digital Goods',
          backgroundColor: 'rgba(60,141,188,0.9)',
          borderColor: 'rgba(60,141,188,0.8)',
          fill: true,
          pointRadius: 0,
          pointColor: '#3b8bba',
          pointStrokeColor: 'rgba(60,141,188,1)',
          pointHighlightFill: '#fff',
          pointHighlightStroke: 'rgba(60,141,188,1)',
          data: [28, 48, 40, 19, 86, 27, 90]
        },
        {
          label: 'Electronics',
          backgroundColor: 'rgba(210, 214, 222, 1)',
          borderColor: 'rgba(210, 214, 222, 1)',
          fill: true,
          pointRadius: 0,
          pointColor: 'rgba(210, 214, 222, 1)',
          pointStrokeColor: '#c1c7d1',
          pointHighlightFill: '#fff',
          pointHighlightStroke: 'rgba(220,220,220,1)',
          data: [65, 59, 80, 81, 56, 55, 40]
        }
      ]
    }
    var salesChartOptions = {
      maintainAspectRatio: false,
      responsive: true,
      tension: 0.4,
      plugins: {
        legend: {
          display: false
        }
      },
      scales: {
        xAxes: {
          gridLines: {
            display: false
          }
        },
        yAxes: {
          gridLines: {
            display: false
          }
        }
      }
    }
    // This will get the first returned node in the js collection.
    var salesChart = new Chart(salesChartCanvas, {
      type: 'line',
      data: salesChartData,
      options: salesChartOptions
    });


    var pieChartCanvas = document.querySelector('#pieChart').getContext('2d');
    var pieData = {
      labels: [
        'Chrome',
        'IE',
        'FireFox',
        'Safari',
        'Opera',
        'Navigator'
      ],
      datasets: [
        {
          data: [700, 500, 400, 600, 300, 100],
          backgroundColor: ['#f56954', '#00a65a', '#f39c12', '#00c0ef', '#3c8dbc', '#d2d6de']
        }
      ]
    }
    var pieOptions = {
      plugins: {
        legend: {
          display: false
        }
      }
    }
    // Create pie or douhnut chart
    // You can switch between pie and douhnut using the method below.
    // eslint-disable-next-line no-unused-vars
    var pieChart = new Chart(pieChartCanvas, {
      type: 'doughnut',
      data: pieData,
      options: pieOptions
    });


    window.document.dispatchEvent(new Event("DOMContentLoaded", {
      bubbles: true,
      cancelable: true
    }));


    const SELECTOR_SIDEBAR_WRAPPER = '.sidebar-wrapper'
    const Default = {
      scrollbarTheme: 'os-theme-light',
      scrollbarAutoHide: 'leave'
    }
    if (typeof OverlayScrollbarsGlobal !== 'undefined') {
      if (typeof OverlayScrollbarsGlobal?.OverlayScrollbars !== 'undefined') {
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
      const activeThemeIcon = document.querySelector('.theme-icon-active i')
      const btnToActive = document.querySelector(`[data-bs-theme-value="${theme}"]`)
      const svgOfActiveBtn = btnToActive.querySelector('i').getAttribute('class')
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

  tmrAdministratorStart.Enabled := True;

end;

end.