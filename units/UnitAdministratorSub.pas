unit UnitAdministratorSub;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, Vcl.Controls, Vcl.StdCtrls, WEBLib.StdCtrls;

type
  TAdministratorSubForm = class(TWebForm)
    bcDashboard: TWebLabel;
    bcDashboards: TWebLabel;
    labelDashboard: TWebLabel;
    procedure WebFormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AdministratorSubForm: TAdministratorSubForm;

implementation

uses
  UnitMain, UnitIcons;

{$R *.dfm}

procedure TAdministratorSubForm.WebFormCreate(Sender: TObject);
begin
  labelDashboard.HTML := DMIcons.Icon('Administrator_Menu')+'Administrator Dashboard';
  bcDashboards.HTML := DMICons.Icon('Dashboard_Menu')+'Dashboards';
  bcDashboard.HTML := DMIcons.Icon('Administrator_Menu')+'Administrator';

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

  } end;



  asm
//    menuSidebar.replaceWith(menuSidebar.cloneNode(true));
//    pas.UnitMain.MainForm.CurrentForm.CreateMenu();
    window.document.dispatchEvent(new Event("DOMContentLoaded", {
      bubbles: true,
      cancelable: true
    }));
  end;

  (document.getElementById('divSubForm') as TJSHTMLElement).style.setProperty('opacity', '1','important');
  MainForm.LogAction('', False);
end;

end.
