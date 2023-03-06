unit UnitUserActionsSub;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, Vcl.Controls, Vcl.StdCtrls, WEBLib.StdCtrls;

type
  TUserActionsSubForm = class(TWebForm)
    labelDashboard: TWebLabel;
    bcDashboards: TWebLabel;
    bcDashboard: TWebLabel;
    bcCurrent: TWebLabel;
    procedure WebFormShow(Sender: TObject);
    procedure WebFormCreate(Sender: TObject);
    procedure bcDashboardClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  UserActionsSubForm: TUserActionsSubForm;

implementation

uses UnitMain, UnitIcons;

{$R *.dfm}

procedure TUserActionsSubForm.bcDashboardClick(Sender: TObject);
begin
  asm
    pas.UnitMain.MainForm.CurrentForm.MenuClicked(pas.UnitMain.MainForm.CurrentFormName,'Dashboard','Sub')
  end;
end;

procedure TUserActionsSubForm.WebFormCreate(Sender: TObject);
begin
  labelDashboard.HTML := DMIcons.Icon('Actions_Menu')+'User Actions';
  bcDashboards.HTML := DMICons.Icon('Dashboard_Menu')+'Dashboards';
  bcDashboard.HTML := mainForm.CurrentFormIcon+MainForm.CurrentFormName;
  bcCurrent.hTML := DMIcons.Icon('Actions_Menu')+'User Actions';
end;

procedure TUserActionsSubForm.WebFormShow(Sender: TObject);
begin
  MainForm.SubFormShow;
end;

end.