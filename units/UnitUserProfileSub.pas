unit UnitUserProfileSub;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, Vcl.Controls, Vcl.StdCtrls, WEBLib.StdCtrls;

type
  TUserProfileSubForm = class(TWebForm)
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
  UserProfileSubForm: TUserProfileSubForm;

implementation

uses
  UnitMain, UnitIcons;

{$R *.dfm}

procedure TUserProfileSubForm.WebFormCreate(Sender: TObject);
begin
  labelDashboard.HTML := DMIcons.Profile_Menu+'User Profile';
  bcDashboards.HTML := DMICons.Dashboard_Menu+'Dashboards';
  bcDashboard.HTML := mainForm.CurrentFormIcon+MainForm.CurrentFormName;
  bcCurrent.hTML := DMIcons.Profile_Menu+'User Profile';
end;

procedure TUserProfileSubForm.WebFormShow(Sender: TObject);
begin
  MainForm.SubFormShow;
end;

procedure TUserProfileSubForm.bcDashboardClick(Sender: TObject);
begin
  asm
    pas.UnitMain.MainForm.CurrentForm.MenuClicked(pas.UnitMain.MainForm.CurrentFormName,'Dashboard','Sub')
  end;
end;

end.