unit UnitUserActionsSub;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, Vcl.Controls, Vcl.StdCtrls, WEBLib.StdCtrls;

type
  TUserActionsSubForm = class(TWebForm)
    actionsTitle: TWebLabel;
    actionsHistory: TWebLabel;
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

uses UnitMain, UnitIcons, UnitMenus;

{$R *.dfm}


procedure TUserActionsSubForm.WebFormCreate(Sender: TObject);
begin
  actionsTitle.HTML := DMIcons.Icon('Actions_Menu')+'User Actions';
  actionsHistory.HTML := '<pre>'+MainForm.ActionLog.Text+'</pre>';
  actionsHistory.ElementHandle.style.setProperty('max-height',IntToStr(MainForm.Height - 325)+'px');
  actionsHistory.ElementHandle.style.setProperty('margin','16px');

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

procedure TUserActionsSubForm.bcDashboardClick(Sender: TObject);
begin
  DMMenus.MenuClicked(StringReplace(MainForm.CurrentFormName,'Form','',[]),'Dashboard',StringReplace(MainForm.CurrentFormName,'Form','',[])+'Sub', True)
end;

end.
