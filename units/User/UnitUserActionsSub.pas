unit UnitUserActionsSub;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls, System.DateUtils,
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
var
  LocalActionLog: TStringList;
  LogLine: String;
  OldDate: TDateTime;
  NewDate: TDateTime;
  i: Integer;
begin
  actionsTitle.HTML := DMIcons.Icon('Actions_Menu')+'User Actions';
  actionsHistory.ElementHandle.style.setProperty('max-height',IntToStr(MainForm.Height - 325)+'px');
  actionsHistory.ElementHandle.style.setProperty('margin','16px');

  LocalActionLog := TStringList.Create;
  i := 0;
  while i < MainForm.ActionLog.Count do
  begin
    LogLine := MainForm.ActionLog[i];

    if Copy(LogLine,24,7) = ' UTC  [' then
    begin
      OldDate := EncodeDateTime(
        StrToInt(Copy(LogLine,1,4)),
        StrToInt(Copy(LogLine,6,2)),
        StrToInt(Copy(LogLine,9,2)),
        StrToInt(Copy(LogLine,12,2)),
        StrToInt(Copy(LogLine,15,2)),
        StrToInt(Copy(LogLine,18,2)),
        StrToInt(Copy(LogLine,21,3))
      );
      NewDate := IncMinute(OldDate, - MainForm.App_TZOffset);
      LogLine := FormatDateTime(MainForm.App_DisplayDateTimeFormat+'.zzz', NewDate)+Copy(LogLine,28,length(LogLine));
    end;

    LocalActionLog.Add(LogLine);
    i := i + 1;
  end;

  actionsHistory.HTML := '<pre>'+LocalActionLog.Text+'</pre>';

  asm
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
