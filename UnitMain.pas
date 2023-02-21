unit UnitMain;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, Vcl.Controls, Vcl.StdCtrls, WEBLib.StdCtrls,
  WEBLib.WebCtrls, System.DateUtils, WEBLib.ExtCtrls;

type
  TMainForm = class(TWebForm)
    divHost: TWebHTMLDiv;
    btnShowLog: TWebButton;
    divLog: TWebHTMLDiv;
    btnLoginForm: TWebButton;
    btnClearForm: TWebButton;
    procedure WebFormCreate(Sender: TObject);
    procedure LogAction(Action: String);
    procedure LoadForm(Form: String);
    procedure btnShowLogClick(Sender: TObject);
    procedure btnLoginFormClick(Sender: TObject);
    procedure btnClearFormClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    LoggedIn: Boolean;
    ActionLog: TStringList;
    CurrentForm: TWebForm;
    CurrentFormName: String;
    LogVisible: Boolean;


  end;

var
  MainForm: TMainForm;

implementation

uses UnitLogin;

{$R *.dfm}

procedure TMainForm.LoadForm(Form: String);
var
  ElapsedTime: TDateTime;

  procedure AfterCreate(AForm: TObject);
  begin
    LogAction('Load Form: '+AForm.ClassName+' Loaded ('+IntToStr(MillisecondsBetween(Now, ElapsedTime))+'ms)');
  end;

begin
  // Time this action
  ElapsedTime := Now;

  if Assigned(CurrentForm) then
  begin
    LogAction('Removing Form: '+CurrentForm.ClassName);
    CurrentForm.Close;
    CurrentForm.Free;
    asm
      divHost.replaceChildren();
    end;
  end;

  LogAction('Load Form: '+Form);
  CurrentFormName := Form;

  // Login FOrm
  if Form = 'Login' then
  begin
    CurrentForm := TLoginForm.CreateNew(divHost.ElementID, @AfterCreate);
  end

  // Not A Valid Form
  else
  begin
    CurrentFormName := 'Invalid Form';
    LogAction('Form Not Found: '+Form);

    // Probably display a better error message or redirect to another default
    // form if an attempt is made to load an unexpected form.
    if Form <> 'Clear'
    then divHost.HTML.Text := 'ERROR: Form Not Found ('+Form+')';
  end;

end;

procedure TMainForm.LogAction(Action: String);
begin
  ActionLog.Add(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',  TTimeZone.Local.ToUniversalTime(Now))+' UTC  ['+CurrentFormName.PadRight(15)+']  '+Action);

  if LogVisible
  then divLog.HTML.Text := '<pre>'+ActionLog.DelimitedText+'</pre>';
end;

procedure TMainForm.btnClearFormClick(Sender: TObject);
begin
  LogAction('CLICK: Clear Form');
  LoadForm('Clear');
end;

procedure TMainForm.btnShowLogClick(Sender: TObject);
begin
  if LogVisible then
  begin
    LogAction('CLICK: Hide Application Action Log');
    LogVisible := False;
    btnShowLog.Caption := 'Show Log';
    divLog.Visible := False;
    divHost.Visible := True;

  end
  else
  begin
    LogAction('CLICK: Show Application Action Log');
    LogVisible := True;
    btnShowLog.Caption := 'Hide Log';
    divLog.Visible := True;
    divHost.Visible := False;
    divLog.HTML.Text := '<pre>'+ActionLog.DelimitedText+'</pre>';
  end;
end;

procedure TMainForm.btnLoginFormClick(Sender: TObject);
begin
  LogAction('CLICK: Login Form');
  LoadForm('Login');
end;

procedure TMainForm.WebFormCreate(Sender: TObject);
begin

  // Application State
  LoggedIn := False;
  LogVisible := False;
  CurrentForm := nil;
  CurrentFormName := 'Initializing';

  // Log what we're doing in the application
  ActionLog := TStringList.Create;
  ActionLog.Delimiter := chr(10);
  LogAction('Application Startup');

  // Setup the Log Viewer
  divLog.Top := divHost.Top;
  divLog.Left := divHost.Left;
  divLog.Width := divHost.Width;
  divLog.Height := divHost.Height;
  divLog.Visible := False;

  // Launch Login
  if not(LoggedIn) then
  begin
    LoadForm('Login');
  end;

end;


end.