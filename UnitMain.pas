unit UnitMain;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, Vcl.Controls, Vcl.StdCtrls, WEBLib.StdCtrls,
  WEBLib.WebCtrls, System.DateUtils, WEBLib.ExtCtrls, XData.Web.Connection,
  XData.Web.Client;

type
  TMainForm = class(TWebForm)
    divHost: TWebHTMLDiv;
    btnShowLog: TWebButton;
    divLog: TWebHTMLDiv;
    btnLoginForm: TWebButton;
    btnClearForm: TWebButton;
    XDataConn: TXDataWebConnection;
    procedure WebFormCreate(Sender: TObject);
    procedure LogAction(Action: String);
    procedure LoadForm(Form: String);
    procedure btnShowLogClick(Sender: TObject);
    procedure btnLoginFormClick(Sender: TObject);
    procedure btnClearFormClick(Sender: TObject);
    [async] procedure XDataConnect;
    [async] function XDataLogin(Username: String; Password: String):Boolean;
    procedure resError(Error: TXDataClientError);
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

procedure TMainForm.resError(Error: TXDataClientError);
begin
  asm
  console.log(error)
  end;
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

  // Connect to XData - it will finish on its own time
  XDataConnect;

  // Launch Login
  if not(LoggedIn) then
  begin
    LoadForm('Login');
  end;

end;


procedure TMainForm.XDataConnect;
var
  ElapsedTime: TDateTime;
begin
  ElapsedTime := Now;

  if not(XDataConn.Connected) then
  begin

    // Should be updated to point at our XData server, wherever it may be
    XDataConn.URL := 'http://localhost:12345/tms/xdata';

    // Try and establish a connection to the server
    try
      LogAction('Connecting to: '+XDataConn.URL);
      await(XDataConn.OpenAsync);
      LogAction('Connection Established: ('+IntToStr(MillisecondsBetween(Now, ElapsedTime))+'ms)');
    except on E: Exception do
      begin
        LogAction('Server Connection Failed: '+XDataConn.URL);
        LogAction('Error: ['+E.ClassName+'] '+E.Message);
      end;
    end;
  end;
end;

function TMainForm.XDataLogin(Username, Password: String):Boolean;
var
  Response: TXDataClientResponse;
  ClientConn: TXDataWebClient;
  Blob: JSValue;
  JWT: String;
  ElapsedTime: TDateTime;
  TZ: String;

  procedure ClientError(Error: TXDataClientError);
  var
    ErrResponse: JSValue;
  begin
    ErrResponse := Error.Response.Content;
    asm
      var reader = new FileReader();
      reader.addEventListener('loadend', (e) => {
        var ErrMessage = JSON.parse(e.srcElement.result);
        pas.UnitMain.MainForm.LogAction(' --> '+ErrMessage.error.code);
        pas.UnitMain.MainForm.LogAction(' --> '+ErrMessage.error.message);
        msgLogin.innerHTML = ErrMessage.error.code+' / '+ErrMessage.error.message;
      });
      reader.readAsText(ErrResponse);
    end;
  end;

begin

  ElapsedTime := Now;
  JWT := '';
  TZ := '';
  LogAction('');
  LogAction('Attempting Login');

  asm
    TZ = Intl.DateTimeFormat().resolvedOptions().timeZone;
  end;

  await(XDataConnect);
  if (XDataConn.Connected) then
  begin
    try

      ClientConn := TXDataWebClient.Create(nil);
      ClientConn.OnError := ClientError;
      ClientConn.Connection := XDataConn;
      Response := await(ClientConn.RawInvokeAsync('ISystemService.Login', [
        Username,
        Password,
        'Testing', // API_KEY
        TZ
      ]));
      Blob := Response.Result;
      asm
       JWT = await Blob.text();
      end;
    except on E: Exception do
      begin
        LogAction('Login Exception:');
        LogAction(' --> ['+E.ClassName+']');
        LogAction(' --> '+Copy(E.Message,1,Pos('.',E.Message)));
        LogAction(' --> '+Copy(E.Message,Pos('Status code:',E.Message),16));
      end;
    end;
  end;

  // We've got a JWT
  if Pos('Bearer ',JWT) = 1 then
  begin
    LogAction('Login Successful ('+IntToStr(MilliSecondsBetween(Now, ElapsedTime))+'ms)');
    Result := True;
  end
  else
  begin
    Result := False;
  end;
end;

end.
