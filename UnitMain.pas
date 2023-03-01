unit UnitMain;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, Vcl.Controls, Vcl.StdCtrls, WEBLib.StdCtrls,
  WEBLib.WebCtrls, System.DateUtils, WEBLib.ExtCtrls, XData.Web.Connection,
  XData.Web.Client, WEBLib.Toast, WEBLib.JSON,jsdelphisystem;

type
  TMainForm = class(TWebForm)
    divHost: TWebHTMLDiv;
    btnShowLog: TWebButton;
    divLog: TWebHTMLDiv;
    btnLoginForm: TWebButton;
    btnClearForm: TWebButton;
    XDataConn: TXDataWebConnection;
    divToasts: TWebHTMLDiv;
    tmrJWTRenewal: TWebTimer;
    procedure WebFormCreate(Sender: TObject);
    procedure LogAction(Action: String);
    procedure LoadForm(Form: String);
    procedure btnShowLogClick(Sender: TObject);
    procedure btnLoginFormClick(Sender: TObject);
    procedure btnClearFormClick(Sender: TObject);
    [async] procedure XDataConnect;
    [async] function XDataLogin(Username: String; Password: String):String;
    procedure XDataConnRequest(Args: TXDataWebConnectionRequest);
    procedure Toast(Header: String; Body: String);
    procedure tmrJWTRenewalTimer(Sender: TObject);
    [async] function JSONRequest(Endpoint: String; Params: Array of JSValue):String;
  private
    { Private declarations }
  public
    { Public declarations }
    App_Name: String;
    App_Short: String;
    App_Version: String;
    App_Release: String;
    App_Start: TDateTime;

    LoggedIn: Boolean;
    ActivityDetected: Boolean;
    IconSet: String;

    ActionLog: TStringList;
    LogVisible: Boolean;

    CurrentForm: TWebForm;
    CurrentFormName: String;

    JWT: String;
    JWT_Expiry: TDateTime;

    User_FirstName: String;
    User_MiddleName: String;
    User_LastName: String;
    User_EMail: String;
    User_Roles: TStringList;

    Role_Administrator: Boolean;
    Role_Sales: Boolean;
    Role_HR: Boolean;

    ToastCount: Integer;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses UnitIcons, UnitLogin, UnitAdministrator;

procedure TMainForm.WebFormCreate(Sender: TObject);
begin

  // Application Information
  App_Name := 'TMS WEB Core Template Demo';
  App_Short := 'Template';
  App_Version := '1.0';
  App_Release := '2022-Feb-28';
  App_Start := Now();
  MainForm.Caption := App_Name;

  // Load Icon Set
  IconSet := document.documentElement.getAttribute('iconset');
  if IconSet = '' then IconSet := 'default';
  DMIcons.InitializeIcons(IconSet);

  // Application State
  LoggedIn := False;
  LogVisible := False;

  // JWT Handling
  JWT := '';
  JWT_Expiry := TTimeZone.Local.ToUniversalTime(Now);

  // Form Management
  CurrentForm := nil;
  CurrentFormName := 'Initializing';

  // Application Information
  Caption := 'TMS WEB Core Template Demo';

  // User Information
  User_FirstName := '';
  User_MiddleName :=  '';
  User_LastName := '';
  User_EMail := '';
  User_Roles := TStringList.Create;

  Role_Administrator := False;
  Role_Sales := False;
  Role_HR := False;

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

  LogAction('');

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

  // Login Form
  if Form = 'Login' then
  begin
    divHost.ElementHandle.className := 'rounded border border-dark Login-Form';
    CurrentForm := TLoginForm.CreateNew(divHost.ElementID, @AfterCreate);
  end

  // Administrator Form
  else if Form = 'Administrator' then
  begin
    divHost.ElementHandle.className := 'rounded border border-dark Administrator-Form';
    CurrentForm := TAdministratorForm.CreateNew(divHost.ElementID, @AfterCreate);
  end

  // Not A Valid Form
  else
  begin
    divHost.ElementHandle.className := 'rounded border bg-white border-dark';
    CurrentFormName := 'Invalid Form';
    LogAction('Form Not Found: '+Form);

    // Probably display a better error message or redirect to another default
    // form if an attempt is made to load an unexpected form.
    if Form <> 'Clear'
    then divHost.HTML.Text := 'ERROR: Form Not Found ('+Form+')';
  end;

end;

procedure TMainForm.LogAction(Action: String);
var
  FilterAction: String;
begin
  FilterAction := StringReplace(Action, chr(10), '', [rfReplaceAll]);
  FilterAction := StringReplace(FilterAction, chr(13), '', [rfReplaceAll]);
  FilterAction := StringReplace(FilterAction, '"', '''', [rfReplaceAll]);

  // Log the action to a TStringList
  ActionLog.Add(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',  TTimeZone.Local.ToUniversalTime(Now))+' UTC  ['+CurrentFormName.PadRight(15)+']  '+FilterAction);

  // Be mindful that generating log entries resets the ActivityDetected state
  ActivityDetected := True;

  // If we're currently looking at the action log, then update it with what we just did
  if LogVisible
  then divLog.HTML.Text := '<pre>'+ActionLog.DelimitedText+'</pre>';
end;

procedure TMainForm.tmrJWTRenewalTimer(Sender: TObject);
begin
  // Renew JWT if there has been activity of some kind
  if ActivityDetected then
  begin

  end
  // Otherwise perform an automatic logout of this session
  else
  begin

  end;
end;

procedure TMainForm.Toast(Header, Body: String);
begin
  // Want ID to be unique
  ToastCount := ToastCount + 1;

  asm
    // Create Toast
    var toast = document.createElement('div');
    toast.className = 'toast';
    toast.setAttribute('id','toast'+this.ToastCount);
    toast.setAttribute('role','alert');
    toast.setAttribute('aria-live','assertive');
    toast.setAttribute('aria-atomic','true');
    toast.setAttribute('data-bs-delay','15000');

    // Create Toast Header
    var toasth = document.createElement('div');
    toasth.className = 'toast-header bg-danger text-white';
    toasth.innerHTML = '<strong class="me-auto">'+Header+'</strong>'+
                       '<small class="text-light">just now</small>'+
                       '<button type="button" class="btn-close" data-bs-dismiss="toast" aria-label="Close"></button>';

    // Create Toast Body
    var toastb = document.createElement('div');
    toastb.className = 'toast-body';
    toastb.innerHTML = Body;

    // Make Toast
    toast.appendChild(toasth);
    toast.appendChild(toastb);
    divToasts.firstElementChild.appendChild(toast);

    // Show Toast
    new bootstrap.Toast(toast).show();
  end;
end;

procedure TMainForm.btnClearFormClick(Sender: TObject);
begin
  // More for testing purposes than anything else
  LogAction('CLICK: Clear Form');
  LoadForm('Clear');
  Toast('divHost Component','Cleared.');
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

function TMainForm.JSONRequest(Endpoint: String; Params: array of JSValue): String;
var
  ClientConn: TXDataWebClient;
  Response: TXDataClientResponse;
  Blob: JSValue;
  Elapsed: TDateTime;
begin
  Elapsed := Now;
  Result := '';
  LogAction('Request: '+Endpoint);

  await(XDataConnect);
  if (XdataConn.Connected) then
  begin
    try
      ClientConn := TXDataWebClient.Create(nil);
      ClientConn.Connection := XDataConn;
      Response := await(ClientConn.RawInvokeAsync(Endpoint, Params));
      Blob := Response.Result;
      asm
        Result = await Blob.text();
      end;
    except on E: Exception do
      begin
        LogAction('Request Exception: '+Endpoint);
        LogAction(' --> ['+E.ClassName+']');
        LogAction(' --> '+E.Message);
      end;
    end;
  end;

  LogAction('Response: '+Endpoint+' ('+IntToStr(MillisecondsBetween(Now, Elapsed))+'ms)');
end;

procedure TMainForm.btnLoginFormClick(Sender: TObject);
begin
  LogAction('CLICK: Login Form');
  LoadForm('Login');
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
        LogAction('Connection Failed: '+XDataConn.URL);
        LogAction(' --> ['+E.ClassName+']');
        LogAction(' --> '+E.Message);
      end;
    end;
  end;
end;

procedure TMainForm.XDataConnRequest(Args: TXDataWebConnectionRequest);
begin
  Args.Request.Headers.SetValue('Authorization', JWT);
end;

function TMainForm.XDataLogin(Username, Password: String):String;
var
  Response: TXDataClientResponse;
  ClientConn: TXDataWebClient;
  Blob: JSValue;
  NewJWT: String;
  ElapsedTime: TDateTime;
  TZ: String;
  ErrorCode: String;
  ErrorMessage: String;
  JWTClaims: TJSONObject;
  i: Integer;
begin

  ElapsedTime := Now;
  NewJWT := '';
  TZ := '';
  ErrorCode := '';
  ErrorMessage := '';

  LogAction('');
  LogAction('Attempting Login');

  asm
    TZ = Intl.DateTimeFormat().resolvedOptions().timeZone;
  end;

  // Call it again in case it has been disconnected
  await(XDataConnect);


  if (XDataConn.Connected) then
  begin
    try
      ClientConn := TXDataWebClient.Create(nil);
      ClientConn.Connection := XDataConn;
      Response := await(ClientConn.RawInvokeAsync('ISystemService.Login', [
        Username,
        Password,
        'Testing', // API_KEY
        TZ
      ]));
      Blob := Response.Result;
      asm
        NewJWT = await Blob.text();
      end;
    except on E: Exception do
      begin
        // Get the error message we created in XData
        asm {
          var ErrorDetail = JSON.parse( await E.FErrorResult.FResponse.$o.FXhr.response.text() );
          ErrorCode = ErrorDetail.error.code;
          ErrorMessage = ErrorDetail.error.message;
        } end;

        // Log the error, but leave out the URI (because it includes the password)
        LogAction('Login Exception:');
        LogAction(' --> ['+E.ClassName+']');
        LogAction(' --> '+Copy(E.Message,1,Pos('Uri:',E.Message)-2));
        LogAction(' --> '+Copy(E.Message,Pos('Status code:',E.Message),16));
        LogAction(' --> '+ErrorCode);
        LogAction(' --> '+ErrorMessage);
      end;
    end;
  end;

  // We've got a JWT
  if Pos('Bearer ',NewJWT) = 1 then
  begin
    LogAction('Login Successful ('+IntToStr(MilliSecondsBetween(Now, ElapsedTime))+'ms)');
    Result := 'Success';
    LoggedIn := True;

    // Assign JWT to form variable - Added to authorization header via procedure TMainForm.XDataConnRequest
    JWT := NewJWT;

    // Get JSON Claims from JWT
    JWTClaims := TJSONObject.ParseJSONValue(Window.atob(Copy(JWT, Pos('.',JWT)+1, LastDelimiter('.',JWT)-Pos('.',JWT)-1))) as TJSONObject;

    // Extract user information
    User_FirstName :=  (JWTClaims.Get('fnm').JSONValue as TJSONString).Value;
    User_MiddleName :=  (JWTClaims.Get('mnm').JSONValue as TJSONString).Value;
    User_LastName :=  (JWTClaims.Get('lnm').JSONValue as TJSONString).Value;
    User_EMail :=  (JWTClaims.Get('eml').JSONValue as TJSONString).Value;
    User_Roles.CommaText :=  (JWTClaims.Get('rol').JSONValue as TJSONString).Value;

    // Set renewal to one minute before expiration
    JWT_Expiry := UnixToDateTime((JWTClaims.Get('exp').JSONValue as TJSONNumber).AsInt);
    tmrJWTRenewal.Enabled := False;
    tmrJWTRenewal.Interval := MillisecondsBetween(JWT_Expiry, TTimeZone.Local.ToUniversalTime(Now)) - 60000;
    tmrJWTRenewal.Enabled := True;

    // Extract Roles
    Role_Administrator := False;
    Role_Sales := False;
    Role_HR := False;
    i := 0;
    while i < User_Roles.Count do
    begin
      if User_Roles[i] = '1' then Role_Administrator := True;
      if User_Roles[i] = '2' then Role_Sales := True;
      if User_Roles[i] = '3' then Role_HR := True;
      i := i + 1;
    end;

    // Load selected form
    if Role_Administrator then
    begin
      LoadForm('Administrator');
    end
    else if Role_HR then
    begin
      LoadForm('HR')
    end
    else if Role_Sales then
    begin
      LoadForm('Sales');
    end;

    ActivityDetected := False;
  end
  else
  begin
    LoggedIn := False;
    Result := ErrorCode+' / '+ErrorMessage;
  end;
end;


end.
