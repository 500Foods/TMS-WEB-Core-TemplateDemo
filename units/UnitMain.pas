unit UnitMain;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, Vcl.Controls, Vcl.StdCtrls, WEBLib.StdCtrls,
  WEBLib.WebCtrls, System.DateUtils, WEBLib.ExtCtrls, XData.Web.Connection,
  XData.Web.Client, WEBLib.Toast, WEBLib.JSON,jsdelphisystem,WEBLib.Storage;

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
    procedure LogAction(Action: String; Extend: Boolean);
    procedure btnShowLogClick(Sender: TObject);
    procedure btnLoginFormClick(Sender: TObject);
    procedure btnClearFormClick(Sender: TObject);
    procedure XDataConnRequest(Args: TXDataWebConnectionRequest);
    procedure Toast(Header: String; Body: String);
    procedure MenuClick(Menu: String);
    procedure MenuAdd(Menu: String);
    procedure ProcessJWT(aJWT: String);
    procedure WebFormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    [async] procedure Logout(Reason: String);
    [async] procedure LoadForm(Form: String);
    [async] function JSONRequest(Endpoint: String; Params: Array of JSValue):String;
    [async] procedure XDataConnect;
    [async] function XDataLogin(Username: String; Password: String):String;
    [async] procedure tmrJWTRenewalTimer(Sender: TObject);
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
    ActionLogCurrent: TStringList;
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
  ActionLogCurrent := TStringList.Create;
  ActionLogCurrent.Delimiter := chr(10);
  LogAction('Application Startup', False);

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

  // What to do if the browser closes unexpectedly
  asm
    window.addEventListener('beforeunload', function (e) {
      pas.UnitMain.MainForm.Logout('Browser Closed');
//      e.preventDefault();
//      e.returnValue = '';
    });
  end;
end;

procedure TMainForm.WebFormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key= VK_F4 then Logout('F4');
end;

procedure TMainForm.LoadForm(Form: String);
var
  ElapsedTime: TDateTime;

  procedure AfterCreate(AForm: TObject);
  begin
    LogAction('Load Form: '+AForm.ClassName+' Loaded ('+IntToStr(MillisecondsBetween(Now, ElapsedTime))+'ms)', True);
  end;

begin
  // Time this action
  ElapsedTime := Now;

  LogAction('', False);

  if Assigned(CurrentForm) then
  begin
    LogAction('Removing Form: '+CurrentForm.ClassName, False);
    CurrentForm.Close;
    CurrentForm.Free;
    asm
      divHost.replaceChildren();
    end;
  end;

  LogAction('Load Form: '+Form, False);
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
    TWebLocalStorage.SetValue('Login.CurrentForm', Form);
    divHost.ElementHandle.className := 'rounded border border-dark Administrator-Form';
    CurrentForm := TAdministratorForm.CreateNew(divHost.ElementID, @AfterCreate);
  end

  // Not A Valid Form
  else
  begin
    divHost.ElementHandle.className := 'rounded border bg-white border-dark';
    CurrentFormName := 'Invalid Form';
    LogAction('Form Not Found: '+Form, False);

    // Probably display a better error message or redirect to another default
    // form if an attempt is made to load an unexpected form.
    if Form <> 'Clear'
    then divHost.HTML.Text := 'ERROR: Form Not Found ('+Form+')';
  end;

end;

procedure TMainForm.LogAction(Action: String; Extend: Boolean);
var
  FilterAction: String;
begin
  FilterAction := StringReplace(Action, chr(10), '', [rfReplaceAll]);
  FilterAction := StringReplace(FilterAction, chr(13), '', [rfReplaceAll]);
  FilterAction := StringReplace(FilterAction, '"', '''', [rfReplaceAll]);

  // Log the action to a TStringList
  ActionLog.Add(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',  TTimeZone.Local.ToUniversalTime(Now))+' UTC  ['+CurrentFormName.PadRight(15)+']  '+FilterAction);
  ActionLogCurrent.Add(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',  TTimeZone.Local.ToUniversalTime(Now))+' UTC  ['+CurrentFormName.PadRight(15)+']  '+FilterAction);

  // Be mindful that generating log entries can reset the ActivityDetected state
  if Extend then ActivityDetected := True;

  // If we're currently looking at the action log, then update it with what we just did
  if LogVisible
  then divLog.HTML.Text := '<pre>'+ActionLog.DelimitedText+'</pre>';
end;

procedure TMainForm.Logout(Reason: String);
var
  ResponseString: String;
begin
  if CurrentFormName <> 'Login' then
  begin
    LogAction('Logout: '+Reason, False);

    ResponseString := await(JSONRequest('ISystemService.Logout',[ActionLogCurrent.Text]));

    JWT := '';
    TWebLocalStorage.setValue('Login.CurrentForm','Login');
    TWebLocalStorage.RemoveKey('Login.JWT');
    TWebLocalStorage.RemoveKey('Login.Expiry');

    window.location.reload(true);
  end;
end;

procedure TMainForm.MenuAdd(Menu: String);
begin
  // Add a menu
end;

procedure TMainForm.MenuClick(Menu: String);
begin
  // A menu option has been selected.  Here we decide what to do about it.

end;

procedure TMainForm.ProcessJWT(aJWT: String);
var
  JWTClaims: TJSONObject;
  i: Integer;
begin
  JWT := aJWT;

  // Get JSON Claims from JWT
  JWTClaims := TJSONObject.ParseJSONValue(Window.atob(Copy(JWT, Pos('.',JWT)+1, LastDelimiter('.',JWT)-Pos('.',JWT)-1))) as TJSONObject;

  // Extract user information
  User_FirstName :=  (JWTClaims.Get('fnm').JSONValue as TJSONString).Value;
  User_MiddleName :=  (JWTClaims.Get('mnm').JSONValue as TJSONString).Value;
  User_LastName :=  (JWTClaims.Get('lnm').JSONValue as TJSONString).Value;
  User_EMail :=  (JWTClaims.Get('eml').JSONValue as TJSONString).Value;

  // If roles have changed since logging in, then inform the user
  if (CurrentFormName <> 'Login') and (User_Roles.CommaText <> (JWTClaims.Get('rol').JSONValue as TJSONString).Value)
  then Toast('Updated Roles', 'The roles for this account have been updated. Please Logout and Login again to access them.');
  User_Roles.CommaText :=  (JWTClaims.Get('rol').JSONValue as TJSONString).Value;

  // Set renewal to one minute before expiration
  JWT_Expiry := UnixToDateTime((JWTClaims.Get('exp').JSONValue as TJSONNumber).AsInt);
  tmrJWTRenewal.Enabled := False;
  tmrJWTRenewal.Interval := MillisecondsBetween(JWT_Expiry, TTimeZone.Local.ToUniversalTime(Now)) - 60000;

  // If it has already expired, then not much point in continuing?
  if tmrJWTRenewal.Interval <= 0
  then Logout('JWT Expired')
  else tmrJWTRenewal.Enabled := True;

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
end;

procedure TMainForm.tmrJWTRenewalTimer(Sender: TObject);
var
  ResponseString: String;
  ActionLogSend: String;
begin
  tmrJWTRenewal.Enabled := False;

  console.log('Renewing JWT');

  ActionLogSend := ActionLogCurrent.Text;
  ActionLogCurrent.Text := '';

  // Renew JWT if there has been activity of some kind
  if ActivityDetected then
  begin
    ResponseString := await(JSONRequest('ISystemService.Renew',[ActionLogSend]));
    if ResponseString <> '' then
    begin
      LogAction('Login Renewed', False);
      ProcessJWT(ResponseString);
      ActivityDetected := False;
    end
    else
    begin
    // Otherwise perform an automatic logout of this session
      Logout('NNoRenewal');
    end
  end
  else
  begin
    Logout('Inactivity');
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
  LogAction('CLICK: Clear Form', True);
  LoadForm('Clear');
  Toast('divHost Component','Cleared.');
end;

procedure TMainForm.btnShowLogClick(Sender: TObject);
begin
  if LogVisible then
  begin
    LogAction('CLICK: Hide Application Action Log', True);
    LogVisible := False;
    btnShowLog.Caption := 'Show Log';
    divLog.Visible := False;
    divHost.Visible := True;
  end
  else
  begin
    LogAction('CLICK: Show Application Action Log', True);
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
  LogAction('Request: '+Endpoint, False);

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
        LogAction('Request Exception: '+Endpoint, False);
        LogAction(' --> ['+E.ClassName+']', False);
        LogAction(' --> '+E.Message, False);
      end;
    end;
  end;

  LogAction('Response: '+Endpoint+' ('+IntToStr(MillisecondsBetween(Now, Elapsed))+'ms)', False);
end;

procedure TMainForm.btnLoginFormClick(Sender: TObject);
begin
  LogAction('CLICK: Login Form', True);
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
      LogAction('Connecting to: '+XDataConn.URL, False);
      await(XDataConn.OpenAsync);
      LogAction('Connection Established: ('+IntToStr(MillisecondsBetween(Now, ElapsedTime))+'ms)', False);
    except on E: Exception do
      begin
        LogAction('Connection Failed: '+XDataConn.URL, False);
        LogAction(' --> ['+E.ClassName+']', False);
        LogAction(' --> '+E.Message, False);
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
  i: Integer;
begin

  ElapsedTime := Now;
  NewJWT := '';
  TZ := '';
  ErrorCode := '';
  ErrorMessage := '';

  LogAction('', False);
  LogAction('Attempting Login', False);

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
        LogAction('Login Exception:', False);
        LogAction(' --> ['+E.ClassName+']', False);
        LogAction(' --> '+Copy(E.Message,1,Pos('Uri:',E.Message)-2), False);
        LogAction(' --> '+Copy(E.Message,Pos('Status code:',E.Message),16), False);
        LogAction(' --> '+ErrorCode, False);
        LogAction(' --> '+ErrorMessage, False);
      end;
    end;
  end;

  // We've got a JWT
  if Pos('Bearer ',NewJWT) = 1 then
  begin
    LogAction('Login Successful ('+IntToStr(MilliSecondsBetween(Now, ElapsedTime))+'ms)', False);
    Result := 'Success';
    LoggedIn := True;

    // Figure out what to do with the JWT
    ProcessJWT(NewJWT);

    // If nothing happens in 15 minutes, we'll automatically logout
//    ActivityDetected := False;
  end
  else
  begin
    LoggedIn := False;
    Result := ErrorCode+' / '+ErrorMessage;
  end;
end;


end.
