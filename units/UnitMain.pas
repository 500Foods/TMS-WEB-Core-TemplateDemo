unit UnitMain;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, Vcl.Controls, Vcl.StdCtrls, WEBLib.StdCtrls,
  WEBLib.WebCtrls, System.DateUtils, WEBLib.ExtCtrls, XData.Web.Connection,
  XData.Web.Client, WEBLib.JSON, jsdelphisystem, WEBLib.Storage;

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
    tmrJWTRenewalWarning: TWebTimer;
    procedure LogAction(Action: String; Extend: Boolean);
    procedure btnShowLogClick(Sender: TObject);
    procedure btnLoginFormClick(Sender: TObject);
    procedure btnClearFormClick(Sender: TObject);
    procedure XDataConnRequest(Args: TXDataWebConnectionRequest);
    procedure Toast(Header: String; Body: String; Timeout: Integer);
    procedure ProcessJWT(aJWT: String);
    procedure WebFormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure tmrJWTRenewalWarningTimer(Sender: TObject);
    procedure WebFormClick(Sender: TObject);
    procedure FinalRequest;
    [async] procedure WebFormCreate(Sender: TObject);
    [async] procedure Logout(Reason: String);
    [async] procedure LoadForm(Form: String; FormIcon: String);
    [async] procedure LoadSubForm(SubForm: String; SubFormIcon: String);
    [async] procedure XDataConnect;
    [async] procedure tmrJWTRenewalTimer(Sender: TObject);
    [async] function XDataLogin(Username: String; Password: String):String;
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
    App_Start_UTC: TDateTime;
    App_Session: String;

    LoggedIn: Boolean;
    ActivityDetected: Boolean;
    IconSet: String;

    ActionLog: TStringList;
    ActionLogCurrent: TStringList;
    LogVisible: Boolean;

    CurrentForm: TWebForm;
    CurrentFormName: String;
    CurrentFormIcon: String;

    CurrentSubForm: TWebForm;
    CurrentSubFormName: String;
    CurrentSubFormIcon: String;

    JWT: String;
    JWT_Expiry: TDateTime;
    Remember: Boolean;

    User_FirstName: String;
    User_MiddleName: String;
    User_LastName: String;
    User_EMail: String;

    User_Roles: TStringList;
    Roles: String;
    Role_Administrator: Boolean;

    ToastCount: Integer;

    procedure StopLinkerRemoval(P: Pointer);
    procedure PreventCompilerHint(I: integer); overload;
    procedure PreventCompilerHint(S: string); overload;
    procedure PreventCompilerHint(J: JSValue); overload;
    procedure PreventCompilerHint(H: TJSHTMLElement); overload;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  UnitIcons, UnitLogin,
  UnitUserProfileSub, UnitUserActionsSub,
  UnitAdministrator, UnitAdministratorSub;

procedure TMainForm.WebFormCreate(Sender: TObject);
var
  i: Int64;
begin

  // Application Information
  App_Name := 'TMS WEB Core Template Demo';
  App_Short := 'Template';
  App_Version := '1.1';
  App_Release := '2022-Mar-08';
  App_Start := Now();
  App_Start_UTC := TTimeZone.Local.ToUniversalTime(Now);

  // MainForm Options
  MainForm.Caption := App_Name;
  MainForm.divHost.ElementHandle.style.setProperty('opacity','0');

  // Load Icon Set
  IconSet := document.documentElement.getAttribute('iconset');
  if IconSet = '' then IconSet := 'default';
  DMIcons.InitializeIcons(IconSet);

  // Application State
  LoggedIn := False;
  LogVisible := False;

  // JWT Handling
  JWT := '';
  JWT_Expiry := App_Start_UTC;
  Remember := True;

  // User Information
  User_FirstName := '';
  User_MiddleName :=  '';
  User_LastName := '';
  User_EMail := '';

  // Role Information
  User_Roles := TStringList.Create;
  Role_Administrator := False;
  Roles := '';

  // Log what we're doing in the application
  ActionLog := TStringList.Create;
  ActionLog.Delimiter := chr(10);
  ActionLogCurrent := TStringList.Create;
  ActionLogCurrent.Delimiter := chr(10);
  CurrentFormName := 'Module / Dashboard - Page';
  LogAction('============================================================', False);

  // Form Management
  CurrentForm := nil;
  CurrentFormName := 'Initializing';
  CurrentFormIcon := '';
  CurrentSubForm := nil;
  CurrentSubFormName := '';
  CurrentSubFormIcon := '';

  // Forms and SubForms that we've defined so far in this project
  asm {
    window.ValidForms = [
      'AdministratorForm',
      'LoginForm'
    ];
    window.ValidSubForms = [
      'AdministratorSub',
      'UserActionsSub',
      'UserProfileSub'
    ]
  } end;

  // Create an App Session key - just a custom Base48-encoded timestamp
  // https://github.com/marko-36/base29-shortener
  App_Session := '';
  i := DateTimeToUnix(App_Start_UTC);

  asm
    // Encode Integer (eg: Unix Timestamp) into String
    const c = ['B','b','C','c','D','d','F','f','G','g','H','h','J','j','K','k','L','M','m','N','n','P','p','Q','q','R','r','S','s','T','t','V','W','w','X','x','Z','z','0','1','2','3','4','5','6','7','8','9'];
    var sLen = Math.floor(Math.log(i)/Math.log(c.length)) +1;
    for(var ex=sLen-1; ex>-1; --ex){
      this.App_Session += c[Math.floor(i / Math.pow(c.length,ex))];
      i = [i % Math.pow(c.length,ex)];
    }

    // Decode String into Integer
    // var s = this.App_Session;
    // i = 0;
    // for (var ex=0; ex<s.length; ++ex){
    //   i += c.indexOf(s.substring(ex,ex+1)) * Math.pow(c.length,s.length-1-ex);
    // }
    // return i
  end;

  // Application Details
  LogAction('Application Startup', False);
  LogAction(' -> '+App_Name, False);
  LogAction(' -> Version '+App_Version, False);
  LogAction(' -> Release '+App_Release, False);
  LogAction(' -> App Started: '+FormatDateTime('yyyy-MMM-dd hh:nn:ss.zzz', App_Start), False);
  LogAction(' -> App Started: '+FormatDateTime('yyyy-MMM-dd hh:nn:ss.zzz', App_Start_UTC)+' UTC', False);
  LogAction(' -> App Session: '+App_Session, False);
  asm {
    this.LogAction(' -> '+window.ValidForms.length+' Forms', false);
    this.LogAction(' -> '+window.ValidSubForms.length+' SubForms', false);
    this.LogAction(' -> '+Object.keys(pas.UnitIcons.DMIcons.Lookup).length+' Icons', false);
  } end;
  LogAction('============================================================', False);


  // Setup the Log Viewer
  divLog.Visible := False;
  divLog.Top := divHost.Top;
  divLog.Left := divHost.Left;
  divLog.Width := divHost.Width;
  divLog.Height := divHost.Height;

  // Setup global sleep function :)
  asm window.sleep = async function(msecs) {return new Promise((resolve) => setTimeout(resolve, msecs)); } end;

  // Connect to XData - it will finish on its own time but give it a moment to connect
  LogAction('', False);
  XDataConnect;
  asm await sleep(100); end;

  // Launch Login
  CurrentFormName := 'LoginForm';
  LoadForm('LoginForm', DMIcons.Icon('Login'));

  // What to do if the browser closes unexpectedly
  asm {
    window.addEventListener('beforeunload', async function (e) {

      pas.UnitMain.MainForm.FinalRequest();

      // Option 1:
      // This logs out user when tab is closed
      // Highest security
      // pas.UnitMain.MainForm.Logout('Browser Closed');

      // Option 2:
      // Do nothing
      // Lower security but more convenient
      // - don't have to login as often.
      // - survives browser restart.

      // Option 3:
      // This enables annoying browser dialog
      // Can be used with either of the above options
      //      e.preventDefault();
      //      e.returnValue = '';

    });

  } end;

end;

procedure TMainForm.WebFormClick(Sender: TObject);
begin
  if (CurrentFormName <> 'LoginForm')
  then ActivityDetected := True;
end;

procedure TMainForm.FinalRequest;
begin
  LogAction('Application Unloaded',False);
  LogAction('Session Duration: '+FormatDateTime('h"h "m"m "s"s"', Now - App_Start), False);
  JSONRequest('ISystemService.Renew',[App_Session, ActionLogCurrent.Text]);
end;

procedure TMainForm.WebFormKeyDown(Sender: TObject; var Key: Word;    Shift: TShiftState);
begin
  if (Key = VK_F4) then Logout('F4');
end;

procedure TMainForm.LoadForm(Form: String; FormIcon: String);
var
  ElapsedTime: TDateTime;
  ValidForm: Boolean;

  procedure AfterCreate(AForm: TObject);
  begin
    LogAction('Load Form: '+Form+' Loaded ('+IntToStr(MillisecondsBetween(Now, ElapsedTime)-500)+'ms)', False);
  end;

begin
  // Time this action
  ElapsedTime := Now;

  // Big change
  LogAction('', False);

  // Is this a valid Form?
  ValidForm := False;
  asm if (window.ValidForms.includes(Form)) { ValidForm = true; } end;
  if not(ValidForm) then
  begin
    LogAction('ERROR: Form Not Found: '+Form, False);
    Toast(DMIcons.Icon('Bug_Menu')+'Form Error', 'Form Not Found: <br />[ '+Form+' ]',15000);
    exit;
  end;

  // Hide the old Form
  MainForm.divHost.ElementHandle.style.setProperty('opacity','0','important');
  asm await sleep(500); end;

  // Remove old Form
  if Assigned(CurrentForm) then
  begin
    LogAction('Drop Form: '+CurrentFormName, False);
    CurrentForm.Close;
    asm
      divHost.replaceChildren();
    end;
  end;

  // Note the new Form
  CurrentFormName := Form;
  CurrentFormIcon := FormIcon;
  divHost.ElementHandle.className := Form;
  LogAction('Load Form: '+Form, False);
  if (Form <> 'LoginForm') then
  begin
    TWebLocalStorage.SetValue('Login.CurrentForm', Form);
    TWebLocalStorage.SetValue('Login.CurrentFormIcon', FormIcon);
  end;

  // Launch Form
  if      (Form = 'LoginForm')         then CurrentForm := TLoginForm.CreateNew(divHost.ElementID, @AfterCreate)
  else if (Form = 'AdministratorForm') then CurrentForm := TAdministratorForm.CreateNew(divHost.ElementID, @AfterCreate);

end;
procedure TMainForm.LoadSubForm(SubForm: String; SubFormIcon: String);
var
  ElapsedTime: TDateTime;
  ValidSubForm: Boolean;
  divSubForm: TJSElement;

  procedure AfterSubCreate(AForm: TObject);
  begin
    LogAction('Load SubForm: '+AForm.ClassName+' Loaded ('+IntToStr(MillisecondsBetween(Now, ElapsedTime)-500)+'ms)', False);
  end;

begin
  // Time this action
  ElapsedTime := Now;

  // Is this a valid SubForm?
  ValidSubForm := False;
  asm if (window.ValidSubForms.includes(SubForm)) { ValidSubForm = true; } end;
  if not(ValidSubForm) then
  begin
    LogAction('SubForm Not Found: '+SubForm, False);
    Toast(DMIcons.Icon('Bug_Menu')+'ERROR: SubForm Error', 'SubForm Not Found: <br />[ '+SubForm+' ]',15000);
    exit;
  end;

  // Hide the old SubForm
  divSubForm := document.getElementById('divSubForm');
  asm divSubForm.style.setProperty('opacity','0','important'); end;
  asm await sleep(500); end;

  // Remove the old SubForm
  if Assigned(CurrentSubForm) then
  begin
    LogAction('Drop SubForm: '+CurrentSubFormName, False);
    CurrentSubForm.Close;
    asm
      document.getElementById('divSubForm').replaceChildren();
    end;
  end;

  // Note the new SubForm
  CurrentSubFormName := SubForm;
  CurrentSubFormIcon := SubFormIcon;
  asm
    document.getElementById('divSubForm').className = 'app-main '+SubForm;
  end;
  LogAction('Load SubForm: '+SubForm, False);
  TWebLocalStorage.SetValue('Login.CurrentSubForm', SubForm);
  TWebLocalStorage.SetValue('Login.CurrentSubFormIcon', SubFormIcon);

  // Launch SubForm
  if      (SubForm = 'UserProfileSub')   then CurrentSubForm := TUserProfileSubForm.CreateNew(divSubForm.id, @AfterSubCreate)
  else if (SubForm = 'UserActionsSub')   then CurrentSubForm := TUserActionsSubForm.CreateNew(divSubForm.id, @AfterSubCreate)
  else if (SubForm = 'AdministratorSub') then CurrentSubForm := TAdministratorSubForm.CreateNew(divSubForm.id, @AfterSubCreate);
end;


procedure TMainForm.LogAction(Action: String; Extend: Boolean);
var
  FilterAction: String;
  Module: String;
begin
  FilterAction := StringReplace(Action, chr(10), '', [rfReplaceAll]);
  FilterAction := StringReplace(FilterAction, chr(13), '', [rfReplaceAll]);
  FilterAction := StringReplace(FilterAction, '"', '''', [rfReplaceAll]);

  Module := StringReplace(CurrentFormName,'Form','',[]);
  if (CurrentSubFormName <> '')
  then Module := Module +'-'+StringReplace(CurrentSubFormName,'Sub','',[]);
  Module := Module.PadRight(30);

  // Log the action to a TStringList
  ActionLog.Add(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',  TTimeZone.Local.ToUniversalTime(Now))+' UTC  ['+Module+']  '+FilterAction);
  ActionLogCurrent.Add(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',  TTimeZone.Local.ToUniversalTime(Now))+' UTC  ['+Module+']  '+FilterAction);

  // Log to Console
//  console.Log(FilterAction);

  // Be mindful that generating log entries can reset the ActivityDetected state
  if Extend then ActivityDetected := True;

  // If we're currently looking at the action log, then update it with what we just did
  if LogVisible
  then divLog.HTML.Text := '<pre>'+ActionLog.DelimitedText+'</pre>';
end;

procedure TMainForm.Logout(Reason: String);
begin
  // Make sure these don't fire
  tmrJWTRenewal.Enabled := False;
  tmrJWTRenewalWarning.Enabled := False;

  if CurrentFormName <> 'Login' then
  begin
    LogAction('Logout: '+Reason, False);
    LogAction('Session Duration: '+FormatDateTime('h"h "m"m "s"s"', Now - App_Start), False);
    Toast(DMIcons.Icon('Logout')+'Logout','Processing. Please wait.',1000);

    await(JSONRequest('ISystemService.Logout',[App_Session, ActionLogCurrent.Text]));

    JWT := '';
    TWebLocalStorage.SetValue('Login.CurrentForm','Login');
    TWebLocalStorage.RemoveKey('Login.JWT');
    TWebLocalStorage.RemoveKey('Login.Expiry');

    asm
      await sleep(1000);
      divHost.style.setProperty('opacity','0');
      await sleep(1000);
    end;

    window.location.reload(true);
  end;
end;

procedure TMainForm.ProcessJWT(aJWT: String);
var
  JWTClaims: TJSONObject;
  i: Integer;
begin
  JWT := aJWT;

  // Roles Disabled by Default
  Role_Administrator := False;

  // Get JSON Claims from JWT
  JWTClaims := TJSONObject.ParseJSONValue(Window.atob(Copy(JWT, Pos('.',JWT)+1, LastDelimiter('.',JWT)-Pos('.',JWT)-1))) as TJSONObject;

  // Extract user information
  User_FirstName :=  (JWTClaims.Get('fnm').JSONValue as TJSONString).Value;
  User_MiddleName :=  (JWTClaims.Get('mnm').JSONValue as TJSONString).Value;
  User_LastName :=  (JWTClaims.Get('lnm').JSONValue as TJSONString).Value;
  User_EMail :=  (JWTClaims.Get('eml').JSONValue as TJSONString).Value;

  // If roles have changed since logging in, then inform the user
  if (CurrentFormName <> 'Login')
     and (User_Roles.CommaText <> (JWTClaims.Get('rol').JSONValue as TJSONString).Value)
     and (User_Roles.CommaText <> '')
  then Toast(DMIcons.Icon('Certificate')+'Updated Roles', 'The roles for this account have been updated. Please Logout and Login again to access them.', 15000);
  User_Roles.CommaText :=  (JWTClaims.Get('rol').JSONValue as TJSONString).Value;
  Roles := (JWTClaims.Get('rol').JSONValue as TJSONString).Value;

  // Set renewal to one minute before expiration
  JWT_Expiry := UnixToDateTime((JWTClaims.Get('exp').JSONValue as TJSONNumber).AsInt);
  tmrJWTRenewal.Enabled := False;
  tmrJWTRenewal.Interval := MillisecondsBetween(JWT_Expiry, TTimeZone.Local.ToUniversalTime(Now)) - 30000;
  tmrJWTRenewalWarning.Interval := MillisecondsBetween(JWT_Expiry, TTimeZone.Local.ToUniversalTime(Now)) - 90000;

  // If it has already expired, then not much point in continuing?
  if tmrJWTRenewal.Interval <= 0 then
  begin
    Logout('JWT Expired')
  end
  else
  begin
    tmrJWTRenewal.Enabled := True;
    tmrJWTRenewalWarning.Enabled := True;

    if Remember then
    begin
      TWebLocalStorage.SetValue('Login.JWT', MainForm.JWT);
      TWebLocalStorage.SetValue('Login.Expiry', FloatToStr(MainForm.JWT_Expiry));
    end;

    // Extract Roles
    Role_Administrator := False;
    i := 0;
    while i < User_Roles.Count do
    begin
      if User_Roles[i] = '1' then Role_Administrator := True;
      i := i + 1;
    end;

    LogAction('Processing Token', False);
    LogAction(' -> Name: '+User_FirstName+' '+User_LastName, False);
    LogAction(' -> EMail: '+User_Email, False);
    LogAction(' -> Roles: '+User_Roles.CommaText, False);
    LogAction(' -> Administrator: '+BoolToStr(Role_Administrator,True), False);
    LogAction(' -> Expires: '+FormatDateTime('yyyy-MMM-dd hh:nn:ss',JWT_Expiry)+' UTC',False);
    LogAction(' -> Remaining: '+FormatDateTime('n"m "s"s"',(JWT_Expiry - TTimeZone.Local.ToUniversalTime(Now))),False);
    LogAction('Token Processed', False);

  end;

end;

procedure TMainForm.tmrJWTRenewalTimer(Sender: TObject);
var
  ResponseString: String;
  ActionLogSend: String;
begin
  tmrJWTRenewal.Enabled := False;

  ActionLogSend := ActionLogCurrent.Text;
  ActionLogCurrent.Text := '';

  // Renew JWT if there has been activity of some kind
  if ActivityDetected then
  begin
    ResponseString := await(JSONRequest('ISystemService.Renew',[App_Session, ActionLogSend]));
    if Copy(ResponseString,1,6) =  'Bearer' then
    begin
      LogAction('Login Renewed', False);
      ProcessJWT(ResponseString);
      ActivityDetected := False;
    end
    else
    begin
    // Otherwise perform an automatic logout of this session
      LogAction('Login NOT Renewed', False);
      Logout('NoRenewal');
    end
  end
  else
  begin
    Logout('Inactivity');
  end;
end;

procedure TMainForm.tmrJWTRenewalWarningTimer(Sender: TObject);
begin
  tmrJWTRenewalWarning.Enabled := False;
  if not(ActivityDetected)
  then Toast(DMIcons.Icon('Logout')+'Auto Logout','No activity has been detected.<br />Auto Logout in $S seconds.', 60000);
end;

procedure TMainForm.Toast(Header, Body: String; Timeout: Integer);
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
    toast.setAttribute('data-bs-delay', Timeout);
    toast.setAttribute('countdown',parseInt(2*Timeout/1000));

    // Create Toast Header
    var toasth = document.createElement('div');
    toasth.className = 'toast-header bg-danger text-white pb-3 position-relative';
    toasth.innerHTML = '<div style="position:absolute; border-radius: var(--custom-rounding); z-index: 2; display:block; top:37px; left:4px; width:98%; height:5px; background: var(--bs-danger);"></div>'+
                       '<div style="position:absolute; border-radius: var(--custom-rounding); z-index: 1; display:block; top:36px; left:3px; width:98%; height:7px; background: var(--bs-dark);"></div>'+
                       '<strong class="me-auto">'+Header+'</strong>'+
                       '<small class="text-light">just now</small>'+
                       '<button type="button" onclick="pas.UnitMain.MainForm.ActivityDetected = true;"; class="btn-close" data-bs-dismiss="toast" aria-label="Close"></button>';

    // Create Toast Body
    var toastb = document.createElement('div');
    toastb.className = 'toast-body';
    toastb.innerHTML = Body.replace('$S',parseInt(Timeout/1000));

    // Make Toast
    toast.appendChild(toasth);
    toast.appendChild(toastb);
    divToasts.appendChild(toast);

    // Add countdown timer
    const toastc = setInterval(function() {
      if (((Timeout == 60000) && (pas.UnitMain.MainForm.ActivityDetected == true)) || ((toast.getAttribute('countdown') | 0) <= 0)) {
        clearInterval(toastc);
        toast.remove();
      }
      else {
        toast.setAttribute('countdown',toast.getAttribute('countdown')-1);
        toast.lastElementChild.innerHTML = Body.replace('$S',parseInt(toast.getAttribute('countdown')/2));
        toast.firstElementChild.firstElementChild.style.setProperty('width', parseInt(98*toast.getAttribute('countdown')/(Timeout/500))+'%');
      }
    },500);

    // Show Toast
    var newtoast = new bootstrap.Toast(toast).show();

  end;
end;

procedure TMainForm.btnClearFormClick(Sender: TObject);
begin
  // More for testing purposes than anything else
  LogAction('CLICK: Clear Form', True);
  LoadForm('Clear','');
  Toast('divHost Component','Cleared.',15000);
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
  ErrorCode: String;
  ErrorMessage: String;
  Elapsed: TDateTime;
begin
  Elapsed := Now;
  Result := '';
  LogAction('Requested: '+Endpoint, False);

  await(XDataConnect);
  if (XdataConn.Connected) then
  begin
    try
      ClientConn := TXDataWebClient.Create(nil);
      ClientConn.Connection := XDataConn;
      Response := await(ClientConn.RawInvokeAsync(Endpoint, Params));

      Blob := Response.Result;
      asm Result = await Blob.text(); end;

    except on E: Exception do
      begin
        // Get the error message we created in XData
        asm {
          var ErrorDetail = JSON.parse( await E.FErrorResult.FResponse.$o.FXhr.response.text() );
          ErrorCode = ErrorDetail.error.code;
          ErrorMessage = ErrorDetail.error.message;
        } end;

        // Log the error, but leave out the URI (because it includes the password)
        LogAction('ERROR Request Exception Received From'+Endpoint, False);
        LogAction(' --> ['+E.ClassName+']', False);
        LogAction(' --> '+Copy(E.Message,1,Pos('Uri:',E.Message)-2), False);
        LogAction(' --> '+Copy(E.Message,Pos('Status code:',E.Message),16), False);
        LogAction(' --> '+ErrorCode, False);
        LogAction(' --> '+ErrorMessage, False);

        // Will tamp these down a bit once we get a better feel for the kinds of errors
        // that come up regularly.
        Toast(DMIcons.Icon('Bug_Menu')+'Unexpected Error',
          '[ '+E.ClassName+' ] '+Endpoint+'<br />'+
          Copy(E.Message,1,Pos('Uri:',E.Message)-2)+'<br />'+
          Copy(E.Message,Pos('Status code:',E.Message),16)+'<br />'+
          ErrorCode+'<br />'+
          ErrorMessage
        ,45000);

      end;
    end;
  end;

  LogAction('Responded: '+Endpoint+' ('+IntToStr(MillisecondsBetween(Now, Elapsed))+'ms)', False);
  PreventCompilerHint(Blob);
end;

procedure TMainForm.btnLoginFormClick(Sender: TObject);
begin
  LogAction('CLICK: Login Form', False);
  LoadForm('Login', DMIcons.Icon('Login'));
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
      asm NewJWT = await Blob.text(); end;

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
  if Pos('Bearer ', NewJWT) = 1 then
  begin
    // How long did it take?
    LogAction('Login Successful ('+IntToStr(MilliSecondsBetween(Now, ElapsedTime))+'ms)', False);

    // Do stuff with the JWT
    ProcessJWT(NewJWT);

    // All done
    Result := 'Success';
    LoggedIn := True;
  end
  else
  begin
    LoggedIn := False;
    Result := ErrorCode+' / '+ErrorMessage;
  end;

  PreventCompilerHint(Blob);
end;

procedure TMainForm.StopLinkerRemoval(P: Pointer);                          begin end;
procedure TMainForm.PreventCompilerHint(I: integer);              overload; begin end;
procedure TMainForm.PreventCompilerHint(J: JSValue);              overload; begin end;
procedure TMainForm.PreventCompilerHint(S: string);               overload; begin end;
procedure TMainForm.PreventCompilerHint(H: TJSHTMLElement);       overload; begin end;

end.
