unit UnitLogin;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, Vcl.StdCtrls, WEBLib.StdCtrls, Vcl.Controls,
  WEBLib.ExtCtrls, WEBLib.Storage, System.DateUtils, WEBLib.WebCtrls;

type
  TLoginForm = class(TWebForm)
    editUsername: TWebEdit;
    editPassword: TWebEdit;
    btnLogin: TWebButton;
    labelLoginTitle: TWebLabel;
    checkRemember: TWebCheckBox;
    tmrLoginStart: TWebTimer;
    divLoginBox: TWebHTMLDiv;
    btnForgot: TWebButton;
    procedure WebFormCreate(Sender: TObject);
    procedure WebFormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure editUsernameChange(Sender: TObject);
    [async] procedure tmrLoginStartTimer(Sender: TObject);
    [async] procedure btnLoginClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  LoginForm: TLoginForm;

implementation

uses UnitMain, UnitIcons;

{$R *.dfm}

procedure TLoginForm.btnLoginClick(Sender: TObject);
var
  LoginCheck: String;
begin
  btnLogin.Caption := 'Authorizing...';

  LoginCheck := await(MainForm.XDataLogin(editUSername.Text, editPassword.Text));

  if LoginCheck = 'Success' then
  begin
    // Briefly show successful login message
    btnLogin.Caption := 'Login Successful';
    asm await sleep(500); end;

    // Remove Toasts
    asm divToasts.replaceChildren(); end;


    // Process Remembering
    if checkRemember.checked then
    begin
      MainForm.Remember := True;
      TWebLocalStorage.SetValue('Login.Username', editUsername.Text);
      TWebLocalStorage.SetValue('Login.JWT', MainForm.JWT);
      TWebLocalStorage.SetValue('Login.Expiry', FloatToStr(MainForm.JWT_Expiry));
    end
    else
    begin
      MainForm.Remember := False;
      TWebLocalStorage.RemoveKey('Login.Username');
      TWebLocalStorage.RemoveKey('Login.JWT');
      TWebLocalStorage.RemoveKey('Login.Expiry');
    end;

    // Load selected form
    if MainForm.Role_Administrator then
    begin
      MainForm.LoadForm('AdministratorForm', DMIcons.Icon('Administrator_Menu'));
    end;


  end
  else
  begin
    btnLogin.Caption := 'Please Try Again';
    LoginCheck := StringReplace(LoginCheck,': ',':<br />',[]);
    LoginCheck := StringReplace(LoginCheck,'. ','.<br />',[]);
    if Trim(LoginCheck) = '/'
    then LoginCheck := 'System Error / Server connection could not be established.';
    MainForm.Toast(DMIcons.Icon('Login')+Copy(LoginCheck,1,Pos('/',LoginCheck) -2),Copy(LoginCheck, Pos('/',LoginCheck)+2,Length(LoginCheck)),15000);
  end;
end;

procedure TLoginForm.WebFormCreate(Sender: TObject);
begin
  // Update Title
  labelLoginTitle.Caption := MainForm.Caption;

  // Hide the main login form
  divLoginBox.ElementHandle.style.setProperty('opacity','0');

  // Check for autologin
  tmrLoginStart.Enabled := True;
end;

procedure TLoginForm.WebFormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RETURN)
     and (trim(editUsername.Text) <> '')
     and (Trim(editPassword.Text) <> '')
  then btnLoginClick(Sender)
  else editUsernameChange(Sender);
end;

procedure TLoginForm.editUsernameChange(Sender: TObject);
begin
  if (Trim(editUsername.Text) = '') and (Trim(editPassword.Text) = '') then
  begin
    btnLogin.Enabled := False;
    btnForgot.Enabled := False;
  end
  else if (Trim(editUsername.Text) = '') then
  begin
    btnLogin.Enabled := False;
    btnForgot.Enabled := False;
  end
  else if (Trim(editPassword.Text) = '') then
  begin
    btnLogin.Enabled := False;
    btnForgot.Enabled := True;
  end
  else
  begin
    btnLogin.Enabled := True;
    btnForgot.Enabled := True;
  end;
end;

procedure TLoginForm.tmrLoginStartTimer(Sender: TObject);
var
  Remembered: String;
  LoggedIn: Boolean;
  CheckForm: String;
  CheckSubForm: String;
  CheckForms: Boolean;
  UserPass: Boolean;
begin
  tmrLoginStart.Enabled := False;
  LoggedIn := False;
  CheckForms := False;
  UserPass := False;

  // Check for valid state information
  CheckForm := TWebLocalStorage.GetValue('Login.CurrentForm');
  CheckSubform := TWebLocalStorage.GetValue('Login.CurrentSubForm');
  asm if (window.ValidForms.includes(CheckForm) && window.ValidSubForms.includes(CheckSubForm)) { CheckForms = true; } end;

  if (CheckForms = True) then
  begin
    if (CheckForm <> 'LoginForm') then
    begin
      if (TWebLocalStorage.GetValue('Login.Expiry') <> '') then
      begin
        MainForm.JWT_Expiry := StrToFloat(TWebLocalStorage.GetValue('Login.Expiry'));
        if MainForm.JWT_Expiry > (TTimeZone.Local.ToUniversalTime(Now) + 60/86400) then
        begin
          LoggedIn := True;
          MainForm.LogAction('AutoLogin - JWT Time Remaining: '+IntToStr(SecondsBetween(MainForm.JWT_Expiry, TTimeZone.Local.ToUniversalTime(Now)))+'s', False);
          MainForm.ProcessJWT(TWebLocalStorage.GetValue('Login.JWT'));
          MainForm.LoadForm(CheckForm, TWebLocalStorage.GetValue('Login.CurrentFormIcon') );
        end
        else
        begin
          MainForm.Logout('AutoLogin - JWT Expired');
        end;
      end;
    end;
  end;

  // Logging in Manually
  if not(LoggedIn) then
  begin
    // Update Icons
    document.getElementById('icon-username').innerHTML := DMIcons.Icon('Username');
    document.getElementById('icon-password').innerHTML := DMIcons.Icon('Password');

    // Check if we have remembered a Username
    Remembered :=  TWebLocalStorage.GetValue('Login.Username');
    if Remembered <> '' then
    begin
      editUsername.Text := Remembered;
      editUsernameChange(Sender);
      UserPass := True;
    end
    else
    begin
      editUsernameChange(Sender);
    end;

    // Add CAPS-LOCK highlighting
    asm
      editPassword.addEventListener("keyup", function(event) {
        if (event.getModifierState("CapsLock")) {
          editPassword.style.setProperty('background','yellow','important');
        }
        else {
          editPassword.style.setProperty('background','rgba(70, 90, 126, 0.4)','important');
        }
      });
      editPassword.addEventListener("focusout", function(event) {
          editPassword.style.setProperty('background','rgba(70, 90, 126, 0.4)','important');
      });
    end;

    // Show the login page
    MainForm.divHost.ElementHandle.style.setProperty('opacity','1');
    asm await sleep(500); end;

    // Show the login form
    divLoginBox.ElementHandle.style.setProperty('opacity','1');

    // Try to set the focus to one of these two elements
    if UserPass
    then editPassword.SetFocus
    else editUsername.SetFocus;


  end;

  MainForm.PreventCompilerHint(CheckSubForm);
end;

end.
