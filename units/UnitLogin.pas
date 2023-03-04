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
    procedure WebFormCreate(Sender: TObject);
    procedure WebFormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure tmrLoginStartTimer(Sender: TObject);
    [async] procedure btnLoginClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  LoginForm: TLoginForm;

implementation

uses UnitMain;

{$R *.dfm}

procedure TLoginForm.btnLoginClick(Sender: TObject);
var
  LoginCheck: String;
begin
  btnLogin.Caption := 'Authorizing...';

  LoginCheck := await(MainForm.XDataLogin(editUSername.Text, editPassword.Text));
//  console.log(LoginCheck);

  if LoginCheck = 'Success' then
  begin
    // Briefly show successful login message
    btnLogin.Caption := 'Login Successful';
    asm
      async function sleep(msecs) {return new Promise((resolve) =>setTimeout(resolve, msecs));}
      await sleep(500);
    end;

    if checkRemember.checked then
    begin
      TWebLocalStorage.SetValue('Login.Username', editUsername.Text);
      TWebLocalStorage.SetValue('Login.JWT', MainForm.JWT);
      TWebLocalStorage.SetValue('Login.Expiry', FloatToStr(MainForm.JWT_Expiry));
    end
    else
    begin
      TWebLocalStorage.RemoveKey('Login.Username');
      TWebLocalStorage.RemoveKey('Login.JWT');
      TWebLocalStorage.RemoveKey('Login.Expiry');
    end;

    // Remove Toasts
    asm
      divToasts.replaceChildren();
    end;

    // Load selected form
    if MainForm.Role_Administrator then
    begin
      MainForm.LoadForm('Administrator');
    end
    else if MainForm.Role_HR then
    begin
      MainForm.LoadForm('HR')
    end
    else if MainForm.Role_Sales then
    begin
      MainForm.LoadForm('Sales');
    end;

  end
  else
  begin
    btnLogin.Caption := 'Please Try Again';
    LoginCheck := StringReplace(LoginCheck,': ',':<br />',[]);
    LoginCheck := StringReplace(LoginCheck,'. ','.<br />',[]);
    if Trim(LoginCheck) = '/'
    then LoginCheck := 'System Error / Server connection could not be established.';
    MainForm.Toast(Copy(LoginCheck,1,Pos('/',LoginCheck) -2),Copy(LoginCheck, Pos('/',LoginCheck)+2,Length(LoginCheck)),15000);
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

  if (Key = VK_RETURN) then btnLoginClick(Sender);
  
end;

procedure TLoginForm.tmrLoginStartTimer(Sender: TObject);
var
  Remembered: String;
  LoggedIn: Boolean;
begin
  tmrLoginStart.Enabled := False;
  LoggedIn := False;

  // Check if we have remembered a JWT
  if TWebLocalStorage.GetValue('Login.CurrentForm') <> 'Login' then
  begin
    if TWebLocalStorage.GetValue('Login.Expiry') <> '' then
    begin
      MainForm.JWT_Expiry := StrToFloat(TWebLocalStorage.GetValue('Login.Expiry'));
      if MainForm.JWT_Expiry > (TTimeZone.Local.ToUniversalTime(Now) + 60/86400) then
      begin
        MainForm.LogAction('AutoLogin / JWT Time Remaining: '+IntToStr(SecondsBetween(MainForm.JWT_Expiry, TTimeZone.Local.ToUniversalTime(Now)))+'s', False);
//        console.log('AutoLogin/JWT Time Remaining: '+IntToStr(SecondsBetween(MainForm.JWT_Expiry, TTimeZone.Local.ToUniversalTime(Now)))+'s');

        LoggedIn := True;
        MainForm.ProcessJWT(TWebLocalStorage.GetValue('Login.JWT'));
        MainForm.LoadForm(TWebLocalStorage.GetValue('Login.CurrentForm'));
      end
      else
      begin
        MainForm.Logout('JWT Expired');
      end;
    end;
  end;

  // Logging in Manually
  if not(LoggedIn) then
  begin
    // Update Icons
    asm
     const IconSet = pas.UnitIcons.DMIcons;
      document.getElementById('ticon-username').innerHTML = IconSet.Username;
      document.getElementById('ticon-password').innerHTML = IconSet.Password;
    end;

    // Check if we have remembered a Username
    Remembered :=  TWebLocalStorage.GetValue('Login.Username');
    if Remembered <> '' then
    begin
      editUsername.Text := Remembered;
      editPassword.SetFocus;
    end
    else
    begin
      editUSername.SetFocus;
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

    // Show the login form
    divLoginBox.ElementHandle.style.setProperty('opacity','1');
  end;

end;

end.
