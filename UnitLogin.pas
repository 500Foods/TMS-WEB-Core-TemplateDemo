unit UnitLogin;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, Vcl.StdCtrls, WEBLib.StdCtrls, Vcl.Controls,
  WEBLib.ExtCtrls;

type
  TLoginForm = class(TWebForm)
    editUsername: TWebEdit;
    editPassword: TWebEdit;
    btnLogin: TWebButton;
    msgLogin: TWebLabel;
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
begin
  btnLogin.Caption := 'Authorizing...';
  asm
    msgLogin.innerHTML = 'Please Wait...';
  end;
  if await(MainForm.XDataLogin(editUSername.Text, editPassword.Text)) then
  begin
    btnLogin.Caption := 'Successful';
    asm
      msgLogin.innerHTML = '';
    end;
    MainForm.LogAction('Selecting First Page');
  end
  else
  begin
    btnLogin.Caption := 'Please Try Again';
  end;
end;

end.
