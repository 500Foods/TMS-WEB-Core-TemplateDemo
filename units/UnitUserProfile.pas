unit UnitUserProfile;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, Vcl.Controls, Vcl.StdCtrls, WEBLib.StdCtrls;

type
  TUserProfileSubForm = class(TWebForm)
    WebEdit1: TWebEdit;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  UserProfileSubForm: TUserProfileSubForm;

implementation

{$R *.dfm}

end.