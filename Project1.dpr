program Project1;

uses
  Vcl.Forms,
  WEBLib.Forms,
  Unit1 in 'Unit1.pas' {Form1: TWebForm} {*.html};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
