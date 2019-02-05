unit MainUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    btnPublishSend: TButton;
    edtTopic: TEdit;
    edtMessage: TEdit;
    edtHost: TEdit;
    edtPort: TEdit;
    Timer1: TTimer;
    procedure btnPublishSendClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    procedure Publish;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  MQTT;

procedure TForm1.btnPublishSendClick(Sender: TObject);
begin
  Publish;
end;


procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Publish;
end;


var
  Counter: int64;

procedure TForm1.Publish;
var
  mq: TMQTT;
begin
  mq := TMQTT.Create(edtHost.Text, StrToIntDef(edtPort.Text, 1883));
  try
    mq.EnableReceiveThread := False;  // just send, forget and disconnect, don't need rx thread
    if mq.Connect then
    begin
      mq.Publish(edtTopic.Text, edtMessage.Text + ' ' + IntToStr(Counter));
      Inc(Counter);
      mq.Disconnect;
    end;
  finally
    mq.Free;
  end;
end;


end.
