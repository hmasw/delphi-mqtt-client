object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 336
  ClientWidth = 527
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btnPublishSend: TButton
    Left = 168
    Top = 40
    Width = 161
    Height = 73
    Caption = 'Publish (Send)'
    TabOrder = 0
    OnClick = btnPublishSendClick
  end
  object edtTopic: TEdit
    Left = 56
    Top = 184
    Width = 393
    Height = 21
    TabOrder = 1
    Text = 'Topic'
  end
  object edtMessage: TEdit
    Left = 56
    Top = 232
    Width = 393
    Height = 21
    TabOrder = 2
    Text = 'Message'
  end
  object edtHost: TEdit
    Left = 56
    Top = 136
    Width = 298
    Height = 21
    TabOrder = 3
    Text = '127.0.0.1'
  end
  object edtPort: TEdit
    Left = 360
    Top = 136
    Width = 89
    Height = 21
    TabOrder = 4
    Text = '1883'
  end
  object Timer1: TTimer
    Interval = 1
    OnTimer = Timer1Timer
    Left = 376
    Top = 80
  end
end
