unit MQTT;

interface

uses
  System.SysUtils, System.Types, System.Classes, Vcl.ExtCtrls,
  Generics.Collections, System.SyncObjs, IdTCPClient,
  MQTTHeaders, MQTTReadThread;

type
  TMQTT = class
  private
    FClientID: string;
    FHostname: string;
    FPort: integer;
    FMessageID: integer;
    FIsConnected: boolean;
    FRecvThread: TMQTTReadThread;
    FCSSock: TCriticalSection;
    FWillMsg: string;
    FWillTopic: string;
    FUsername: string;
    FPassword: string;
    FSocket: TIdTCPClient;
    FKeepAliveTimer: TTimer;
    FEnableReceiveThread: boolean;
    function SendDisconnectMessage: boolean;
    procedure StartRecvThread(aMsg: TMQTTMessage);
    function SendConnectMessage(aMsg: TMQTTMessage): boolean;
    procedure StopAndFreeRecvThread;
    function CreateClientID: string;
  protected
    FConnAckEvent: TConnAckEvent;
    FPublishEvent: TPublishEvent;
    FPingRespEvent: TPingRespEvent;
    FPingReqEvent: TPingReqEvent;
    FSubAckEvent: TSubAckEvent;
    FUnSubAckEvent: TUnSubAckEvent;
    FPubAckEvent: TPubAckEvent;
    FPubRelEvent: TPubRelEvent;
    FPubRecEvent: TPubRecEvent;
    FPubCompEvent: TPubCompEvent;

    function WriteData(aData: TBytes): boolean;
    function hasWill: boolean;
    function getNextMessageId: integer;
    function CreateRecvThread(var aSocket: TIdTCPClient): boolean;
  protected
    // TMQTTMessage Factory Methods.
    function CreateConnectMessage: TMQTTMessage;
    function CreateDisconnectMessage: TMQTTMessage;
    function CreatePublishMessage: TMQTTMessage;
    function CreatePingReqMessage: TMQTTMessage;
    function CreateSubscribeMessage: TMQTTMessage;
    function CreateUnsubscribeMessage: TMQTTMessage;

    // Our Keep Alive Ping Timer Event
    procedure KeepAliveTimer_Event(sender: TObject);

    // Recv Thread Event Handling Procedures.
    procedure GotConnAck(Sender: TObject; aReturnCode: integer);
    procedure GotPingResp(Sender: TObject);
    procedure GotSubAck(Sender: TObject; aMessageID: integer; aGrantedQoS: Array of integer);
    procedure GotUnSubAck(Sender: TObject; aMessageID: integer);
    procedure GotPub(Sender: TObject; aTopic, aPayload: string);
    procedure GotPubAck(Sender: TObject; aMessageID: integer);
    procedure GotPubRec(Sender: TObject; aMessageID: integer);
    procedure GotPubRel(Sender: TObject; aMessageID: integer);
    procedure GotPubComp(Sender: TObject; aMessageID: integer);
  public
    constructor Create(aHostName: string; aPort: integer);
    destructor Destroy; override;
    function Connect: boolean;
    function Disconnect: boolean;
    function Publish(aTopic: string; aPayload: string): boolean; overload;
    function Publish(aTopic: string; aPayload: string; aRetain: boolean): boolean; overload;
    function Publish(aTopic: string; aPayload: string; aRetain: boolean; aQoS: integer): boolean; overload;
    function Subscribe(aTopic: string; aRequestQoS: integer): integer; overload;
    function Subscribe(aTopics: TDictionary<string, integer>): integer; overload;
    function Unsubscribe(aTopic: string): integer; overload ;
    function Unsubscribe(aTopics: TStringList): integer; overload;
    function PingReq: boolean;
    procedure ConnectPublishDisconnect(aHostName: string; aPort: integer; aTopic, aMsg: string);

    property WillTopic: string read FWillTopic write FWillTopic;
    property WillMsg: string read FWillMsg write FWillMsg;
    property Username: string read FUsername write FUsername;
    property Password: string read FPassword write FPassword;
    property ClientID : string read FClientID write FClientID;
    property IsConnected: boolean read FIsConnected;
    property EnableReceiveThread: boolean read FEnableReceiveThread write FEnableReceiveThread;

    // Event Handlers
    property OnConnAck: TConnAckEvent read FConnAckEvent write FConnAckEvent;
    property OnPublish: TPublishEvent read FPublishEvent write FPublishEvent;
    property OnPingResp: TPingRespEvent read FPingRespEvent write FPingRespEvent;
    property OnPingReq: TPingRespEvent read FPingRespEvent write FPingRespEvent;
    property OnSubAck: TSubAckEvent read FSubAckEvent write FSubAckEvent;
    property OnUnSubAck: TUnSubAckEvent read FUnSubAckEvent write FUnSubAckEvent;
    property OnPubAck: TUnSubAckEvent read FUnSubAckEvent write FUnSubAckEvent;
    property OnPubRec: TUnSubAckEvent read FUnSubAckEvent write FUnSubAckEvent;
    property OnPubRel: TUnSubAckEvent read FUnSubAckEvent write FUnSubAckEvent;
    property OnPubComp: TUnSubAckEvent read FUnSubAckEvent write FUnSubAckEvent;
  end;

implementation



constructor TMQTT.Create(aHostName: string; aPort: integer);
begin
  inherited Create;

  FHostname := aHostname;
  FPort := aPort;
  FMessageID := 1;
  FClientID := CreateClientID;
  FCSSock := TCriticalSection.Create;
  FEnableReceiveThread := True;

  // Create the timer responsible for pinging.
  FKeepAliveTimer := TTimer.Create(nil);
  FKeepAliveTimer.Enabled := False;
  FKeepAliveTimer.OnTimer := KeepAliveTimer_Event;
end;


destructor TMQTT.Destroy;
begin
  StopAndFreeRecvThread;
  Disconnect;
  FreeAndNil(FKeepAliveTimer);
  FreeAndNil(FCSSock);
  inherited;
end;


function TMQTT.CreateClientID: string;
var
  H,M,S,Ms: word;
begin
  // Create a 20 digit unique identifier for this client
  Randomize;  // Randomise and create a random client id.
  DecodeTime(Now, H, M, S, Ms);
  Result := 'TMQTT' + IntToHex(H,2) + IntToHex(M,2) + IntToHex(S,2) + IntToHex(Ms,3) +
    IntToStr(Random(1000000) + 1);
end;


procedure TMQTT.ConnectPublishDisconnect(aHostName: string; aPort: integer; aTopic, aMsg: string);
begin
  FHostName := aHostName;
  FPort := aPort;
  if Connect then
  begin
    EnableReceiveThread := False;
    Publish(aTopic, aMsg);
    Disconnect;
  end;
end;


procedure TMQTT.GotConnAck(Sender: TObject; aReturnCode: integer);
begin
  if Assigned(FConnAckEvent) then
    OnConnAck(Self, aReturnCode);
end;


function TMQTT.Connect: boolean;
var
  Msg: TMQTTMessage;
begin
  Result := False;
  Disconnect;
  FSocket := TIdTCPClient.Create(nil);   // Create socket and connect.

  try
    FSocket.Host := FHostname;
    FSocket.Port := FPort;
    FSocket.Connect;
    FIsConnected := True;
  except
    // If we encounter an exception upon connection then reraise it, free the socket
    // and reset our isConnected flag.
    on E: Exception do
    begin
      FSocket.Free;
    end;
  end;

  if FIsConnected then
  begin
    Msg := CreateConnectMessage;
    try
      Result := SendConnectMessage(Msg);
      if Result And FEnableReceiveThread then
        StartRecvThread(Msg);
    finally
      FreeAndNil(Msg);
    end;
  end;
end;


function TMQTT.SendConnectMessage(aMsg: TMQTTMessage): boolean;
begin
  aMsg.Payload.Contents.Add(FClientID);
  (aMsg.VariableHeader as TMQTTConnectVarHeader).WillFlag := ord(hasWill);
  if hasWill then
  begin
    aMsg.Payload.Contents.Add(FWillTopic);
    aMsg.Payload.Contents.Add(FWillMsg);
  end;

  if ((Length(FUsername) > 1) and (Length(FPassword) > 1)) then
  begin
    aMsg.Payload.Contents.Add(FUsername);
    aMsg.Payload.Contents.Add(FPassword);
  end;

  Result := WriteData(aMsg.ToBytes);
end;


function TMQTT.CreateConnectMessage: TMQTTMessage;
begin
  Result := TMQTTMessage.Create;
  Result.VariableHeader := TMQTTConnectVarHeader.Create;
  Result.Payload := TMQTTPayload.Create;
  Result.FixedHeader.MessageType := Ord(TMQTTMessageType.CONNECT);
  Result.FixedHeader.Retain := 0;
  Result.FixedHeader.QoSLevel := 0;
  Result.FixedHeader.Duplicate := 0;
end;


procedure TMQTT.StartRecvThread(aMsg: TMQTTMessage);
begin
  // Start our Receive thread.
  if CreateRecvThread(FSocket) then
  begin
    // Use the KeepAlive that we just sent to determine our ping timer.
    FKeepAliveTimer.Interval := (Round((aMsg.VariableHeader as TMQTTConnectVarHeader).KeepAlive * 0.80)) * 1000;
    FKeepAliveTimer.Enabled := True;
  end;
end;


function TMQTT.CreateRecvThread(var aSocket: TIdTCPClient): boolean;
begin
  try
    FRecvThread := TMQTTReadThread.Create(aSocket, FCSSock);

    { Todo: Assign Event Handlers here.   }
    FRecvThread.OnConnAck := Self.GotConnAck;
    FRecvThread.OnPublish := Self.GotPub;
    FRecvThread.OnPingResp := Self.GotPingResp;
    FRecvThread.OnSubAck := Self.GotSubAck;
    FRecvThread.OnPubAck := Self.GotPubAck;
    Result := True;
  except
    Result := False;
  end;
end;


function TMQTT.Disconnect: boolean;
begin
  Result := False;
  if IsConnected then
    begin
      FKeepAliveTimer.Enabled := False;
      SendDisconnectMessage;
      StopAndFreeRecvThread;
      try
        FSocket.Disconnect;
      except
        // Swallow here because there are a few circumstances where Indy
        // throws exception on shutdown, but we don't care about the reason
      end;
      FIsConnected := False;
      FreeAndNil(FSocket);
    end;
end;


procedure TMQTT.StopAndFreeRecvThread;
begin
  if Assigned(FRecvThread) then
  begin
    FRecvThread.Terminate;
    FRecvThread.WaitFor;
    FreeAndNil(FRecvThread);
  end;
end;


function TMQTT.SendDisconnectMessage: boolean;
var
  Msg: TMQTTMessage;
begin
  Msg := CreateDisconnectMessage;
  try
    Result := WriteData(Msg.ToBytes);
  finally
    Msg.Free;
  end;
end;


function TMQTT.CreateDisconnectMessage: TMQTTMessage;
begin
  Result := TMQTTMessage.Create;
  Result.FixedHeader.MessageType := Ord(TMQTTMessageType.DISCONNECT);
end;


function TMQTT.getNextMessageId: integer;
begin
  // If we've reached the upper bounds of our 16 bit unsigned message Id then
  // start again. The spec says it typically does but is not required to Inc(MsgId,1).
  if (FMessageID = 65535) then
    FMessageID := 1;

  // Return our current message Id
  Result := FMessageID;
  Inc(FMessageID);   // Increment message Id
end;


function TMQTT.hasWill: boolean;
begin
  if ((Length(FWillTopic) < 1) and (Length(FWillMsg) < 1)) then
    Result := False
  else
    Result := True;
end;


procedure TMQTT.KeepAliveTimer_Event(sender: TObject);
begin
  if Self.IsConnected then
    PingReq;
end;


function TMQTT.PingReq: boolean;
var
  Msg: TMQTTMessage;
begin
  Result := False;
  if IsConnected then
  begin
    Msg := CreatePingReqMessage;
    if WriteData(Msg.ToBytes) then
      Result := True
    else
      Result := False;
    Msg.Free;
  end;
end;


function TMQTT.CreatePingReqMessage: TMQTTMessage;
begin
  Result := TMQTTMessage.Create;
  Result.FixedHeader.MessageType := Ord(TMQTTMessageType.PINGREQ);
end;


procedure TMQTT.GotPingResp(Sender: TObject);
begin
  if Assigned(FPingRespEvent) then
    OnPingResp(Self);
end;


function TMQTT.Publish(aTopic, aPayload: string; aRetain: boolean): boolean;
begin
  Result := Publish(aTopic, aPayload, aRetain, 0);
end;


function TMQTT.Publish(aTopic, aPayload: string): boolean;
begin
  Result := Publish(aTopic, aPayload, False, 0);
end;


function TMQTT.Publish(aTopic, aPayload: string; aRetain: boolean; aQoS: integer): boolean;
var
  Msg: TMQTTMessage;
begin
  Result := False;
  if ((aQoS > -1) and (aQoS <= 3)) then
  begin
    if IsConnected then
    begin
       Msg := CreatePublishMessage;
       try
         Msg.FixedHeader.QoSLevel := aQoS;
         (Msg.VariableHeader as TMQTTPublishVarHeader).QoSLevel := aQoS;
         (Msg.VariableHeader as TMQTTPublishVarHeader).Topic := aTopic;
         if (aQoS > 0) then
           (Msg.VariableHeader as TMQTTPublishVarHeader).MessageID := getNextMessageId;
         Msg.Payload.Contents.Add(aPayload);
         Msg.Payload.PublishMessage := True;
         Result := WriteData(Msg.ToBytes);
       finally
         Msg.Free;
       end;
    end;
  end
  else
    raise EInvalidOp.Create('QoS level can only be equal to or between 0 and 3.');
end;


function TMQTT.CreatePublishMessage: TMQTTMessage;
begin
  Result := TMQTTMessage.Create;
  Result.FixedHeader.MessageType := Ord(TMQTTMessageType.PUBLISH);
  Result.VariableHeader := TMQTTPublishVarHeader.Create;
  Result.Payload := TMQTTPayload.Create;
end;


procedure TMQTT.GotPubRec(Sender: TObject; aMessageID: integer);
begin
  if Assigned(FPubRecEvent) then
    OnPubRec(Self, aMessageID);
end;


procedure TMQTT.GotPubRel(Sender: TObject; aMessageID: integer);
begin
  if Assigned(FPubRelEvent) then
    OnPubRel(Self, aMessageID);
end;


function TMQTT.Subscribe(aTopic: string; aRequestQoS: integer): integer;
var
  dTopics: TDictionary<string, integer>;
begin
  dTopics := TDictionary<string, integer>.Create;
  dTopics.Add(aTopic, aRequestQoS);
  Result := Subscribe(dTopics);
  dTopics.Free;
end;


procedure TMQTT.GotSubAck(Sender: TObject; aMessageID: integer;
  aGrantedQoS: array of integer);
begin
  if Assigned(FSubAckEvent) then OnSubAck(Self, aMessageID, aGrantedQoS);
end;


function TMQTT.Subscribe(aTopics: TDictionary<string, integer>): integer;
var
  Msg: TMQTTMessage;
  MsgId: Integer;
  sTopic: string;
  data: TBytes;
begin
  Result := -1;
  if IsConnected then
    begin
      Msg := CreateSubscribeMessage;
      try
        MsgId := getNextMessageId;
        (Msg.VariableHeader as TMQTTSubscribeVarHeader).MessageID := MsgId;

        for sTopic in aTopics.Keys do
        begin
          Msg.Payload.Contents.Add(sTopic);
          Msg.Payload.Contents.Add(IntToStr(aTopics.Items[sTopic]))
        end;
        // the subscribe message contains integer literals not encoded as UTF8Strings.
        Msg.Payload.ContainsIntLiterals := True;

        data := Msg.ToBytes;
        if WriteData(data) then
          Result := MsgId;
      finally
        Msg.Free;
      end;
    end;
end;


function TMQTT.CreateSubscribeMessage: TMQTTMessage;
begin
  Result := TMQTTMessage.Create;
  Result.FixedHeader.MessageType := Ord(TMQTTMessageType.SUBSCRIBE);
  Result.FixedHeader.QoSLevel := 0;
  Result.VariableHeader := TMQTTSubscribeVarHeader.Create;
  Result.Payload := TMQTTPayload.Create;
end;


function TMQTT.Unsubscribe(aTopic: string): integer;
var
  slTopics: TStringList;
begin
  slTopics := TStringList.Create;
  slTopics.Add(aTopic);
  Result := Unsubscribe(slTopics);
  slTopics.Free;
end;


procedure TMQTT.GotUnSubAck(Sender: TObject; aMessageID: integer);
begin
  if Assigned(FUnSubAckEvent) then
    OnUnSubAck(Self, aMessageID);
end;


function TMQTT.Unsubscribe(aTopics: TStringList): integer;
var
  Msg: TMQTTMessage;
  MsgId: integer;
begin
  Result := -1;
  if IsConnected then
    begin
      Msg := CreateUnsubscribeMessage;
      MsgId := getNextMessageId;
      (Msg.VariableHeader as TMQTTSubscribeVarHeader).MessageID := MsgId;
      Msg.Payload.Contents.AddStrings(aTopics);
      if WriteData(Msg.ToBytes) then
        Result := MsgId;
      Msg.Free;
    end;
end;


function TMQTT.CreateUnsubscribeMessage: TMQTTMessage;
begin
  Result := TMQTTMessage.Create;
  Result.FixedHeader.MessageType := Ord(TMQTTMessageType.UNSUBSCRIBE);
  Result.FixedHeader.QoSLevel := 1;
  Result.VariableHeader := TMQTTUnsubscribeVarHeader.Create;
  Result.Payload := TMQTTPayload.Create;
end;


function TMQTT.WriteData(aData: TBytes): boolean;
var
  dataStream: TMemoryStream;
begin
  Result := False;
  if IsConnected then
  begin
    try
      dataStream := TMemoryStream.Create;
      try
        dataStream.Position := 0;
        dataStream.WriteBuffer(aData, Length(aData));
        FSocket.IOHandler.Write(dataStream);
      finally
        dataStream.Free;
      end;
      Result := True;
      FIsConnected := True;
    except
      Result := False;
      FIsConnected := False;
    end;
  end;
end;


procedure TMQTT.GotPub(Sender: TObject; aTopic, aPayload: string);
begin
  if Assigned(FPublishEvent) then
    OnPublish(Self, aTopic, aPayload);
end;


procedure TMQTT.GotPubAck(Sender: TObject; aMessageID: integer);
begin
  if Assigned(FPubAckEvent) then
    OnPubAck(Self, aMessageID);
end;


procedure TMQTT.GotPubComp(Sender: TObject; aMessageID: integer);
begin
  if Assigned(FPubCompEvent) then
    OnPubComp(Self, aMessageID);
end;

end.

