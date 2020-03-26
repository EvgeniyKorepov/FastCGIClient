unit UnitFastCGI;
(*
http://www.mit.edu/~yandros/doc/specs/fcgi-spec.html
*)
interface

uses
  System.SysUtils, System.Types, System.Classes, System.Variants,
  System.Generics.Collections,
  System.Hash,
  System.DateUtils,
  System.Net.URLClient,
  System.Net.Mime,
  IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdGlobal, IdException;

Const
  FCGI_LISTENSOCK_FILENO  = 0;

  FCGI_HEADER_LEN = 8;
  FCGI_VERSION_1  = 1;
  FCGI_BEGIN_REQUEST = 1;
  FCGI_ABORT_REQUEST = 2;
  FCGI_END_REQUEST = 3;
  FCGI_PARAMS = 4;
  FCGI_STDIN = 5;
  FCGI_STDOUT = 6;
  FCGI_STDERR = 7;
  FCGI_DATA = 8;
  FCGI_GET_VALUES = 9;
  FCGI_GET_VALUES_RESULT = 10;
  FCGI_UNKNOWN_TYPE = 11;
  FCGI_MAXTYPE = (FCGI_UNKNOWN_TYPE);
  FCGI_NULL_REQUEST_ID = 0;

  FCGI_TYPE_BEGIN_REQUEST     = 1;
  FCGI_TYPE_ABORT_REQUEST     = 2;
  FCGI_TYPE_END_REQUEST       = 3;
  FCGI_TYPE_PARAMS            = 4;
  FCGI_TYPE_STDIN             = 5;
  FCGI_TYPE_STDOUT            = 6;
  FCGI_TYPE_STDERR            = 7;
  FCGI_TYPE_DATA              = 8;
  FCGI_TYPE_GET_VALUES        = 9;
  FCGI_TYPE_GET_VALUES_RESULT = 10;
  FCGI_TYPE_MAX               = 11;

  FCGI_ROLE_KEEP_CONNECTION = 1;
  FCGI_ROLE_RESPONDER       = 1;
  FCGI_ROLE_AUTHORIZER      = 2;
  FCGI_ROLE_FILTER          = 3;

  FCGI_REQUEST_COMPLETE = 0;
  FCGI_CANT_MPX_CONN    = 1;
  FCGI_OVERLOADED       = 2;
  FCGI_UNKNOWN_ROLE     = 3;
  FCGI_MAX_CONNS        = 'FCGI_MAX_CONNS';
  FCGI_MAX_REQS         = 'FCGI_MAX_REQS';
  FCGI_MPXS_CONNS       = 'FCGI_MPXS_CONNS';
  FCGI_MAX_LENGTH       = $FFFF;
  WIN32_OPEN_MAX        = 128;
  FCGI_HEADER_LENGTH    = 8;
  PARAMS_BUFF_MAX_LEN   = 32 * 1024;

type

  TEnvironment = TDictionary<String, String>;

  TResponceHeaders = TDictionary<String, String>;

  TFastCGI = class(TObject)
  private
  var
    FTCPClient: TIdTCPClient;
    FHost : String;
    FPort : Integer;

    FConnectTimeout : Integer;
    FReadTimeout : Integer;

    FIsConnected : Boolean;

    FStreamRead : TMemoryStream;
    FStreamWrite : TMemoryStream;
    FBinaryReader : TBinaryReader;
    FBinaryWriter : TBinaryWriter;

    FRequestID  : Word;
    FKeepAlive : Boolean;
    FEnvironment : TEnvironment;

    FReceivedRecordType : Byte;
    FResponceHeaders : TResponceHeaders;
    FStatusCode : Integer;
    FStatusText : String;

    FRawContent : String;
    FLastError : String;

    function SwapEndiannessOfBytes2(const Value: Word) : Word;
    function SwapEndiannessOfBytes4(const Value: Integer) : Integer;

    function WriteStream : Boolean;
    function ReadStream : Boolean;

    procedure WriteHeader(const AType : Byte; const AContentLength : Word; out APaddingLength : Byte);
    procedure WriteBeginRequest();
    procedure WriteEnvironment();

    function ReadHeader(out AContentLength : Word; out APaddingLength : Byte) : Boolean;
    function ReadContent(const AContentLength : Word; const APaddingLength : Byte; out AContent : String) : Boolean;

    function ParseStream(out AContent : String) : Boolean;
    function ParseContent(var AContent : String) : Boolean;

    procedure LogStream(const AStream : TMemoryStream);
  protected
  public
    constructor Create(const AHost : String; const APort : Word; const AConnectTimeout : Integer = 1000; const AReadTimeout : Integer = 1000);
    destructor Destroy; override;
    function ConnectToFastCGI : Boolean;
    function ReconnectToFastCGI() : Boolean;
    function ConnectedToFastCGI() : Boolean;
    function Get(const AScriptFilename, AQueryString : String; out AContent : String) : Boolean;

    property KeepAlive : Boolean read FKeepAlive write FKeepAlive;
    property StatusCode : Integer read FStatusCode;
    property StatusText : String read FStatusText;
    property LastError : String read FLastError;

    property ResponceHeaders : TResponceHeaders read FResponceHeaders;
    property RawContent : String read FRawContent;
  end;

implementation

constructor TFastCGI.Create(const AHost : String; const APort : Word; const AConnectTimeout : Integer = 1000; const AReadTimeout : Integer = 1000);
begin
  inherited Create;

  FHost := AHost;
  FPort := APort;
  FConnectTimeout := AConnectTimeout;
  FReadTimeout := AReadTimeout;
  FIsConnected := False;

  FRequestID := 1;
  FStatusCode := 0;
  FKeepAlive := True;

  FEnvironment := TEnvironment.Create();

  FStreamRead := TMemoryStream.Create;
  FStreamWrite := TMemoryStream.Create;

  FBinaryReader := TBinaryReader.Create(FStreamRead);
  FBinaryWriter := TBinaryWriter.Create(FStreamWrite, TEncoding.UTF8);

  FResponceHeaders := TResponceHeaders.Create();

  FTCPClient:=TIdTCPClient.Create;
  FTCPClient.Host := FHost;
  FTCPClient.Port := FPort;
  FTCPClient.ConnectTimeout := FConnectTimeout;
  FTCPClient.ReadTimeout := FReadTimeout;
end;

destructor TFastCGI.Destroy;
begin
  if Assigned(FTCPClient) then
  begin
    FTCPClient.Disconnect;
    if FTCPClient.IOHandler <> nil then
      FTCPClient.IOHandler.InputBuffer.Clear;
    FTCPClient.Free;
  end;


  if Assigned(FBinaryReader) then
    FBinaryReader.Free;
  if Assigned(FBinaryWriter) then
    FBinaryWriter.Free;

  if Assigned(FStreamRead) then
  begin
    FStreamRead.Clear;
    FStreamRead.Free;
  end;
  if Assigned(FStreamWrite) then
  begin
    FStreamWrite.Clear;
    FStreamWrite.Free;
  end;

  if Assigned(FResponceHeaders) then
  begin
    FResponceHeaders.Clear;
    FResponceHeaders.Free;
  end;

  if Assigned(FEnvironment) then
  begin
    FEnvironment.Clear;
    FEnvironment.Free;
  end;

  inherited Destroy;
end;

function TFastCGI.ConnectToFastCGI() : Boolean;
begin
  Result := False;
{
  if FIsConnected then
  begin
    Result := True;
    exit;
  end;
}
  try
    FTCPClient.Connect();
    FIsConnected := True;
    Result := True;
  except
    on E : Exception do
    begin
      FStatusCode := -1;
      FStatusText := 'ERROR';
      FLastError := 'TFastCGI.Connect : ' + E.ClassName + ': ' + E.Message;
    end;
  end;
end;

function TFastCGI.ReconnectToFastCGI() : Boolean;
var ACounter : Integer;
begin
  Result := False;
  if Assigned(FTCPClient.IOHandler) then
    FTCPClient.IOHandler.InputBuffer.Clear;

  for ACounter := 1 to 5 do
  begin
    if ConnectToFastCGI() then
      break;
    Sleep(1000);
    if ACounter = 5 then
      Exit;
  end;
  Result := True;
end;

function TFastCGI.ConnectedToFastCGI() : Boolean;
begin
  Result := False;
  if not Assigned(FTCPClient.IOHandler) then
    exit;
  try
    FTCPClient.IOHandler.CheckForDisconnect(True);
  except
    on E : Exception do
    begin
      FStatusCode := -1;
      FStatusText := 'ERROR';
      FLastError := 'TFastCGI.Connect : ' + E.ClassName + ': ' + E.Message;
      FIsConnected := False;
      exit;
    end;
  end;
  FIsConnected := True;
  Result := True;
end;

procedure TFastCGI.WriteHeader(const AType : Byte; const AContentLength : Word; out APaddingLength : Byte);
var AProtocolVersion : Byte;
    AReserved : Byte;
begin
  AProtocolVersion := FCGI_VERSION_1;

  APaddingLength := 8 - (AContentLength mod 8);

  APaddingLength := 0;
  AReserved := 0;

  FBinaryWriter.Write(AProtocolVersion);
  FBinaryWriter.Write(AType);
  FBinaryWriter.Write(SwapEndiannessOfBytes2(FRequestID));
  FBinaryWriter.Write(SwapEndiannessOfBytes2(AContentLength));
  FBinaryWriter.Write(APaddingLength);
  FBinaryWriter.Write(AReserved);
end;

function TFastCGI.ReadHeader(out AContentLength : Word; out APaddingLength : Byte) : Boolean;
var AProtocolVersion : Byte;
    AReserved : Byte;
begin
  Result := False;

  try
    AProtocolVersion := FBinaryReader.ReadByte();
    FReceivedRecordType := FBinaryReader.ReadByte();
    FRequestID := SwapEndiannessOfBytes2(FBinaryReader.ReadWord());
    AContentLength := SwapEndiannessOfBytes2(FBinaryReader.ReadWord());
    APaddingLength := FBinaryReader.ReadByte();
    AReserved :=FBinaryReader.ReadByte();
  except
    on E : Exception do
    begin
      FStatusCode := -1;
      FStatusText := E.Message;
      FLastError := 'TFastCGI.ReadHeader : ' + E.Message;
      exit;
    end;
  end;

  Result := True;
end;

procedure TFastCGI.WriteBeginRequest();
var ARole : Word;
    AFlags : Byte;
    AReserved : Byte;
    APaddingLength : Byte;
begin
  WriteHeader(FCGI_BEGIN_REQUEST, 8, APaddingLength);
  ARole := FCGI_ROLE_RESPONDER;
  FBinaryWriter.Write(SwapEndiannessOfBytes2(ARole));

  if FKeepAlive then
    AFlags := 1
  else
    AFlags := 0;
  FBinaryWriter.Write(AFlags);

  AReserved := 0;
  FBinaryWriter.Write(AReserved);
  FBinaryWriter.Write(AReserved);
  FBinaryWriter.Write(AReserved);
  FBinaryWriter.Write(AReserved);
  FBinaryWriter.Write(AReserved);
end;

procedure TFastCGI.WriteEnvironment();
Var AKey, AValue : String;
//    AKeyLength, AValueLength : Cardinal;
    AKeyLength, AValueLength : Integer;
//    AKeyLength, AValueLength : Byte;
    AContentLength : Integer;
    APaddingLength, APaddingByte : Byte;
    I : Integer;
begin
  AContentLength := 0;
  for AKey in FEnvironment.Keys do
  begin
    AValue := FEnvironment.Items[AKey];

    AKeyLength := TEncoding.UTF8.GetByteCount(AKey);
    AValueLength := TEncoding.UTF8.GetByteCount(AValue);

    Inc(AContentLength, AKeyLength.Size);
    Inc(AContentLength, AValueLength.Size);
    Inc(AContentLength, AKeyLength);
    Inc(AContentLength, AValueLength);
  end;

  WriteHeader(FCGI_PARAMS, AContentLength, APaddingLength);

  for AKey in FEnvironment.Keys do
  begin
    AValue := FEnvironment.Items[AKey];
    AKeyLength := TEncoding.UTF8.GetByteCount(AKey);
    AValueLength := TEncoding.UTF8.GetByteCount(AValue);

    if AKeyLength.Size > 1 then
    begin
      AKeyLength := AKeyLength or (1 shl 31);
      AKeyLength := SwapEndiannessOfBytes4(AKeyLength);
    end;
    if AValueLength.Size > 1 then
    begin
      AValueLength := AValueLength or (1 shl 31);
      AValueLength := SwapEndiannessOfBytes4(AValueLength);
    end;

    FBinaryWriter.Write(AKeyLength);
    FBinaryWriter.Write(AValueLength);

    FBinaryWriter.Write(TEncoding.UTF8.GetBytes(AKey), 0, TEncoding.UTF8.GetByteCount(AKey));
    FBinaryWriter.Write(TEncoding.UTF8.GetBytes(AValue), 0, TEncoding.UTF8.GetByteCount(AValue));

  end;

  APaddingByte := 0;
  for I := 1 to APaddingLength do
    FBinaryWriter.Write(APaddingByte);

  WriteHeader(FCGI_PARAMS, 0, APaddingLength);
end;

function TFastCGI.Get(const AScriptFilename, AQueryString : String; out AContent : String) : Boolean;
begin
  Result := False;
  AContent := '';

  FEnvironment.Clear;
  FStreamWrite.Clear;
  FStreamRead.Clear;

{
  if FRequestID = High(FRequestID) then
    FRequestID := 0
  else
    Inc(FRequestID);
}

  FEnvironment.Add('REQUEST_METHOD', 'GET');
  FEnvironment.Add('SCRIPT_FILENAME', AScriptFilename);
  FEnvironment.Add('QUERY_STRING', AQueryString);

  WriteBeginRequest();
  WriteEnvironment();

  if not ConnectedToFastCGI() then
    if Not ReconnectToFastCGI then
      exit;

  if not WriteStream() then
    exit;

  if not ReadStream() then
    exit;

  if not ParseStream(AContent) then
    exit;

  Result := True;
end;

function TFastCGI.WriteStream : Boolean;
begin
  Result := false;
  FStreamWrite.Position:=0;
  LogStream(FStreamWrite);
  FStreamWrite.Position:=0;
  try
    FTCPClient.IOHandler.Write(FStreamWrite);
  except
    on E : EIdException do
    begin
      FStatusCode := -1;
      FStatusText := E.Message;
      FLastError := 'TFastCGI.WriteStream : ' + E.Message;
      FTCPClient.Disconnect;
      exit;
    end;
  end;
  Result := True;
end;

function TFastCGI.ReadStream : Boolean;
const ConstCounterMax = 5;
var ASize : integer;
    S : String;
begin
  Result := False;
  FStreamRead.Clear;

  if not Assigned(FTCPClient.IOHandler) then
    S := '';


  while FTCPClient.IOHandler.InputBufferIsEmpty do
  begin
    if not ConnectedToFastCGI() then
      exit;
    FTCPClient.IOHandler.CheckForDataOnSource(1000);
  end;

  ASize:=FTCPClient.IOHandler.InputBuffer.Size;
  if ASize = 0 then
    exit;
  try
    FTCPClient.IOHandler.ReadStream(FStreamRead, ASize);
    if FStreamRead.Size = 0 then
      exit;
  except
    on E : EIdException do
    begin
      FStatusCode := -1;
      FStatusText := E.Message;
      FLastError := 'TFastCGI.ReadStream.ReadStream : ' + E.Message;
      exit;
    end;
  end;
  LogStream(FStreamRead);
  Result := True;
end;

function TFastCGI.ReadContent(const AContentLength : Word; const APaddingLength : Byte; out AContent : String) : Boolean;
begin
  Result := False;

  try
    FRawContent := '';
    if AContentLength = 8 then
      AContent := TEncoding.ASCII.GetString(FBinaryReader.ReadBytes(AContentLength))
    else
      AContent := TEncoding.UTF8.GetString(FBinaryReader.ReadBytes(AContentLength));
//    FRawContent := AContent;
    FBinaryReader.ReadBytes(APaddingLength)
  except
    on E : Exception do
    begin
//      FStatusCode := -1;
//      FStatusText := E.Message;
//      FLastError := 'TFastCGI.ReadContent : ' + E.Message;
      exit;
    end;
  end;

  Result := True;
end;


function TFastCGI.ParseStream(out AContent : String) : Boolean;
var AContentLength : Word;
    APaddingLength : Byte;
    AContentEnd : String;
    ARawContent : String;
begin
  Result := False;
  FStreamRead.Size;
  FStreamRead.Position := 0;

  if Not ReadHeader(AContentLength, APaddingLength) then
    exit;

  if Not ReadContent(AContentLength, APaddingLength, AContent) then
    exit;

  case FReceivedRecordType of
    FCGI_TYPE_STDERR :
    begin
      if Not ReadHeader(AContentLength, APaddingLength) then
        exit;

      if Not ReadContent(AContentLength, APaddingLength, AContent) then
        exit;

      if Not ParseContent(AContent) then
        exit;

      exit;
    end;
    FCGI_TYPE_STDOUT :
    begin
      if Not ParseContent(AContent) then
        exit;
      ARawContent := FRawContent;

      if Not ReadHeader(AContentLength, APaddingLength) then
        exit;

//      if Not ReadContent(AContentLength, APaddingLength, AContentEnd) then
//        exit;
//      FRawContent := ARawContent;
      ReadContent(AContentLength, APaddingLength, AContentEnd);

      Result := True;
    end;
    else
    begin
      if Not ParseContent(AContent) then
        exit;
      ARawContent := FRawContent;

      if Not ReadHeader(AContentLength, APaddingLength) then
        exit;

      if Not ReadContent(AContentLength, APaddingLength, AContentEnd) then
        exit;
      FRawContent := ARawContent;

      Result := True;
    end;
  end;

end;

function TFastCGI.ParseContent(var AContent : String) : Boolean;
var AContentArray : TArray<String>;
    I : Integer;
    AIsHeader : Boolean;
    APair : TArray<String>;
    AStatusCodeText : String;
begin
  Result := False;

  AContentArray := AContent.Split([#$D#$A]);
  if Length(AContentArray) = 0 then
    exit;

  FResponceHeaders.Clear;

  try
    AIsHeader := True;
    AContent := '';
    FStatusCode := 200;
    FStatusText := '';
    for I := Low(AContentArray) to High(AContentArray) do
    begin
      if AContentArray[I].IsEmpty then
      begin
        AIsHeader := False;
        continue;
      end;
      if AIsHeader then
      begin
        APair := AContentArray[I].Split([':']);
        if Length(APair) = 2 then
        begin
          FResponceHeaders.TryAdd(APair[0], APair[1].Trim);
          if APair[0].Equals('Status') then
          begin
            AStatusCodeText := APair[1].Substring(1, 3);
            FStatusText := APair[1].Substring(4, APair[1].Length).Trim;
            if Not TryStrToInt(AStatusCodeText, FStatusCode) then
              exit;
          end;
        end;
      end
      else
        AContent := AContent + AContentArray[I];
    end;
  except
    on E : Exception do
    begin
      FStatusCode := -1;
      FStatusText := E.Message;
      FLastError := 'TFastCGI.ParseContent : ' + E.Message;
      exit;
    end;
  end;

  Result := FStatusCode = 200
end;

procedure TFastCGI.LogStream(const AStream : TMemoryStream);
var ALogFilePathStream : String;
    APos : Int64;
begin
exit;
  ALogFilePathStream := 'D:\Embarcadero\Projects\ShareCode\FastCGI\logs\' + FormatDateTime('yyyy.mm.dd_hh-nn-ss', Now()) + '_stream.log';
  APos := AStream.Position;
  AStream.SaveToFile(ALogFilePathStream);
  AStream.Position := APos;
end;

function TFastCGI.SwapEndiannessOfBytes2(const Value: Word) : Word;
var ATemp : Word;
    I: Integer;
begin
  ATemp := 0;
  for I := 0 to sizeof(Value) - 1 do
    inc(ATemp, ((Value shr (8*I)) and $FF) shl (8*(sizeof(Value) - I - 1)));
  Result := ATemp;
end;

function TFastCGI.SwapEndiannessOfBytes4(const Value: Integer) : Integer;
var ATemp : Integer;
    I: Integer;
begin
  ATemp := 0;
  for I := 0 to sizeof(Value) - 1 do
    inc(ATemp, ((Value shr (8*I)) and $FF) shl (8*(sizeof(Value) - I - 1)));
  Result := ATemp;
end;

end.

