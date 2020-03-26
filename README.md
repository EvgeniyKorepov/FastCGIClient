# FastCGIClient
Delphi FastCGI client for php-fpm and etc.
https://ru.wikipedia.org/wiki/FastCGI
http://www.mit.edu/~yandros/doc/specs/fcgi-spec.html


Example :
```
var
  FFastCGI : TFastCGI;
...

procedure TFormMain.FormCreate(Sender: TObject);
var AHost : String;
    APort : Word;
    AScriptFileName, ARequest, AContent : String;    
begin
  AHost := '10.0.0.4';
  APort := 9000;
  FFastCGI := TFastCGI.Create(AHost, APort);
  FFastCGI.KeepAlive := True;    
  
  AScriptFileName := '/opt/xxx.php';
  ARequest := 'request=1234567890';
  
  if FFastCGI.Get(AScriptFileName, ARequest, AContent) then
    Memo.Text :=  AContent
  else
    Memo.Text := FFastCGI.StatusCode.ToString + ' ' + FFastCGI.StatusText;
end;

```
