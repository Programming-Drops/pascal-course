program httpget;

{$mode objfpc}{$H+}

uses
  classes, 
  fphttpclient,
  openssl,
  opensslsockets;


function GetPage(const url: string): string;
var
  Client: TFPHttpClient;
begin
  GetPage := '';
  InitSSLInterface;
  Client := TFPHttpClient.Create(nil);
  try    
    Client.AllowRedirect := true;
    GetPage := Client.Get(url); 
  finally
    Client.Free;
  end;
end;

begin
  WriteLn('Consultando a página do google...')  ;
  WriteLn(GetPage('https://google.com'));
end.