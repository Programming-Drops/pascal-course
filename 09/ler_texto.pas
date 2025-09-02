program ler_texto;
var
  arq: TextFile;
  linha: string;
begin
  Assign(arq, 'numeros.txt');
  Reset(arq);
  while not Eof(arq) do
  begin
    Readln(arq, linha);
    Writeln('Linha lida: ', linha);
  end;
  Close(arq);
end.