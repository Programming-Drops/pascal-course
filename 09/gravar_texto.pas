program gravar_texto;

var
  arq: TextFile;

  i: integer;
begin
  Assign(arq, 'numeros.txt');
  Rewrite(arq);
  for i := 1 to 10 do
    Writeln(arq, 'Número: ', i);
  Close(arq);
end.