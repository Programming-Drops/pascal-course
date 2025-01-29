program somatorio;

// calcular o somatório dos valores inteiros de 1 até n
// se n = 3
// soma = (1 + 2 + 3) = 6

var
  n, i, soma: integer;
begin
  Write('Informe até onde devo somar: ');
  ReadLn(n);
  
  i := 1;
  soma := 0;  
  WriteLn('Calculando o somatório...');
  while (i <= n) do
  begin
    WriteLn('  ', soma, ' + ', i, ' = ', soma+i);
    soma += i;
    i += 1;    
  end;
  WriteLn('O somatório é igual a ', soma);
end.