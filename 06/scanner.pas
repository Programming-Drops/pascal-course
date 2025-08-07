program scanner;

{
"Scanner de números positivos"
Peça 10 números e mostre somente os positivos.
}

var
  numeros: array[1..10] of integer;


procedure Leia10Numeros;
var
  i : integer;
begin
  for i:= 1 to 10 do
  begin
    Write('Informe o ', i, 'º numero: ');
    ReadLn(numeros[i]);
  end;
end;

procedure ImprimirPositivos;
var
  i: integer;
begin
  for i:= 1 to 10 do
  begin
    if ( numeros[i] mod 2 = 0 ) then
        Write(numeros[i], ' ');
  end;
end;

begin
  Leia10Numeros;
  ImprimirPositivos;
end.