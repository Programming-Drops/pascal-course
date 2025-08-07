program maior_numero;

{
    "Caça ao maior número"
    Leia 7 números e mostre qual foi o maior.
}

var
  numeros: array[1..7] of integer;

procedure LeiaOsNumeros;
var
  n :  integer;
begin
  for n:= 1 to 7 do
  begin
    Write('Infome o ', n, 'º número: ');
    ReadLn(numeros[n]);
  end;
end;

function MaiorNumero: integer;
var
  x, n : integer;
begin
  x := numeros[1];
  for n := 2 to 7 do
  begin
    if (numeros[n] > x) then
      x := numeros[n];
  end;

  MaiorNumero := x;
end;

begin
   LeiaOsNumeros;
   WriteLn('O maior número informado é ', MaiorNumero);
end.