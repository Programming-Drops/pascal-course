program media;

{
 "Mini banco de notas"
 Armazene 4 notas e calcule a média final.
}

var
  notas: array[1..4] of real;

procedure LerNotas;
var
  i: integer;
begin
  for i:= 1 to 4 do
  begin
    Write('Insira a ', i, 'ª nota: ');
    ReadLn(notas[i]);
  end;
end;

function CalcularMedia(n: array[1..4] of real): real;
var
  i: integer;
  soma : real;
begin
  soma := 0.0;
  for i:= 1 to 4 do
    soma := soma + n[i];

  CalcularMedia := soma / 4.0;    
end;

{ início do programa }
begin
  LerNotas;
  WriteLn('A média das notas é: ', CalcularMedia(notas):0:2);
end.