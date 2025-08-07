program pares_e_impares;

(* "Contador de pares e ímpares"
Conte e exiba quantos números pares e ímpares foram digitados.}*)

const 
  LIMITE = 5;

var
  valores: array[1..LIMITE] of integer;

function LeValores : integer;
var
  i: integer;
  continua: char;
begin
  LeValores := LIMITE;

  for i:= 1 to LIMITE do
  begin
    Write('Digite o ', i, 'º número: ');
    ReadLn(valores[i]);

    Write('Deseja informar mais um número (S/N)? ');
    ReadLn(continua);

    if ( (continua = 'N') or (continua = 'n') ) then
    begin
        LeValores := i;
        break;
    end;
  end;  
end;

procedure ContaEImprime;
var
  n, i, pares, impares: integer;
begin
  pares   := 0;
  impares := 0;
  n := LeValores;

  for i := 1 to n do 
  begin
     if (valores[i] mod 2 = 0) then
        pares := pares + 1
    else
        impares := impares +1;
  end;

  if (n = 1) then
    WriteLn('O usuário digitou 1 número.')
  else
    WriteLn('O usuário digitou ', n, ' números.');

  if (pares = 1 ) then  
    WriteLn(pares:5, ' número par')
  else
    WriteLn(pares:5, ' números pares');

  if (impares = 1) then
    WriteLn(pares:5, ' número ímpar')
  else
    WriteLn(impares:5, ' números ímpares');
end;

begin
  ContaEImprime;
end.