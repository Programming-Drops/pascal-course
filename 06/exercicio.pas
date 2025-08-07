program exercicio;

(* 
    Faça um programa em Pascal que leia 6 nomes de pessoas e depois exiba:

    O primeiro nome digitado
    O último nome digitado
    Todos os nomes em ordem inversa
*)

const
  LIMITE = 6;

var
  pessoas: array[1..LIMITE] of string;

procedure LeNomes;
var
  i: integer;
begin
  WriteLn('Informe ', LIMITE, ' nomes:');
  for i:= 1 to LIMITE do
  begin
    Write('Informe o nome da ', i, 'ª pessoa: ');
    ReadLn(pessoas[i]);
  end;
end;

procedure ImprimePessoasNaOrdeReversa;
var
  i : integer;
begin
 WriteLn('Lista de pessoas na ordem inversa:');
 for i:= LIMITE downto 1 do
 begin
   WriteLn('  -> ', pessoas[i]);
 end;
end;

begin
  LeNomes;

  WriteLn('A primeira pessoa é: ', pessoas[1]);
  WriteLn('A última pessoa é: ', pessoas[LIMITE]);
  ImprimePessoasNaOrdeReversa;

end.