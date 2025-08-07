program teste;

var
  i : integer;
  produtos: array[0..100] of string;

begin
  
  produtos[1] := 'Maçã';
  produtos[2] := 'Banana';
  produtos[3] := 'Bala';
  produtos[4] := 'Pilha AA';
  produtos[5] := 'Mouse';
 
  for i:= 1 to 5 do
    WriteLn(produtos[i]);

end.