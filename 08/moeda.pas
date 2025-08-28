program moeda;

{ Declare TMoeda = (Real, Dolar, Euro) e escreva 
um case que exibe a letra (R, D, E).}

type 
  TMoeda = (moReal, moDolar, moEuro);

const
  NomeDaMoeda: array[TMoeda] of string = ('R', 'D', 'E');

var
  m: TMoeda;  
begin  
  for m := Low(TMoeda) to High(TMoeda) do
  begin
    WriteLn(NomeDaMoeda[m]);
  end;
end.