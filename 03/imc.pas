program imc;

var 
  peso: real;
  altura: real;
  resultado: real;
begin
  WriteLn('Calculadora de IMC');
  Write('  -> Informe seu peso em Kg: '); 
  Read(peso);

  Write('  -> Informe sua altura em metros: '); 
  Read(altura);

  resultado := peso / (altura * altura);

  Write('  Seu IMC é ', resultado:4:2, '. ');
  if (resultado <= 18.5) then
    WriteLn('Você está com peso abaixo do ideal')
  else
  if (resultado <= 24.9) then
    WriteLn('Você está com peso normal')
  else
  if (resultado <= 29.9) then
    WriteLn('Você está com sobrepeso')
  else
    WriteLn('Você está com obesidade');
end.