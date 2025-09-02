program analiste_cpfs;

uses
  SysUtils;


var
  arquivo: TextFile;  
  n: integer;
  cpf: string;
  estatistica : array[0..9] of integer;
begin   
   Assign(arquivo, 'cpfs.txt');
   Reset(arquivo);

   for n:= 0 to 0 do
    estatistica[n] := 0;

   n := 0;
   while not Eof(arquivo) do
   begin
    ReadLn(arquivo, cpf);
    n := n + 1;
    case cpf[9] of
       '0' : WriteLn('O ', n, 'º cpf (', cpf, ') foi emitido no Rio Grande do Sul');
       '1' : WriteLn('O ', n ,'º cpf (', cpf, ') pode ter sido emitido em um destes estados: DF, GO, MT, MS, TO');
       '2' : WriteLn('O ', n ,'º cpf (', cpf, ') sido emitido em um destes estados: AC, AP, AM, PA, RO, RR');
       '3' : WriteLn('O ', n ,'º cpf (', cpf, ') sido emitido em um destes estados: CE, MA, PI');
       '4' : WriteLn('O ', n ,'º cpf (', cpf, ') sido emitido em um destes estados: AL, PB, PE, RN');
       '5' : WriteLn('O ', n ,'º cpf (', cpf, ') sido emitido em um destes estados: BA, SE');
       '6' : WriteLn('O ', n, 'º cpf (', cpf, ') foi emitido em Minas Gerais');
       '7' : WriteLn('O ', n ,'º cpf (', cpf, ') sido emitido em um destes estados: RJ, ES');
       '8' : WriteLn('O ', n, 'º cpf (', cpf, ') foi emitido em São Paulo');
       '9' : WriteLn('O ', n ,'º cpf (', cpf, ') pode ter sido emitido em um destes PR, SC');
   end;
    estatistica[ StrToInt(cpf[9]) ] :=  estatistica[ StrToInt(cpf[9]) ] + 1;
   end;   
   Close(arquivo);

   WriteLn('Estatísticas do arquivo:');
   WriteLn('------------------------');
   WriteLn(estatistica[0]:5, ' cpfs no RS');
   WriteLn(estatistica[1]:5, ' cpfs distriuídos em DF, GO, MT, MS, TO');
   WriteLn(estatistica[2]:5, ' cpfs distriuídos em AC, AP, AM, PA, RO, RR');
   WriteLn(estatistica[3]:5, ' cpfs distriuídos em CE, MA, PI');
   WriteLn(estatistica[4]:5, ' cpfs distriuídos em AL, PB, PE, RN');
   WriteLn(estatistica[5]:5, ' cpfs distriuídos em BA, SE');
   WriteLn(estatistica[6]:5, ' cpfs em MG');
   WriteLn(estatistica[7]:5, ' cpfs distriuídos em RJ, ES');
   WriteLn(estatistica[8]:5, ' cpfs em SP');
   WriteLn(estatistica[9]:5, ' cpfs distriuídos em PR, SC'); 

end.

