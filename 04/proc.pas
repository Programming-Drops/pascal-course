program proc;


procedure Cabecalho;
begin
    WriteLn('--------------------------------------------');
    WriteLn('|       MEU PROGRAMA INCRÍVEL              |');
    WriteLn('--------------------------------------------');
end;

function AreaRect(base: real; altura: real): real;
begin  
  AreaRect := base * altura;
end;

var
  a :real;
begin
  a := AreaRect(10.0, 10.0);
  WriteLn('Área = ', a:5:2);
end.
