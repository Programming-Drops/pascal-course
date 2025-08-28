program ordinais;

uses
  Crt;

procedure PrintOrdinalLimits;
begin
  ClrScr;
  WriteLn('Limites dos tipos ordinais nativos');
  WriteLn('.---------------------------------------.');
  WriteLn('│Tipo       │ Low         │ High        │');    
  WriteLn('│---------------------------------------│');    
  WriteLn('│char       │ ', Ord(Low(char)):12, '│ ', Ord(High(char)):12, '│');      
  WriteLn('│byte       │ ', Low(byte):12, '│ ', High(byte):12, '|');    
  WriteLn('│integer    │ ', Low(integer):12, '│ ', High(integer):12, '│');    
  WriteLn('│longint    │ ', Low(longint):12, '│ ', High(longint):12, '│');    
  WriteLn('│boolean    │ ', Low(boolean):12, '│ ', High(boolean):12, '│');
  WriteLn('.---------------------------------------.');
end;  

procedure PrintPredAndSucc;
begin
  ClrScr;
  WriteLn('Os tipos ordinais aceitam as funcções de iteração Prec e Succ');
  WriteLn('O predecessor de 10 é ', Pred(10));
  WriteLn('O sucessor de 10 é ', Succ(10));
end;

begin
  PrintOrdinalLimits;
  ReadKey;  

  PrintPredAndSucc;
end.