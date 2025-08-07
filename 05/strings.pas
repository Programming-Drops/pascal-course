program strings;

uses
  SysUtils;


function Substitui(a:char; b: char; s: string) : string;
var
  i: integer;
  r: string;
begin  
  r := s;
  for i:= 1 to Length(r)do
  begin
    if ( r[i] = a ) then
       r[i] := b;
  end;

  Substitui := LowerCase(r);  
end;

var
   texto: string;

begin  
  
  texto := 'Curso de Pascal do Canal Programming drops com URLS normalizadas';    
  Writeln(Substitui(' ', '-', texto));  
  
end.