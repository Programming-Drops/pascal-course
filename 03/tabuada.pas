program tabuada;

var
  i, x : integer;
begin
  for i:= 1 to 20 do
  begin
    x := i * 5;
    WriteLn('5 * ', i:2, ' = ', x:3);
  end;
end.