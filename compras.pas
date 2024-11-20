program compas;

var
  product  : string;
  price    : double;
  quantity : integer;
begin
  product  := 'Dell Computer';
  price    := 3.200;
  quantity := 3;

  WriteLn('Order 310:');
  WriteLn('  - ', product);
  WriteLn('    price ', price);
  WriteLn('    qtdy   ', quantity);
  WriteLn('  --------------------');
  WriteLn('    ', price * quantity);
end.