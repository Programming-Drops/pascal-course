program teste_clientes;

type TCliente = record
  cpf   : string;
  nome  : string;
  score : integer;
end;

type TCadastro= array[1..20] of TCliente;

procedure MostrarCliente(cliente: TCliente);
begin
  WriteLn('Dados do Cliente:');
  WriteLn('  Cpf  :', cliente.cpf);
  WriteLn('  Nome :', cliente.nome);
  WriteLn('  Score:', cliente.score);  
end;

(*
  Se o cliente existir, retorna o índice do cliente no cadastro;
  Se o clinete não existir, retorna -1;
*)
function ProcurarClientePorCPF(cadastro: TCadastro; cpf: string): integer;
var
  i: integer;
begin
  ProcurarClientePorCPF := -1;
  WriteLn('Low: ', Low(cadastro), '| High: ', High(cadastro));
  for i:= Low(cadastro) to High(cadastro) do
  begin
     Write(' ', i);
     if (cadastro[i].cpf = cpf) then
     begin
        ProcurarClientePorCPF := i;
        break;
     end;
  end;
end;

var  
  x: integer;
  clientes: TCadastro;

begin
  clientes[3].cpf := '123.123.123-00';
  clientes[3].nome:= 'Francisco Carlos';
  clientes[3].score := 83;
  //MostrarCliente(clientes[3]);
  
  clientes[10].cpf := '321.321.321-00';
  clientes[10].nome:= 'Maria Antonieta';
  clientes[10].score := 3;
  //MostrarCliente(clientes[10]);  
 
  WriteLn('Low: ', Low(clientes), '| High: ', High(clientes));
  x := ProcurarClientePorCPF(clientes, '321.321.321-00');
  WriteLn(x)
  {
  if (x = -1) then
    WriteLn('Cliente não encontrado')      
  else
    MostrarCliente(clientes[x]);     
}
end.
