program gravar_clientes;

type
  TCliente = record
    cpf: string[11];
    nome: string[50];
    telefone: string[15];
  end;

var
  arq: file of TCliente;
  cliente: TCliente;

begin
  Assign(arq, 'clientes.dat');
  Rewrite(arq);

  cliente.cpf := '12345678901';
  cliente.nome := 'Joao Silva';
  cliente.telefone := '11999999999';  
  Write(arq, cliente);

  cliente.cpf      := '98765432100';
  cliente.nome     := 'Maria Oliveira';
  cliente.telefone := '11888888888';
  Write(arq, cliente);

  Close(arq);
end.