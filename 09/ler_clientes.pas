program ler_clientes;
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
  Reset(arq);
  while not Eof(arq) do
  begin
    Read(arq, cliente);
    Writeln('CPF: ', cliente.cpf, ' Nome: ', cliente.nome, ' Telefone: ', cliente.telefone);
  end;
  Close(arq);
end.
