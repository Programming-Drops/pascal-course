program BuscarCliente;

type
  TCliente = record
    cpf: string[11];
    nome: string[50];
    telefone: string[15];
  end;

var
  arq: file of TCliente;
  cliente: TCliente;
  buscado: string[11];
  encontrado: boolean;
begin
  Assign(arq, 'clientes.dat');
  Reset(arq);
  Write('Digite o CPF para buscar: ');
  Readln(buscado);  
  encontrado := false;
  while not Eof(arq) do
  begin
    read(arq, cliente);    
    if cliente.cpf = buscado then
    begin
      Writeln('Cliente encontrado: ', cliente.nome, ' - ', cliente.telefone);
      encontrado := true;
      break;
    end;
  end;
  if not encontrado then
    Writeln('Cliente não encontrado.');
  Close(arq);
end.