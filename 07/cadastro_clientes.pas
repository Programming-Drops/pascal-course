program cadastro_clientes;

{ 2. Crie um vetor com 10 clientes (`TCliente`). Preencha os dados via teclado e mostre todos no final.  }

const
  MAX_CADASTRO = 10;

type TCliente = record
  nome: string;
  telefone: string;
end;

type TCadastro = array [1..MAX_CADASTRO] of TCliente;


procedure PreencherCadastro(var cadastro: TCadastro);
var
  i : integer;
begin
  for i:= Low(cadastro) to High(cadastro) do
  begin
     writeln('Cadastro do ', i, 'º cliente:');
     write('      nome: ');
     readln(cadastro[i].nome);

     write('  telefone: ');
     readln(cadastro[i].telefone);
  end;
  WriteLn('Todos os clientes foram cadastrados.');
end;

procedure ExibirCadastro(var cadastro: TCadastro);
var
  i : integer;
begin
  WriteLn('--------------------------');
  WriteLn('-- CADASTRO DE CLIENTES --');
  WriteLn('--------------------------');
  for i:= Low(cadastro) to High(cadastro) do
  begin
    WriteLn(' ', i:2, ': ', cadastro[i].nome, ' (' , cadastro[i].telefone, ')'); 
  end;
  WriteLn('--------------------------');
end;

var
  cadastro : TCadastro;

begin
  PreencherCadastro(cadastro);
  ExibirCadastro(cadastro);
end.