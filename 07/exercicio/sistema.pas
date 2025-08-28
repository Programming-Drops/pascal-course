program sistema;
(* 
Crie um sistema que cadastre até 10 clientes e permita ao usuário escolher:
   (1) Cadastrar novo cliente
   (2) Listar clientes
   (3) Procurar cliente pelo nome
   (4) Sair
*)

const
    MAX_CLIENTES = 10;

type
  TMenuOption = (    
    Cadastrar = 1,
    ListarClientes,
    Procurar,
    Sair
  );

type 
  TTipoCliente = (
    Desconhecido = 0,
    PessoaFisica = 1,
    PessoaJuridica
  );

type
  TCliente = record
    tipo     : TTIpoCliente;
    nome     : string;    
    documento: string;
  end;

  TCadastro = record
    count: integer;
    dados: array[1..MAX_CLIENTES] of TCliente;    
  end;


  getCliente(pos)
  setCliente(pos, cliente)
  addCliente(cliente) 



procedure ImprimirMenu;
begin
  WriteLn('Escolha uma das opções abaixo: ');
  WriteLn('  (1) Cadastrar novo cliente');
  WriteLn('  (2) Listar clientes');
  WriteLn('  (3) Procurar cliente pelo nome');
  WriteLn('  (4) Sair');
end;

function LerOpcaoDoUsuario: TMenuOption;
var
  opcaoValida: boolean;
  opcao: integer;
begin
  opcao := -1;
  repeat 
    ImprimirMenu;
    ReadLn(opcao);
    opcaoValida := ( 
        (opcao >= Ord(Low(TMenuOption))) and 
        (opcao <= Ord(High(TMenuOption)))
    );
    if (not opcaoValida) then
        WriteLn('Opção inválida');
  until (opcaoValida);
  LerOpcaoDoUsuario := TMenuOption(opcao);
end;

function LerTipoCliente: TTipoCliente;
var
  opcaoValida: boolean;
  opcao: integer;
begin
  opcao := -1;
  repeat 
    WriteLn('-> Informe o tipo do cliente (1. Pessoa Física | 2. Pessoa Jurídica)');
    ReadLn(opcao);
    opcaoValida := ( 
        (opcao >= Ord(Low(TTipoCliente))) and 
        (opcao <= Ord(High(TTipoCliente)))
    );
    if (not opcaoValida) then
        WriteLn('Opção inválida');
  until (opcaoValida);
  LerTipoCliente := TTipoCliente(opcao);
end;

procedure CadastrarNovoCliente(var dados: TCadastro; posicao: integer);
begin  

  dados[posicao].nome      := '';
  dados[posicao].tipo      := Desconhecido;
  dados[posicao].documento := '';

  Write('-> Informe o nome: ');
  read(dados[posicao].nome);
  dados[posicao].tipo := LerTipoCliente;
  case (dados[posicao].tipo) of
    PessoaFisica   : Write('-> Informe o CPF: ');
    PessoaJuridica : Write('-> Informe o CNPJ: ');
  end;    
  read(dados[posicao].documento);
  WriteLn('Cliente cadastrado com sucesso!');
end;

procedure ListarTodosOsClientes(var cadastro: TCadastro);
var
  i: integer;
begin
  for i:= Low(cadastro) to High(cadastro) do
  begin
    WriteLn('Cliente nº ', i);
    WriteLn('Nome      :', cadastro[i].nome);
    Write  ('Tipo      : ');
    case cadastro[i].tipo of
      PessoaFisica   : WriteLn('PF');
      PessoaJuridica : WriteLn('PJ');
    end;
    WriteLn('Documento :', cadastro[i].nome);
    WriteLn('--');
  end;
end;

var
  opcaoSelecionada: TMenuOption;
  cadastro: TCadastro;
begin
  repeat
    opcaoSelecionada := LerOpcaoDoUsuario;  
    case (opcaoSelecionada) of
        Cadastrar      : CadastrarNovoCliente(cadastro, 1);
        ListarClientes : ListarTodosOsClientes(cadastro);
        Procurar       : WriteLn('Procurar');
        Sair           : WriteLn('Sair');
    end;
  until (opcaoSelecionada = Sair);

end.