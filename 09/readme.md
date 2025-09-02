# Aula 09 | Manipulação de Arquivos


## Introdução
 Até agora, vimos como manipular variáveis simples, vetores, records e outros tipos de dados. Porém, todas essas informações só existem **enquanto o programa está em execução**.  
Se quisermos armazenar dados de forma **permanente**, precisamos trabalhar com **arquivos**.

Em Pascal, a manipulação de arquivos é bastante poderosa, permitindo desde a gravação e leitura de **textos simples** até o gerenciamento de **estruturas complexas com registros**.

Nesta aula, aprenderemos a trabalhar com:

- Arquivos de texto (`TextFile`);
- Arquivos binários (`File of ...`);
- Arquivos de registros (`Record`);
- Operações de localizar, adicionar e excluir registros em arquivos.

---

## Arquivos de Texto

Um arquivo de texto é composto por caracteres legíveis, como `.txt`.  
No Pascal, trabalhamos com arquivos de texto usando o tipo **`TextFile`**.

### Principais procedimentos e funções

- `Assign(arquivo, 'nome.txt')` → associa a variável de arquivo ao nome do arquivo físico.
- `Rewrite(arquivo)` → cria/abre o arquivo para escrita (apaga conteúdo existente).
- `Append(arquivo)` → abre o arquivo para escrita no final.
- `Reset(arquivo)` → abre o arquivo para leitura.
- `Close(arquivo)` → fecha o arquivo.
- `Read` / `ReadLn` → lê dados.
- `Write` / `WriteLn` → escreve dados.

### Exemplo: Gravando um arquivo de texto

```pascal
program GravarTexto;
var
  arq: TextFile;
  i: integer;
begin
  Assign(arq, 'numeros.txt');
  Rewrite(arq); // cria/abre para escrita
  for i := 1 to 10 do
    Writeln(arq, 'Número: ', i);
  Close(arq);
end.
```

### Exemplo: Lendo um arquivo de texto

```pascal
program LerTexto;
var
  arq: TextFile;
  linha: string;
begin
  Assign(arq, 'numeros.txt');
  Reset(arq); // abre para leitura
  while not Eof(arq) do
  begin
    Readln(arq, linha);
    Writeln('Linha lida: ', linha);
  end;
  Close(arq);
end.
```

---

## Arquivos Binários (`File of`)

Enquanto os arquivos de texto armazenam caracteres legíveis, os arquivos binários guardam os dados de forma compacta e mais eficiente.

A declaração é feita com `File of Tipo`.

### Exemplo: Gravando inteiros em um arquivo binário

```pascal
program ArquivoBinario;
var
  arq: file of integer;
  i: integer;
begin
  Assign(arq, 'dados.bin');
  Rewrite(arq);
  for i := 1 to 5 do
    Write(arq, i * 10);
  Close(arq);
end.
```

### Exemplo: Lendo inteiros de um arquivo binário

```pascal
program LerBinario;
var
  arq: file of integer;
  valor: integer;
begin
  Assign(arq, 'dados.bin');
  Reset(arq);
  while not Eof(arq) do
  begin
    Read(arq, valor);
    Writeln('Valor: ', valor);
  end;
  Close(arq);
end.
```

---

## Arquivos de Registros

Um **registro (record)** pode ser armazenado diretamente em um arquivo binário.  
Isso é muito útil para sistemas de cadastro.

### Exemplo: Estrutura de cliente

```pascal
type
  TCliente = record
    cpf: string[11];
    nome: string[50];
    telefone: string[15];
  end;
```

O arquivo pode ser declarado como:

```pascal
var
  arqClientes: file of TCliente;
```

### Exemplo: Gravando registros

```pascal
program GravarClientes;
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

  cliente.cpf := '98765432100';
  cliente.nome := 'Maria Oliveira';
  cliente.telefone := '11888888888';
  Write(arq, cliente);

  Close(arq);
end.
```

### Exemplo: Lendo registros

```pascal
program LerClientes;
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
```

---

## Localizando Registros em um Arquivo

Podemos percorrer o arquivo registro por registro até encontrar o dado desejado.

```pascal
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
    Read(arq, cliente);
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
```

---

## Adicionando Registros em um Arquivo

Para adicionar novos registros, usamos `Reset` + `Seek` para ir até o final do arquivo e `Write` para gravar.

```pascal
Seek(arq, FileSize(arq)); // vai para o fim do arquivo
Write(arq, novoCliente);
```

---

## Excluindo Registros em um Arquivo

Não existe exclusão direta em arquivos binários. A estratégia comum é:

1. Criar um arquivo temporário.
2. Copiar todos os registros, exceto o que será excluído.
3. Substituir o arquivo original pelo temporário.

```pascal
program ExcluirCliente;
type
  TCliente = record
    cpf: string[11];
    nome: string[50];
    telefone: string[15];
  end;
var
  arq, temp: file of TCliente;
  cliente: TCliente;
  cpfExcluido: string[11];
begin
  Assign(arq, 'clientes.dat');
  Assign(temp, 'temp.dat');
  Reset(arq);
  Rewrite(temp);

  Write('Digite o CPF a excluir: ');
  Readln(cpfExcluido);

  while not Eof(arq) do
  begin
    Read(arq, cliente);
    if cliente.cpf <> cpfExcluido then
      Write(temp, cliente);
  end;

  Close(arq);
  Close(temp);

  Erase(arq);
  Rename(temp, 'clientes.dat');

  Writeln('Cliente excluído com sucesso.');
end.
```

---

## Exercícios

1. Crie um programa que grave os nomes de 5 pessoas em um arquivo de texto e depois leia-os na tela.
2. Crie um programa que leia um arquivo de texto linha por linha e conte quantas linhas ele possui.
3. Grave 20 números inteiros em um arquivo binário e depois leia-os de volta mostrando na tela.
4. Crie um programa que leia um arquivo binário de inteiros e calcule a soma de todos os valores.
5. Defina um `record` para armazenar livros (título, autor, ano) e grave 3 registros em um arquivo.
6. Faça um programa que leia os registros do arquivo de livros e exiba apenas os livros publicados após 2000.
7. Implemente a busca de clientes em um arquivo binário pelo CPF (como no exemplo).
8. Crie uma rotina para adicionar novos clientes a um arquivo já existente.
9. Implemente a exclusão de clientes pelo CPF, recriando o arquivo sem o registro excluído.
10. Crie um sistema simples de cadastro de alunos usando arquivos de registros, com opções de adicionar, listar, buscar e excluir.
11. Leia um arquivo de texto contendo um cpf em cada linha e exibia no console o número do cpf o estado em que ele pode ter sido emitido.
Para saber qual estado o CPF foi emitido, utilize a tabela abaixo onde a coluna `código` corresponde ao 9º dpigito do cpf;

Site para gerar o arquivo de CPF
https://fasttools.dev/cpf


| UF | Nome do Estado              | Código |
|----|-----------------------------|--------|
| RS | Rio Grande do Sul           | 0      |
| DF | Distrito Federal            | 1      |
| GO | Goiás                       | 1      |
| MT | Mato Grosso                 | 1      |
| MS | Mato Grosso do Sul          | 1      |
| TO | Tocantins                   | 1      |
| AC | Acre                        | 2      |
| AP | Amapá                       | 2      |
| AM | Amazonas                    | 2      |
| PA | Pará                        | 2      |
| RO | Rondônia                    | 2      |
| RR | Roraima                     | 2      |
| CE | Ceará                       | 3      |
| MA | Maranhão                    | 3      |
| PI | Piauí                       | 3      |
| AL | Alagoas                     | 4      |
| PB | Paraíba                     | 4      |
| PE | Pernambuco                  | 4      |
| RN | Rio Grande do Norte         | 4      |
| BA | Bahia                       | 5      |
| SE | Sergipe                     | 5      |
| MG | Minas Gerais                | 6      |
| RJ | Rio de Janeiro              | 7      |
| ES | Espírito Santo              | 7      |
| SP | São Paulo                   | 8      |
| PR | Paraná                      | 9      |
| SC | Santa Catarina              | 9      |


---


👉 Com isso, você tem uma base sólida para trabalhar com **arquivos em Pascal**, desde operações simples em **texto** até sistemas de cadastro completos com **registros**.
