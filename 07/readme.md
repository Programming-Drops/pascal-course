# Aula 07 | Records e Tipos Abstratos de Dados (TADs)

## O que é um Record?

Um **record** em Pascal é uma estrutura que permite agrupar diferentes tipos de dados sob um mesmo identificador.  
Enquanto **vetores armazenam vários elementos do mesmo tipo**, os **records armazenam atributos de tipos diferentes**, mas que fazem sentido juntos.

**Exemplo do mundo real:**  
Pense em um **cadastro de cliente**: cada cliente tem um CPF (número), um nome (texto) e um telefone (texto ou número).  
Esses dados são diferentes, mas fazem sentido estarem unidos em uma única entidade: o cliente.

---

## Como declarar um Record

A sintaxe básica é:

```pascal
type
  TCliente = record
    cpf: string;
    nome: string;
    telefone: string;
  end;
```

- `TCliente` é o **tipo** que criamos.
- Dentro do `record`, temos os **campos** (atributos) `cpf`, `nome` e `telefone`.

Agora podemos declarar variáveis do tipo `TCliente`:

```pascal
var
  cliente1: TCliente;
```

---

## Preenchendo e acessando um Record

Atribuímos valores aos campos com o operador `.` (ponto):

```pascal
begin
  cliente1.cpf := '12345678901';
  cliente1.nome := 'Maria Silva';
  cliente1.telefone := '(11)91234-5678';

  writeln('Cliente: ', cliente1.nome, ' - CPF: ', cliente1.cpf);
end.
```

Saída:

```
Cliente: Maria Silva - CPF: 12345678901
```

---

## Records dentro de Arrays

Muitas vezes precisamos armazenar **vários clientes**.  
Para isso, criamos um **vetor de records**:

```pascal
var
  clientes: array[1..5] of TCliente;
  i: integer;
begin
  for i := 1 to 5 do
  begin
    writeln('Digite o CPF do cliente ', i, ':');
    readln(clientes[i].cpf);

    writeln('Digite o nome do cliente ', i, ':');
    readln(clientes[i].nome);

    writeln('Digite o telefone do cliente ', i, ':');
    readln(clientes[i].telefone);
  end;

  writeln('--- Lista de clientes cadastrados ---');
  for i := 1 to 5 do
    writeln(clientes[i].nome, ' - ', clientes[i].cpf, ' - ', clientes[i].telefone);
end.
```

Esse código cria um **mini sistema de cadastro de clientes**.

---

## Tipos Abstratos de Dados (TADs)

Um **Tipo Abstrato de Dados (TAD)** é um modelo lógico que define:
- **Quais dados** serão armazenados
- **Quais operações** podem ser feitas sobre eles

Em Pascal, podemos criar TADs usando **types**, **records**, **procedures** e **functions**.  

Por exemplo, podemos criar um **TAD Cliente** com operações para:
- Cadastrar um cliente
- Mostrar os dados de um cliente
- Procurar um cliente pelo CPF

### Exemplo: Procedimentos associados a um Record

```pascal
procedure MostrarCliente(c: TCliente);
begin
  writeln('--- Dados do Cliente ---');
  writeln('Nome: ', c.nome);
  writeln('CPF: ', c.cpf);
  writeln('Telefone: ', c.telefone);
end;
```

Agora podemos chamar:

```pascal
MostrarCliente(cliente1);
```

Isso organiza o código e facilita a reutilização.

---

## Exercícios

1. Crie um record `TPessoa` com os campos `nome`, `idade` e `cidade`. Leia os dados e exiba-os formatados.  
2. Crie um vetor com 10 clientes (`TCliente`). Preencha os dados via teclado e mostre todos no final.  
3. Leia 5 clientes e exiba apenas os que possuem telefone começando com DDD "11".  
4. Cadastre 7 pessoas e exiba somente as que têm idade maior que 18 anos.  
5. Faça um programa que cadastre 4 clientes e permita procurar pelo **CPF** digitado.  
6. Crie um vetor de 6 clientes e exiba o **nome do cliente mais novo**.  
7. Cadastre 8 clientes e mostre apenas os que têm nome começando com a letra "A".  
8. Leia um conjunto de clientes e mostre o **número total de telefones duplicados** (repetidos).  
9. Implemente uma função que receba um cliente e retorne seu nome completo em letras maiúsculas.  
10. Crie um sistema que cadastre até 10 clientes e permita ao usuário escolher:  
   - (1) Cadastrar novo cliente  
   - (2) Listar clientes  
   - (3) Procurar cliente pelo nome  
   - (4) Sair  
