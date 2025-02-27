# Aula 04

## Procedimentos e Funções

Funções e Procedimentos em Pascal: Guia para Alunos
Em Pascal, tanto funções quanto procedimentos são ferramentas essenciais para organizar e otimizar seu código. Ambos permitem dividir um programa em blocos menores e reutilizáveis, facilitando a leitura, manutenção e evitando a repetição de código.


### Funções

Uma função é como uma máquina que recebe entradas (parâmetros), realiza um processamento específico e retorna uma saída (resultado).
Utilizamos funções quando precisamos calcular um valor e usá-lo em outras partes do programa.
1.2. Sintaxe
Delphi

```pascal
function NomeDaFuncao(parametro1: tipo; parametro2: tipo): tipoDeRetorno;
var
  variavelLocal: tipo;
begin
  // Código da função
  nomeDaFuncao := valorDeRetorno;
end;
```
Exemplo
```pascal
function CalcularAreaRetangulo(base: real; altura: real): real;
begin
  calcularAreaRetangulo := base * altura;
end;
```

Esta função calcularAreaRetangulo recebe a base e a altura de um retângulo como parâmetros e retorna sua área.


```pascal
var
  largura, comprimento, area: real;
begin
  largura := 10;
  comprimento := 5;
  area := calcularAreaRetangulo(largura, comprimento);
  writeln('A área do retângulo é: ', area);
end;
```

## Procedimentos


Um procedimento é como um robô que executa uma tarefa específica, sem a necessidade de retornar um valor.
Usamos procedimentos para realizar ações como imprimir mensagens na tela, manipular dados ou controlar o fluxo do programa.
2.2. Sintaxe

```pascal
procedure nomeDoProcedimento(parametro1: tipo; parametro2: tipo);
var
  variavelLocal: tipo;
begin
  // Código do procedimento
end;
```

## Diferenças

Tanto a função quanto o procedimento servem para agrupar blocos de código em uma unidade reutilizável.
No caso da função, elas sempre retornam um valor e o procedimento não.