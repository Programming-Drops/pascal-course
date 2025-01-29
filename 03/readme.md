# Estruturas de controle

As estruturas de controle em Pascal são comandos que alteram a ordem de execução de um programa. Elas permitem a repetição de instruções ou operações, e a execução de blocos de código específicos. 

As estruturas de controle em Pascal podem ser de desvio ou de repetição. 

## Estruturas de controle de desvio 
`if`: Compara uma condição e executa um bloco de código se a condição for verdadeira

`else`: Executa um bloco de código se a condição for falsa


## Estruturas de controle de repetição 

Permitem a repetição de um conjunto de instruções quantas vezes forem necessárias

`for` : repete um conjunto de instruções `n` vezes utilizando-se de uma variável de controle que funciona como um contador. 

```pascal
var n: integer; //varável de controle (contador)
begin
  for n:= 1 to 10 do
  begin
    {as instruções dentro desta seção serão repetidas n vezes
     com n iniciando com o valor 1 e sendo incrementado de
     um em um até chegar em 10}
  end;
end.
```

`while..do` : repete um conjunto de instruções enquanto uma condição de teste for verdadeira
```pascal
var
  x : integer;
begin
  x := 15;
  while ( x <= 20 ) do
  begin
    x := x + 1;
     {as instruções dentro desta seção serão repetidas enquanto
     x for menor ou igual a 20}
  end;
end.
```

`repeat..until` : similar ao `while`, repete um conjunto de instruções enquanto uma condição for verdadeira, mas o teste da condição acontece no final do bloco ou seja, as instruções são executadas pelo menos uma vez.

```pascal
var
  x: integer;
begin
  x := 1; 
  repeat
    x := x + 1;
    {as instruções dentro desta seção serão repetidas enquanto
     x for menor ou igual a 10}
  until (x <= 10);
end.  
```

## Exemplos

### Tabuada

```pascal
{escreva a tabuada de multiplicação de 2 usando for}
var 
   i, x: integer;
begin
  for i:=1 to 10 do
  begin
    x := i * 2;
    WriteLn(i, '* 2 = ', x);
  end;
```

### Somatório
```pascal
{escreva a soma dos números de 1 até 100}
var 
  n, soma: integer;
begin
  n := 1;
  soma := 0;
  while (n <= 100)
  begin
    n += 1;
    soma += n;    
  end;
  WriteLn(soma);
```