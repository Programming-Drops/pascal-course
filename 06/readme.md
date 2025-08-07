# Aula 06 | Vetores

## O que é um vetor?

Um **vetor** é uma estrutura de dados que armazena uma **sequência ordenada de elementos do mesmo tipo**, acessados por meio de **índices**.

 **Exemplo do mundo real:**  
Pense em uma fila com 10 senhas. Cada posição guarda um número diferente. A senha na primeira posição pode ser acessada diretamente pelo seu número (índice).



## Como declarar um vetor em Pascal

Para declarar vetores em Pascal, usamos a seguinte sintaxe:

```pascal
var
  numeros: array[1..10] of integer;
```

Neste exemplo, criamos um vetor chamado `numeros` que pode armazenar **10 valores do tipo inteiro**. O primeiro valor está no índice `1` e o último no índice `10`.

Veja também este outro exemplo:

```pascal
var
  exemplo: array[-5..5] of integer;
```

Aqui, o vetor `exemplo` possui **11 posições**, com índices que vão de `-5` até `5`.

**Em Pascal, você pode definir o intervalo dos índices conforme a sua necessidade**, desde que sejam números inteiros válidos. Isso permite maior flexibilidade em alguns contextos.



## Acessando elementos do vetor

Vamos supor que estamos criando um programa para registrar o **clima dos últimos dias**.

Podemos representar essas informações com um vetor, onde **cada índice negativo representa um dia no passado**, e o índice `0` representa o dia atual:

```pascal
var
  clima: array[-4..0] of string;
begin
  clima[-4] := 'Chuva';
  clima[-3] := 'Nublado';
  clima[-2] := 'Ensolarado';
  clima[-1] := 'Parcialmente nublado';
  clima[ 0] := 'Chuva leve';
end.
```

Com esse arranjo, podemos interpretar:
- `clima[0]` → Clima de **hoje**
- `clima[-1]` → Clima de **ontem**
- `clima[-2]` → Clima de **anteontem**
- e assim por diante

Por exemplo, o código:

```pascal
write(clima[-2]);
```

Irá exibir no terminal:

```
Ensolarado
```



## Usando laços para preencher e percorrer vetores

Quando um vetor possui várias posições, não é viável preencher cada uma manualmente. Em vez disso, usamos estruturas de repetição como o `for`.



### Exemplo: Lendo 5 números inteiros do usuário

```pascal
var
  i: integer;
  numeros: array[1..5] of integer;
begin
  for i := 1 to 5 do
  begin
    write('Digite o número da posição ', i, ': ');
    readln(numeros[i]);
  end;
end.
```

Neste exemplo:
- `for i := 1 to 5` percorre cada posição do vetor.
- O valor digitado é armazenado em `numeros[i]`.



### Exibindo os valores armazenados

Depois de preencher o vetor, podemos **mostrar os valores** armazenados com outro `for`:

```pascal
for i := 1 to 5 do
  writeln('Posição ', i, ': ', numeros[i]);
```


### Preenchimento automático (com valores fixos ou aleatórios)

```pascal
for i := 1 to 5 do
  numeros[i] := i * 10;
```

ou com números aleatórios:

```pascal
randomize;
for i := 1 to 5 do
  numeros[i] := random(100); // valores de 0 a 99
```

---

### Exemplo completo: Mostrar apenas os pares

```pascal
var
  i: integer;
  numeros: array[1..5] of integer;
begin
  for i := 1 to 5 do
  begin
    write('Digite o número ', i, ': ');
    readln(numeros[i]);
  end;

  writeln('Números pares digitados:');
  for i := 1 to 5 do
    if (numeros[i] mod 2 = 0) then
      writeln(numeros[i]);
end.
```

---

## Exercícios

1. **"Scanner de números positivos"**  
   Peça 10 números e mostre somente os positivos.

2. **"Mini banco de notas"**  
   Armazene 4 notas e calcule a média final.

3. **"Caça ao maior número"**  
   Leia 7 números e mostre qual foi o maior.

4. **"Contador de pares e ímpares"**  
   Conte e exiba quantos números pares e ímpares foram digitados.

5. **"Soma dos elementos extremos"**  
   Some o primeiro e o último valor do vetor.



## ✍️ Exercício proposto para a aula

> Faça um programa em Pascal que leia 6 nomes de pessoas e depois exiba:
> - O primeiro nome digitado
> - O último nome digitado
> - Todos os nomes em ordem inversa
