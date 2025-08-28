# Aula 08 | Tipos Enumerados (Enums)

## Visão geral

**Tipos enumerados** (ou *enums*) são tipos **ordinais** definidos por você, compostos por um **conjunto finito de valores nomeados**. Eles permitem representar estados, categorias e opções de forma **legível, segura e expressiva**, substituindo números “mágicos” por nomes claros.

Exemplos do mundo real:

* Dia da semana (`Domingo`, `Segunda`, …)
* Status de pagamento (`Pendente`, `Pago`, `Atrasado`)
* Nível de acesso (`Convidado`, `Usuário`, `Admin`)

Benefícios:

* **Legibilidade**: `Pago` é mais claro que `2`.
* **Segurança de tipo**: o compilador evita valores inválidos.
* **Manutenção**: mudanças ficam localizadas na declaração do tipo.
* **Integração com linguagem**: funcionam com `case`, `for`, `set of`, `Low/High`, `Ord`, `Succ/Pred` etc.

---

## Sintaxe básica

```pascal
type
  TPagamentoStatus = (psPendente, psPago, psAtrasado);

var
  status: TPagamentoStatus;

begin
  status := psPendente;

  if status = psPago then
    Writeln('Tudo certo!')
  else
    Writeln('Pagamento não confirmado.');
end.
```

* A **ordem de declaração** define a **ordem ordinal**:
  `Ord(psPendente) = 0`, `Ord(psPago) = 1`, `Ord(psAtrasado) = 2`.
* Como todo tipo ordinal, você tem:

  * `Ord(x)`: inteiro correspondente.
  * `Succ(x)`: sucessor (próximo valor).
  * `Pred(x)`: predecessor (valor anterior).
  * `Low(TTipo)`, `High(TTipo)`: menor e maior valor do tipo.

Exemplo:

```pascal
Writeln(Ord(psPendente));  // 0
Writeln(Ord(psPago));      // 1
Writeln(Succ(psPendente)); // psPago
Writeln(Pred(psAtrasado)); // psPago
```

> **Dica:** Ative verificação de intervalo para pegar erros cedo:
>
> ```pascal
> {$R+}  // Range checks ON
> ```

---

## Enums em `case of`

```pascal
type
  TPlano = (planoBasico, planoPro, planoEnterprise);

procedure DescreverPlano(p: TPlano);
begin
  case p of
    planoBasico:     Writeln('Plano Básico: essencial.');
    planoPro:        Writeln('Plano Pro: recursos avançados.');
    planoEnterprise: Writeln('Enterprise: sob medida.');
  end;
end;
```

---

## Iterando sobre um enum

```pascal
type
  TDiaSemana = (dsDom, dsSeg, dsTer, dsQua, dsQui, dsSex, dsSab);

var
  d: TDiaSemana;
begin
  for d := Low(TDiaSemana) to High(TDiaSemana) do
    Writeln(Ord(d), ' - ', d); // imprime o índice e (por padrão) o número; veja seção "Entrada/Saída"
end.
```

> **Observação:** `Write/WriteLn` de enums normalmente imprime o **nome** em Delphi; em Free Pascal pode variar conforme modo/RTL. A seguir mostramos jeitos portáveis para exibir nomes legíveis.

---

## Arrays indexados por enum

Arrays podem ser indexados diretamente por um enum — isso é muito útil para tabelas de lookup:

```pascal
type
  TDiaSemana = (dsDom, dsSeg, dsTer, dsQua, dsQui, dsSex, dsSab);

const
  DiaSemanaNome: array[TDiaSemana] of string = (
    'Domingo', 'Segunda', 'Terça', 'Quarta', 'Quinta', 'Sexta', 'Sábado'
  );

var d: TDiaSemana;
begin
  for d := Low(TDiaSemana) to High(TDiaSemana) do
    Writeln(DiaSemanaNome[d]);
end.
```

---

## Subfaixas (subranges) de enums

Você pode definir **subintervalos** de um enum:

```pascal
type
  TDiaSemana   = (dsDom, dsSeg, dsTer, dsQua, dsQui, dsSex, dsSab);
  TDiaUtil     = dsSeg .. dsSex; // apenas dias úteis

var
  dia: TDiaUtil;
begin
  for dia := Low(TDiaUtil) to High(TDiaUtil) do
    Writeln('Dia útil: ', Ord(dia));
end.
```

---

## Conjuntos (sets) de enums

Conjuntos permitem **combinar múltiplos valores** de um enum, com operações eficientes. Em Pascal, `set of` tem limite de até **256 elementos**.

```pascal
type
  TDiaSemana = (dsDom, dsSeg, dsTer, dsQua, dsQui, dsSex, dsSab);
  TDias      = set of TDiaSemana;

var
  horarioComercial: TDias;
begin
  horarioComercial := [dsSeg, dsTer, dsQua, dsQui, dsSex];

  if dsSab in horarioComercial then
    Writeln('Abrimos no sábado.')
  else
    Writeln('Fechados no sábado.');

  // Inclui e remove
  Include(horarioComercial, dsSab);
  Exclude(horarioComercial, dsSeg);
end.
```

Operações comuns com `set of`:

* **Teste de pertinência**: `if valor in conjunto then ...`
* **União**: `A + B`
* **Interseção**: `A * B`
* **Diferença**: `A - B`

---

## Enums em registros (records) — exemplo de cadastro

```pascal
type
  TPlano = (planoBasico, planoPro, planoEnterprise);
  TUF    = (ufPR, ufSC, ufRS, ufSP, ufRJ, ufMG); // exemplo reduzido
  TStatusCliente = (stAtivo, stInativo, stInadimplente);

  TCliente = record
    CPF: string[14];
    Nome: string[80];
    Telefone: string[20];
    UF: TUF;
    Plano: TPlano;
    Status: TStatusCliente;
  end;

procedure ImprimirCliente(const C: TCliente);
const
  UFNome: array[TUF] of string = ('PR','SC','RS','SP','RJ','MG');
  PlanoNome: array[TPlano] of string = ('Básico','Pro','Enterprise');
  StatusNome: array[TStatusCliente] of string = ('Ativo','Inativo','Inadimplente');
begin
  Writeln('Cliente: ', C.Nome, ' (', C.CPF, ')');
  Writeln('Telefone: ', C.Telefone);
  Writeln('UF: ', UFNome[C.UF]);
  Writeln('Plano: ', PlanoNome[C.Plano]);
  Writeln('Status: ', StatusNome[C.Status]);
end;
```

---

## Conversão para string e leitura do usuário

### 1) Tabela de nomes (portável)

```pascal
type
  TDiaSemana = (dsDom, dsSeg, dsTer, dsQua, dsQui, dsSex, dsSab);

const
  DiaSemanaNome: array[TDiaSemana] of string = (
    'domingo','segunda','terca','quarta','quinta','sexta','sabado'
  );

function StrParaDiaSemana(const S: string): TDiaSemana;
var
  d: TDiaSemana;
  smin: string;
begin
  smin := LowerCase(S);
  for d := Low(TDiaSemana) to High(TDiaSemana) do
    if smin = DiaSemanaNome[d] then
      exit(d);
  raise Exception.Create('Dia inválido: ' + S);
end;
```

### 2) (Avançado) Usando `TypInfo` (Delphi/FPC)

Se seu modo/RTL suportar RTTI de enums:

```pascal
uses TypInfo;

function EnumToString<T>(Value: T): string;
begin
  Result := GetEnumName(TypeInfo(T), Ord(Value));
end;
```

> **Dica:** Prefira manter **tabelas de nomes** para estabilidade e i18n, mesmo quando `TypInfo` estiver disponível.

---

## Enums com valores explícitos (Delphi/FPC)

Alguns dialetos (Delphi e Free Pascal em modo Delphi) permitem atribuir **valores numéricos** específicos aos itens do enum:

```pascal
type
  THttpStatus = (hsOK = 200, hsBadRequest = 400, hsNotFound = 404);

begin
  Writeln(Ord(hsOK));        // 200
  Writeln(Ord(hsNotFound));  // 404
end.
```

Cuidado com:

* **Lacunas**: `Succ`/`Pred` passam pelo próximo nome declarado, não pelo “próximo inteiro”.
* **Persistência**: mudar ordinais **quebra dados** gravados/serializados. Congele a ordem após publicar.

---

## Enums com escopo (scoped enums)

Em Delphi modernos e FPC (com `{$scopedenums on}`), os nomes ficam qualificados pelo tipo, evitando colisões:

```pascal
{$scopedenums on}
type
  TStatus = (Ativo, Inativo);

var
  s: TStatus;
begin
  s := TStatus.Ativo; // qualificado
end.
```

> Útil quando há muitos enums com rótulos parecidos (`Ativo`, `Pago`, etc.).

---

## Boas práticas

1. **Nomeie com prefixo do domínio** (`TPlano`, `TStatusCliente`) e rótulos claros.
2. **Congele a ordem** após estabilizar o modelo de dados.
3. **Evite “int casting”** arbitrário; use `Ord` somente para fins legítimos (índices, persistência controlada).
4. **Use `set of`** para flags combináveis (permissões, dias de funcionamento).
5. **Valide entradas** ao converter texto para enum (tratando acentos, variações).
6. **Ative `{$R+}`** em desenvolvimento para capturar acessos fora de faixa.
7. Para **serialização**, prefira salvar **strings estáveis** (nomes), não ordinais crus.

---

## Exemplos completos

### Exemplo 1 — Agendamento semanal de atendimento

```pascal
program AgendaSemanal;
{$mode objfpc}{$H+}
{$R+}

uses SysUtils;

type
  TDiaSemana = (dsDom, dsSeg, dsTer, dsQua, dsQui, dsSex, dsSab);
  TDias = set of TDiaSemana;

const
  DiaNome: array[TDiaSemana] of string =
    ('Domingo','Segunda','Terça','Quarta','Quinta','Sexta','Sábado');

procedure MostrarAgenda(const A: TDias);
var d: TDiaSemana;
begin
  for d := Low(TDiaSemana) to High(TDiaSemana) do
    if d in A then
      Writeln(DiaNome[d], ': aberto')
    else
      Writeln(DiaNome[d], ': fechado');
end;

var
  Aberto: TDias;
begin
  Aberto := [dsSeg, dsTer, dsQua, dsQui, dsSex];
  Include(Aberto, dsSab); // abriu sábado

  MostrarAgenda(Aberto);
end.
```

### Exemplo 2 — Cadastro com enum e `case of`

```pascal
program CadastroCliente;
{$mode objfpc}{$H+}

type
  TPlano = (planoBasico, planoPro, planoEnterprise);
  TStatus = (stAtivo, stInativo, stInadimplente);

  TCliente = record
    CPF, Nome, Telefone: string;
    Plano: TPlano;
    Status: TStatus;
  end;

procedure EmitirCobranca(const C: TCliente);
begin
  case C.Status of
    stAtivo:        Writeln('Emitindo cobrança normal.');
    stInadimplente: Writeln('Aplicar multa e notificar.');
    stInativo:      Writeln('Conta inativa. Não cobrar.');
  end;
end;

var C: TCliente;
begin
  C.Nome := 'Ana Silva';
  C.CPF := '123.456.789-00';
  C.Telefone := '(11) 99999-0000';
  C.Plano := planoPro;
  C.Status := stAtivo;

  EmitirCobranca(C);
end.
```

---

## Erros comuns e como evitar

* **Reordenar itens depois de salvar dados** → quebra compatibilidade. **Congele a ordem.**
* **Acessar `Succ/Pred` nos extremos** → `Range error`. Use checagens:

  ```pascal
  if X < High(TTipo) then X := Succ(X);
  ```
* **Converter texto sem validar** → exceções. Normalize (`LowerCase`, remover acentos) e mapeie em tabela.
* **Usar enum gigante em `set of`** → limite de 256. Quebre em categorias ou use `QWord`/bitfields quando necessário (avançado).

---

## Exercícios (10 + 2 desafios)

1. **Básico**: Declare `TMoeda = (Real, Dolar, Euro)` e escreva um `case` que exibe a letra (`R`, `D`, `E`).
2. **Ordinais**: Para `TMoeda` acima, imprima `Ord(Real)`, `Ord(Dolar)`, `Ord(Euro)` e comente o resultado.
3. **Iteração**: Crie `TDiaSemana` e imprima todos com `for d := Low(..) to High(..)`.
4. **Subrange**: Defina `TDiaUtil = dsSeg..dsSex` e liste apenas dias úteis.
5. **Set de flags**: Com `TPermissao = (permLer, permEscrever, permApagar)`, use `set of` para representar as permissões de um usuário. Teste inclusão e remoção.
6. **Lookup**: Faça um array `array[TPlano] of Currency` com preços por plano e imprima a tabela.
7. **Cadastro**: Crie `TStatusCliente` e um record `TCliente`. Escreva uma função que retorna `boolean` indicando se pode faturar (depende de `Status`).
8. **Conversão**: Implemente `StrParaDiaSemana` (sem `TypInfo`) que aceita “segunda”, “Segunda”, “SEGUNDA”.
9. **Validação**: Escreva `TryStrToStatus` que retorna `true/false` sem lançar exceção em entradas inválidas.
10. **Menu**: Defina `TOpcaoMenu = (opListar, opIncluir, opExcluir, opSair)` e um loop `case` que processa a opção.

**Desafios**

11. **HTTP**: Crie `THttpStatus = (hsOK=200, hsCreated=201, hsBadRequest=400, hsUnauthorized=401, hsNotFound=404)` e mostre como `Succ/Pred` se comporta com lacunas. Explique.
12. **Agenda**: Modele uma agenda de horários com `set of TDiaSemana` + outro `set` para turnos (`manha`, `tarde`, `noite`). Implemente operações de união/interseção para descobrir disponibilidade comum entre dois estabelecimentos.

---

## Checklist mental ao modelar com enums

* O conjunto é finito e conhecido? **Enum** é candidato.
* Preciso combinar múltiplos valores? Use **`set of`**.
* Vou persistir? **Congele a ordem** e serialize por **nome**.
* Haverá colisão de rótulos? Considere **scoped enums**.
* Preciso apresentar nomes ao usuário? Tenha **tabelas de nomes** (i18n).

---

## Resumo

* **Enums** trazem **clareza**, **segurança** e **integração** com as construções da linguagem.
* Trabalham muito bem com `case`, `for`, `set of`, `Low/High`, `Ord`, `Succ/Pred`.
* **Boas práticas** (congelar ordem, validar entradas, usar sets apropriadamente) evitam armadilhas em projetos reais.

Pronto! Com isso, você domina o essencial — e o avançado — para usar **tipos enumerados** com confiança em Pascal.



# Apêndice | Tipos Ordinais em Pascal

## O que são?
Um **tipo ordinal** em Pascal é aquele cujos valores:
- São **distintos** e **finitos** (mesmo que o intervalo seja muito grande, como nos inteiros).  
- Têm uma **ordem natural** (você pode ir do menor para o maior, sucessor para antecessor).  
- Permitem uso das funções:  
  - `Ord(x)` → retorna o valor ordinal (número correspondente).  
  - `Succ(x)` → sucessor.  
  - `Pred(x)` → predecessor.  
  - `Low(T)` / `High(T)` → limites inferior e superior do tipo.

---

## Tabela comparativa

| Tipo        | Exemplo de declaração | Conjunto de valores | Exemplo de uso |
|-------------|------------------------|----------------------|----------------|
| **Integer** (e variações: `ShortInt`, `LongInt`, `Byte` etc.) | `var i: Integer;` | Depende do compilador (ex: -2.147.483.648 a 2.147.483.647 em `LongInt`) | `Ord(10)=10`, `Succ(10)=11`, `Pred(10)=9`, `Low(Integer)=-2147483648`, `High(Integer)=2147483647` |
| **Boolean** | `var b: Boolean;` | `False`, `True` | `Ord(False)=0`, `Ord(True)=1`, `Succ(False)=True`, `Pred(True)=False` |
| **Char**    | `var c: Char;` | Todos os caracteres ASCII/Unicode suportados | `Ord('A')=65`, `Succ('A')='B'`, `Pred('C')='B'`, `Low(Char)=#0`, `High(Char)=#255` |
| **Enum (enumerado)** | `type TCor = (Vermelho, Verde, Azul);` | Valores definidos pelo programador | `Ord(Vermelho)=0`, `Succ(Vermelho)=Verde`, `Pred(Azul)=Verde`, `Low(TCor)=Vermelho`, `High(TCor)=Azul` |
| **Subrange** | `type Nota = 0..10;` | Um intervalo específico de um tipo ordinal | `Ord(Nota(5))=5`, `Succ(5)=6`, `Pred(5)=4`, `Low(Nota)=0`, `High(Nota)=10` |
| **Set of (conjunto)** | `type Dias = set of (Seg, Ter, Qua, Qui, Sex, Sab, Dom);` | Combinação de valores de um tipo ordinal (máx. 256 elementos) | Usa operações `in`, `Include`, `Exclude`, `+`, `*`, `-` (não usa `Ord`) |

---

## Exemplos práticos

### Inteiro
```pascal
var i: Integer;
begin
  i := 42;
  Writeln(Ord(i));       // 42
  Writeln(Succ(i));      // 43
  Writeln(Pred(i));      // 41
  Writeln(Low(Integer)); // menor inteiro possível
  Writeln(High(Integer));// maior inteiro possível
end.
```

### Boolean
```pascal
var b: Boolean;
begin
  b := False;
  Writeln(Ord(b));       // 0
  Writeln(Succ(b));      // True
  Writeln(Pred(True));   // False
end.
```

### Char
```pascal
var c: Char;
begin
  c := 'A';
  Writeln(Ord(c));       // 65
  Writeln(Succ(c));      // B
  Writeln(Pred('C'));    // B
end.
```

### Enumerado
```pascal
type TCor = (Vermelho, Verde, Azul);

var cor: TCor;
begin
  cor := Verde;
  Writeln(Ord(cor));     // 1
  Writeln(Succ(cor));    // Azul
  Writeln(Pred(cor));    // Vermelho
end.
```

### Subrange
```pascal
type Nota = 0..10;

var n: Nota;
begin
  n := 7;
  Writeln(Ord(n));       // 7
  Writeln(Succ(n));      // 8
  Writeln(Pred(n));      // 6
  Writeln(Low(Nota));    // 0
  Writeln(High(Nota));   // 10
end.
```

---

## ✅ Resumo final
Todos os **tipos inteiros**, `Boolean`, `Char`, **enumerações** e **subranges** são **ordinais** em Pascal, e compartilham as mesmas operações fundamentais (`Ord`, `Succ`, `Pred`, `Low`, `High`).

