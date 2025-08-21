program notas;
(*

   Aluno: José
   Notas: [6.3, 8.0, 9.6]
   Aprovado: true
*)

const
   IDX_NAO_ENCONTRADO = -1;
   MAX_ALUNOS_POR_TUMA = 40;

type TNotasSemestrais = array[1..4] of real;

type TAluno = record
  cpf      : string;
  nome     : string;
  notas    : TNotasSemestrais;
  aprovado : boolean;
end;

type TTurma = array[1..MAX_ALUNOS_POR_TUMA] of TAluno;


function PesquisarAlunoPorCPF(turma: TTurma; cpf: string): integer;
var
  i: integer;
begin  
  PesquisarAlunoPorCPF := IDX_NAO_ENCONTRADO;

  for i:=Low(turma) to High(turma) do
  begin
    if (turma[i].cpf = cpf) then
    begin
       PesquisarAlunoPorCPF := i;
       break;
    end;
  end;
end;  


function AlunoCadastrado(turma: TTurma; cpf: string): boolean;
var
  i: integer;
begin  
  AlunoCadastrado := false;

  for i:=Low(turma) to High(turma) do
  begin
    if (turma[i].cpf = cpf) then
    begin
       AlunoCadastrado := true;
       break;
    end;
  end;
end;

var
  x: integer;
  turma: TTurma;    
begin
  
  turma[ 2].cpf  := '123.456.789-00';
  turma[ 2].nome := 'José Carlos';

  turma[20].cpf  := '321.321.321-00';
  turma[20].nome := 'Maria Clara';

  if (AlunoCadastrado(turma, '321.321.321-00')) then
     WriteLn('Aluno encontrado');

  x := PesquisarAlunoPorCPF(turma, '321.321.321-00');
  if (x = IDX_NAO_ENCONTRADO) then
     WriteLn('Não foi encontrado ninguém com este cpf na turma')
  else
     WriteLn('O aluno ', turma[x].nome, ' foi encontrado na turma no índice ', x);

end.

