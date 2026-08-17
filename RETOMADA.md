# Retomada — MATD48 2026 (reescrita completa do curso)

**Última atualização:** 2026-08-13, ~20:30 (horário local). Nada foi commitado no git ainda — o
professor pediu tempo para revisar antes de qualquer commit/push. Este arquivo existe para retomar
o trabalho sem precisar reconstruir o contexto do zero. (Substitui a versão anterior deste arquivo,
de 2026-08-12, que descrevia um estado bem mais antigo — 6 capítulos, sem diagramas de Hasse, com
o bug de citações dos slides ainda não descoberto. Histórico completo, rodada a rodada, em
`PLANO_CONTEUDO.md`.)

## Backup

Snapshot completo do projeto (fora do git, seguro para qualquer experimento) em:
`/home/raydonal/Github/Cursos/matd48-backups/matd48_backup_20260813-2029.tar.gz` (413 MB, 2297
arquivos, integridade de gzip verificada). Backup anterior (2026-08-12, estado bem mais antigo)
ainda preservado no mesmo diretório. Gere um novo antes de qualquer mudança grande futura:

```bash
TS=$(date +%Y%m%d-%H%M)
cd /home/raydonal/Github/Cursos
tar --exclude='matd48/Livro/_bookdown_files' -czf "matd48-backups/matd48_backup_${TS}.tar.gz" matd48
gzip -t "matd48-backups/matd48_backup_${TS}.tar.gz" && echo OK
```

## Estado atual (git status — nada commitado)

`M`: `Aulas/refs.bib`, `CLAUDE.md`, `index.Rmd`, `index.html`
`??` (novos, nunca versionados): `.gitignore`, `ApoioLuz/` (pasta do professor, não versionar sem
confirmar), `Aulas2026/`, `Listas2026/`, `Livro/`, `PLANO_CONTEUDO.md`, `Projetos/Projeto-II/III/IV.*`,
`index-2025.Rmd`/`index-2025.html` (cópia arquivada do site anterior), `RETOMADA.md`.

## Estrutura atual — o que existe hoje

1. **`Livro/`** — bookdown, **8 capítulos** (mudou de 6→7→8 ao longo das rodadas — ver
   "Mudanças estruturais" abaixo):
   1. Princípios (método científico, causalidade Neyman-Rubin, elo amostragem↔desenho)
   2. Modelos lineares (**diagramas de Hasse** — seção nova, `hasse_helpers.R` — + FWL + SVD)
   3. DCA (submuestreo, efeitos aleatórios, pressupostos, contrastes, ANCOVA, Kruskal-Wallis)
   4. Blocos (DBCA, Friedman, BIB, **quadrado latino aprofundado** — Youden, MOLS/Euler —,
      quadrado greco-latino)
   5. Fatoriais (A×B, A×B×C)
   6. Fatoriais avançados ($2^k$, não replicado, confusão, $3^k$, fracionados)
   7. **Superfície de resposta** (capítulo novo, extraído do 6: CCD, análise canônica, ridge,
      steepest ascent, desejabilidade multi-resposta)
   8. Testes A/B e bandits (além do programa do semestre)
2. **`Aulas2026/`** — 14 decks xaringan (`MATD48-01` a `14`), Teoria→Aplicação→Discussão→Uso do R,
   citações **corrigidas** (ver "Bugs sérios encontrados e corrigidos" abaixo).
3. **`Listas2026/`** — 14 listas + 14 gabaritos em LaTeX (Lista01 ganhou questão extra sobre
   falseabilidade/validade externa).
4. **`Projetos/Projeto-II/III/IV.Rmd`** — 3 projetos-desafio incrementais, cada um fechando uma das
   3 notas parciais do curso (N1/N2/N3 = 50% projeto + 50% listas do período).
5. **`index.Rmd`** — homepage 2026, sistema de avaliação = 3 notas (não mais 2 provas), cronograma
   atualizado para 8 capítulos.

## Mudanças estruturais recentes (podem confundir se você não souber que aconteceram)

- **RSM virou capítulo próprio.** Era a última seção do Cap.6; foi extraída para `Livro/
  07-superficie-resposta.Rmd`, e o antigo Cap.7 (A/B/bandits) virou Cap.8
  (`08-ab-testing-bandits.Rmd`, arquivo renomeado). `_bookdown.yml`, a tabela de capítulos em
  `Livro/index.Rmd` e o cronograma da home foram todos atualizados. Quadrados latinos/greco-latinos
  **não** foram extraídos para capítulo próprio (decisão deliberada, para não encadear uma segunda
  rodada de renumeração no meio do livro) — ficaram bem mais profundos dentro do Cap.4 mesmo.
- **`Livro/hasse_helpers.R`** é um arquivo novo, compartilhado — cada capítulo que usa diagramas de
  Hasse dá `source("hasse_helpers.R")` no próprio chunk de setup (bookdown roda `new_session: yes`,
  então não dá pra compartilhar função de outra forma).

## Bugs sérios encontrados e corrigidos nesta sessão (relevantes para a revisão)

Duas classes de bug passaram despercebidas por várias rodadas porque a verificação anterior só
conferia "renderizou sem erro" (exit code), nunca abria o HTML gerado para olhar o resultado.
Documentadas em `CLAUDE.md` como "Known footgun #2" e "#3" para não se repetirem:

1. **Imagens geradas por R quebradas em todo o livro** (chegou a 55 de 63 `<img>` quebradas de
   uma vez). Causa: com `output_dir: "."`, o bookdown deixa os PNGs só em
   `_bookdown_files/0N-capitulo_files/...`, mas as páginas finais linkam sem esse prefixo.
   **Depois de rodar `bookdown::render_book()`, é preciso rodar:**
   ```bash
   cd Livro
   for d in _bookdown_files/*_files; do cp -r "$d" "./$(basename "$d")"; done
   ```
   Sem isso, o livro publicado tem quase todo gráfico quebrado. Verificar sempre com o script
   Python de checagem de `<img>` na seção de comandos abaixo.

2. **`\@ref(...)` aparecendo como texto cru** ("Figura \@ref(fig:xyz)" literal) em 13 pontos do
   livro — três causas: dentro de `fig.cap="..."`, dentro de comentário de código R, dentro de
   bloco raw ```` ```{=html} ````. Todos reescritos como texto simples. Se adicionar `\@ref(...)`
   em qualquer um desses três contextos no futuro, o mesmo bug volta.

3. **Citações nos slides de `Aulas2026/` nunca funcionaram** (desde o início do projeto, todas as
   14 aulas): `xaringan::moon_reader` não processa citação pandoc (`[@chave]`) nem
   `<div id="refs">` — confirmado por reprodução mínima isolada, mesmo forçando
   `pandoc_args: ["--citeproc"]`. Toda citação nos 14 decks foi reescrita à mão como texto
   formatado ("Fisher, 1935") e cada slide de Referências virou uma lista markdown manual. **Se
   adicionar uma citação nova em qualquer slide de `Aulas2026/`, ela também precisa ser escrita à
   mão** — a sintaxe `[@chave]` não vai funcionar nesse formato de output, só em `Livro/` (bookdown)
   e `Aulas/` antigo (verificar antes de assumir que funciona lá também).

4. **Um gráfico não mostrava o que o texto dizia mostrar** (`plot-submuestreo`, Cap.1 e slide da
   Aula 01): colorido só por técnica de estudo, sem nada distinguindo a prova A da prova B, apesar
   do texto/legenda afirmarem que sim. Corrigido com `shape`. Uma auditoria depois disso não achou
   outra instância do mesmo padrão nos Cap.1-3 — mas Cap.5-8 não passaram por essa auditoria
   específica (foram cobertos pelas rodadas anteriores de reforço gráfico, que verificavam menos
   rigorosamente). **Vale a pena, na revisão, abrir os PNGs dos capítulos 5-8 e conferir se toda
   legenda bate com o que o gráfico realmente mostra.**

5. Uma imagem reaproveitada de `Aulas/images/` estava em inglês (`there_is_only_one_test.png`,
   diagrama de Allen Downey) — as outras ~10 imagens reaproveitadas já estavam em português.
   Substituída por um diagrama nativo + um gráfico novo com dados reais do capítulo.

## O que falta — pendente de revisão humana

O professor disse que vai revisar agora. Não há uma lista específica do que ele quer reexaminar —
quando ele voltar, perguntar o que exatamente revisar antes de fazer mudanças novas.

Pontos que valem releitura própria antes de perguntar, caso ele já tenha algo em mente:
- **Cap.7 (Superfície de Resposta) é o conteúdo mais novo e menos testado por outros olhos** — fui
  eu quem escreveu a maior parte dele diretamente (o agente que deveria fazer isso travou), então
  não teve uma segunda verificação independente como o resto do livro teve. As seções de análise
  de ridge e caminho de máxima inclinação passaram por 2-3 rodadas de correção de bugs de gráfico
  antes de ficarem certas — vale conferir com atenção extra.
- Auditoria gráfica (item 4 acima) só cobriu Cap.1-3 explicitamente — Cap.5-8 não tiveram essa
  checagem específica "a legenda bate com o gráfico?".
- `Livro/03-dca.Rmd` (agora ~2000 linhas) segue sendo o arquivo com mais histórico de edição
  concorrente entre rodadas — maior chance de alguma costura estranha entre seções.
- Nenhuma leitura humana ponta-a-ponta do livro inteiro aconteceu ainda depois de todas essas
  rodadas — os agentes e eu verificamos consistência local (dentro do próprio escopo de cada
  tarefa), não uma leitura corrida do livro inteiro.
- Confirmar que o sistema de avaliação novo (3 notas, N1/N2/N3 = projeto + listas) é mesmo o que o
  professor quer, incluindo os pesos 50/50 dentro de cada nota (decisão meio automática, nunca
  validada explicitamente por ele item a item).

## Como retomar

1. Ler o feedback específico do professor sobre o que revisar.
2. Se for correção pontual: editar diretamente o(s) arquivo(s) apontado(s), re-renderizar (comandos
   abaixo), conferir **abrindo o HTML/PNG gerado**, não só checando exit code — lição cara desta
   sessão (bugs 1-4 acima só foram achados assim).
3. Se for rodada grande de novo: repetir o padrão desta sessão (diagnóstico concreto primeiro —
   grep/leitura real, nunca suposição — depois agentes em paralelo com instruções específicas, um
   por módulo, cada um instruído a verificar visualmente, não só exit 0).
4. Só commitar/pushar quando o professor disser explicitamente que está satisfeito.

### Comandos de verificação rápida

```bash
# Livro (bookdown) -- sempre os 3 passos, nesta ordem
cd Livro
rm -rf _bookdown_files *.md 0*_files
Rscript -e 'bookdown::render_book("index.Rmd", quiet=TRUE)'
for d in _bookdown_files/*_files; do cp -r "$d" "./$(basename "$d")"; done

# checagem de imagens quebradas e de \@ref cru sobrando
python3 -c "
import re, glob, os
img_pat = re.compile(r'<img src=\"([^\"]+)\"')
total = missing = 0
for fn in glob.glob('*.html'):
    html = open(fn, encoding='utf-8', errors='ignore').read()
    for src in img_pat.findall(html):
        total += 1
        p = src.split('?')[0]
        if not p.startswith('http') and not os.path.exists(p):
            missing += 1; print('FALTANDO:', fn, src)
print(f'{total} imagens, {missing} faltando')
"
grep -l '@ref(' *.html && echo "referencias quebradas encontradas acima" || echo "0 referencias quebradas"

# Aulas2026 (xaringan)
cd ../Aulas2026
for f in MATD48-*.Rmd; do Rscript -e "rmarkdown::render('$f', quiet=TRUE)"; done
# checagem de citação crua sobrando em qualquer slide
for f in MATD48-*.html; do grep -o '\[@[a-zA-Z0-9_]*\]' "$f" && echo "quebrado em $f"; done

# Listas2026 (LaTeX)
cd ../Listas2026
latexmk -pdf -interaction=nonstopmode Lista*.tex Gabarito*.tex && latexmk -c

# home do curso
cd ..
Rscript -e 'rmarkdown::render("index.Rmd", quiet=TRUE)'
```

## Histórico completo

Toda rodada de trabalho (o que foi pedido, o que foi feito, o que foi verificado) está registrada
em `PLANO_CONTEUDO.md`, na seção "Status", em ordem cronológica — é o log detalhado por trás deste
resumo.
