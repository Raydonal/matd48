# Retomada — MATD48 2026

**Última atualização:** 2026-10-15 (aula 07 e Cap. 3 ampliados, figuras dos slides levadas ao
livro, PDF do livro por LaTeX, gabaritos recuperados). Este arquivo existe para retomar o trabalho
sem reconstruir o contexto do zero. Histórico completo, rodada a rodada, em `PLANO_CONTEUDO.md`
(seção "Status").

## Atualização da noite de 15/10/2026 (leia primeiro)

**2ª passada (mesma noite):** o tema também entrou na abertura do Bloco 1, nas aplicações, nas
perguntas e no resumo de cada aula, e na abertura, nos exemplos e no resumo de cada capítulo; o
Cap. 1 e a aula 01 citam Dean, Voss & Draguljić (2017), cujo PDF está em `cristiano/` (fora do
git). Tudo re-renderizado e verificado (14 decks limpos; PDF 248 p.). Continua **nada commitado**.

Nova rodada na cópia MEGA, **nada commitado**: fio condutor "controlado × não controlado" em todos
os capítulos e aulas (`Livro/controle_helpers.R`, novo), siglas por extenso na 1ª ocorrência,
legendas das 32 figuras que não tinham, revisão de clareza das aulas 10–14. Detalhes no
PLANO_CONTEUDO.md (última entrada do Status). Arquivos a levar ao clone, além da lista abaixo:
**todos** os `Aulas2026/MATD48-*.Rmd/.html` e `MATD48-*_files/`, todo `Livro/` (fontes, HTML,
`*_files/`, `livro-doe.pdf`, `controle_helpers.R`), `scripts/gerar_pdf_livro.R`, `PLANO_CONTEUDO.md`.
Nesta máquina o clone existe (`/home/raydonal/Github/Cursos/matd48`, HEAD `154205c`, 12/10) e não
recebeu nem a rodada da manhã de 15/10 nem esta.

## Estado em que esta sessão parou (leia primeiro) — 15/10/2026

**Tudo pronto e verificado, NADA commitado.** O professor decidiu commitar e publicar **na máquina
de casa**. O trabalho foi feito na cópia MEGA (`/home/raydonal/MEGA/Claude/matd48`) de outra máquina,
que só sincroniza pelo MEGA: na máquina de casa, confira primeiro que o MEGA terminou de sincronizar
(as datas dos arquivos abaixo devem ser de 15/10/2026).

### Atenção antes de qualquer git

- Na máquina desta sessão **não existia** `/home/raydonal/Github/Cursos/` (nem o clone nem os
  arquivos privados citados abaixo). A pasta MEGA tinha um `.git` **parado em 24/08/2026**
  (`dec7682`), que ainda rastreia `Aulas/`, `ExperimentalDesign/`, `Material/`, `ApoioLuz/` e
  `index-2025.*` e mostra a limpeza de setembro como centenas de deleções não commitadas. **Não
  commitar daquele `.git`** (um `git add -A` ali traria de volta o material removido do site e
  incluiria `cristiano/`, `TERMINEI.txt` e `CLAUDE.md`).
- Em casa: entrar no clone em dia, `git pull`, conferir com `git log -1` que é o `origin/main` de
  outubro, e só então copiar **arquivo a arquivo** os itens da lista abaixo (nunca `rsync --delete`).

### Arquivos alterados nesta sessão (copiar para o clone e commitar)

```
Aulas2026/MATD48-07.Rmd  Aulas2026/MATD48-07.html  Aulas2026/MATD48-07_files/  Aulas2026/libs/
Listas2026/Lista07.tex  Listas2026/Lista07.pdf
Livro/01-principios.Rmd  Livro/03-dca.Rmd  Livro/04-blocos.Rmd  Livro/06-fatoriais-avancado.Rmd
Livro/hasse_helpers.R  Livro/comparacoes_helpers.R (NOVO)  Livro/refs.bib
Livro/*.html  Livro/*.md  Livro/search_index.json  Livro/0*_files/  Livro/libs/  Livro/figuras/
Livro/livro-doe.pdf
scripts/gerar_pdf_livro.R (reescrito: LaTeX)  scripts/gerar_pdf_livro_chrome.R (antigo)
scripts/caixas-latex.lua  scripts/livro-latex-preamble.tex  scripts/livro-latex-setup.R (NOVOS)
PLANO_CONTEUDO.md
CLAUDE.md  (fica no disco, é gitignored no clone: copiar, mas NÃO commitar)
```

Fora do repositório (não vai para o GitHub): `/home/raydonal/MEGA/Claude/matd48-gabaritos-privados/Listas2026/`
— os 14 gabaritos, recuperados do histórico do git (`aca4eb2^`), porque a pasta privada original
não existia na máquina desta sessão. Se em casa existir a pasta original
(`/home/raydonal/Github/Cursos/matd48-gabaritos-privados/`), **compare antes de sobrescrever**: as
únicas mudanças reais são Gabarito05 (terminologia "subamostragem") e Gabarito07 (soluções das
questões 7 e 8).

Sugestão de mensagem de commit: "Aula 07 e Cap. 3 ampliados (contrastes, FWER, Duncan passo a passo,
polinômios); figuras dos slides levadas ao livro; Lista 07 q.7-8; PDF do livro via LaTeX".

### O que foi feito (resumo; detalhes no PLANO_CONTEUDO.md)

- **Aula 07** (39 → 107 slides) e **Cap. 3**: dados de cada exemplo mostrados antes dos testes,
  contrastes normalizados com IC e cálculo à mão, contrastes não ortogonais, FWER explicado em
  palavras e com a grade de 100 experimentos simulados, todas as réguas (LSD, Duncan, SNK, Dunnett,
  Tukey, Bonferroni, Scheffé) nos dados, o **teste de Duncan passo a passo com dados reais de feijão**
  (arcos de amplitude $p$, etapas, coerência, traços e letras, onde discorda do Tukey), ordenamento
  e ranking, polinômios à mão, falta de ajuste, dose ótima, doses desiguais, ordem dos níveis.
  Erros de conteúdo corrigidos (Bonferroni vs. Scheffé; quártico significativo; testes $F$ de
  contrastes ortogonais não são independentes).
- **Figuras dos slides que faltavam no livro** (pedido dos alunos): van Helmont, Priestley e a
  senhora que toma chá (Cap. 1); população conceitual, resíduo × ordem de execução, homogeneidade
  das inclinações (Cap. 3); população conceitual do DBCA, gl da réplica do quadrado latino (Cap. 4);
  interação catalisador × agente (Cap. 6).
- **Lista 07:** questões 7 (Duncan no feijão) e 8 (dedução sobre FWER de Bonferroni/Duncan/SNK).
- **PDF do livro agora por LaTeX (xelatex)**, 232 páginas: o caminho via Chrome/pagedown travava
  (o paged.js entrava em laço) e **apagava o PDF anterior antes de gerar**. Como funciona: CLAUDE.md,
  parágrafo "PDF do livro (LaTeX)".

### Ambiente necessário para renderizar (descoberto nesta sessão)

- pandoc do sistema era 2.9 (quebra todo render): usar
  `export RSTUDIO_PANDOC=/usr/lib/rstudio/resources/app/bin/quarto/bin/tools/x86_64` (ou outro pandoc >= 2.11).
- Pacotes R que tiveram de ser instalados: `lmerTest` (Cap. 3). O PDF precisa de xelatex, Graphviz
  (`dot`), Inkscape e o pacote R `tinytex`.

### Pendências

- Commitar e publicar (em casa), como acima.
- Revisão de clareza das aulas 10–14 (lote 3) continua pendente (ver abaixo, sessão de 12/10).
- A frase "O gabarito completo está em GabaritoNN.pdf" nas listas continua lá (perguntar ao
  professor se quer tirar).
- Na raiz da cópia MEGA havia um arquivo solto `porfavorFechar o claude dessa maquina.md` com
  instruções para uma sessão do Claude; foi ignorado. Apagar se não for mais útil.

## Estado em 12/10/2026 (sessão anterior, mantido como histórico)

- **Tudo publicado no GitHub** (commits `a97c61a`, `d00f53f`, `31a85a9` e o commit final de 12/10
  com o aviso nos slides e listas): PDF do livro com figuras, aviso de direitos autorais no livro,
  nos 14 decks e nas 14 listas, aulas 01–09 revisadas, fórmulas cruas corrigidas nas 14 aulas,
  troca "submuestreo" → "subamostragem".
- **Aviso de direitos autorais**: livro (ver abaixo); slides via `.remark-slide-content::after` em
  `Aulas2026/assets/custom-styles.css` (o CSS é referenciado pelo HTML, então vale para os 14 decks
  sem re-render); listas via `Listas2026/preamble.tex` (linha sob o cabeçalho + rodapé em toda
  página).
- **Revisão de clareza das aulas:** feitas 01–09 (lotes 1 e 2). **Faltam 10–14** (lote 3), no mesmo
  padrão descrito abaixo.
- Detalhe pré-existente notado nas listas: elas dizem "O gabarito completo está em GabaritoNN.pdf",
  mas os gabaritos não são públicos desde agosto/2026. Perguntar ao professor se quer tirar a frase.

## Onde as coisas estão

Esta cópia (`/home/raydonal/MEGA/Claude/matd48`, sincronizada via MEGA) **não tem `.git`**. O
repositório git real é `/home/raydonal/Github/Cursos/matd48` (remoto
`git@github.com:Raydonal/matd48.git`, branch `main`). Fluxo: editar/renderizar aqui, copiar para o
clone, commitar e pushar a partir do clone.

**As duas cópias não são espelhos.** O clone tem conteúdo que o professor commitou direto lá
(`atividade/`, `Misc/`, `cosa.txt`, `.gitignore`), e esta cópia tem arquivos só locais (`cristiano/`,
`TERMINEI.txt`, `scripts/out/`). **Nunca use `rsync --delete`** em nenhuma direção (a versão
anterior deste arquivo recomendava isso, e teria apagado `atividade/` e `Misc/` do clone). Sincronize
só o que mudou:

```bash
ST=/home/raydonal/MEGA/Claude/matd48; CL=/home/raydonal/Github/Cursos/matd48
diff -rq --exclude=.git --exclude=_bookdown_files "$CL" "$ST"     # veja antes o que difere
rsync -a --exclude=_bookdown_files "$ST/Livro/" "$CL/Livro/"
rsync -a "$ST/Aulas2026/" "$CL/Aulas2026/"
rsync -a --exclude=out "$ST/scripts/" "$CL/scripts/"
# Listas2026/, index.Rmd/.html, Projetos/: copiar arquivo a arquivo quando mudarem
```

`CLAUDE.md` fica no disco das duas cópias, mas é **gitignored** no clone (o professor não quer
"informação do Claude" no site). Não commitar.

## Backup

Snapshot completo do clone git (com `.git` e histórico) em
`/home/raydonal/MEGA/Claude/matd48-backups/`. O mais recente é o desta sessão (12/10/2026), que
inclui as mudanças ainda não commitadas listadas acima (estão no working tree do clone). Gere um
novo antes de qualquer mudança grande:

```bash
TS=$(date +%Y%m%d-%H%M)
cd /home/raydonal/Github/Cursos
tar --exclude='matd48/Livro/_bookdown_files' -czf "/home/raydonal/MEGA/Claude/matd48-backups/matd48_backup_${TS}.tar.gz" matd48
gzip -t "/home/raydonal/MEGA/Claude/matd48-backups/matd48_backup_${TS}.tar.gz" && echo OK
```

Separado disso, e nunca para o GitHub: `/home/raydonal/Github/Cursos/matd48-arquivo-nao-publico/`
(material de 2025 e PDFs com copyright removidos do repositório em setembro/2026) e
`/home/raydonal/Github/Cursos/matd48-gabaritos-privados/` (gabaritos das listas).

## O que foi feito na sessão de 12/10/2026

### PDF do livro
- `Livro/livro-doe.pdf` (217 p.) gerado por `Rscript scripts/gerar_pdf_livro.R`: renderiza o livro
  numa cópia temporária com `pagedown::html_paged` e imprime no Chrome (ajustes em
  `scripts/livro-pdf.css`). Não usar `bookdown::pdf_book`: o LaTeX descarta as caixas em HTML cru,
  as tabelas HTML e os SVGs. Botão de download ativado no gitbook (`download: ["pdf"]`).
  **Regerar o PDF sempre que o livro mudar.**
- O script copia `_bookdown_files/*_files` para junto do HTML (sem isso todo gráfico do R saía em
  branco no PDF, o footgun #2 do CLAUDE.md) e **para com erro** se alguma `<img>` apontar para
  arquivo inexistente. `LIVRO_PDF_BUILD=<pasta>` fixa a pasta de build para inspeção.

### Livro (site)
- `Livro/mathjax-config.html` liga `$...$` no MathJax: toda fórmula dentro das caixas em HTML cru
  aparecia crua no site publicado (8 capítulos).
- Data da capa em português independente do locale (pt_BR não está instalado na máquina).
- **Aviso de direitos autorais**: caixa `caixa-aviso` no início do Prefácio (uso exclusivo dos
  alunos de MATD48, reprodução proibida sem autorização, Lei nº 9.610/1998, plágio, forma de citação,
  versão preliminar em construção); rodapé em toda página do site via `Livro/aviso-rodape.html`
  (incluído em `after_body`); nota na barra lateral (`_output.yml`); rodapé em toda página do PDF
  (`@page @bottom-center` em `scripts/livro-pdf.css`). O mesmo script JS troca o título "References"
  por "Referências" (pôr `reference-section-title` no YAML **quebra** o `split_chapters` do
  bookdown; não tentar de novo).
- Removida `Livro/além-do-programa-do-semestre.html`, página órfã de render antigo.
- Cap. 3, seção "Subamostragem em um DCA": planilha antes do modelo, por que a tabela
  tratamento × réplica não é tabela de contingência nem DBCA, ponte "média da UE segue o DCA" com
  dedução dos E[QM], análise da tilápia por médias de aquário e `lmerTest::lmer` (o `aov` com
  `Error()` dava aviso de singularidade nos dados desbalanceados, contradizendo o próprio texto).

### Terminologia
- "submuestreo"/"submuestras" (espanhol, herdado do livro-fonte) → "subamostragem"/"subamostras"
  em aulas, livro, Lista 05 e home, com concordância de gênero. IDs de seção, rótulos de chunk e
  nomes de arquivo (`dca-submuestreo`, `slide-submuestreo.svg`...) foram mantidos de propósito para
  não quebrar URLs e `\@ref`.

### Revisão de clareza das aulas (pedido do professor: código sem explicação, ideias desconectadas)
Padrão aplicado em cada deck, mantendo os quatro blocos: **mostrar a planilha antes do modelo**,
dizer explicitamente o que a tabela tratamento × réplica é e não é, decompor uma observação real
nos termos do modelo, ligar cada aula à anterior com um exemplo numérico, deduzir em vez de
apresentar pronto, e conferir em R toda afirmação numérica do texto.
- 01: resultados potenciais numa tabela de 6 estudantes (sorteio "azarado" vs. média dos 20
  sorteios), prova depois dos três pilares, contagens vs. medidas (tabela do chá), planilha.
- 02: da planilha para Y e X (exemplo de brinquedo, 3 servidores × 2), por que μ não é estimável.
- 03: projeção no brinquedo (= média do grupo), ANOVA de um fator como modelo particionado
  (SQ 784 + 58), Aitken/Zyskind movidos para o fim e motivados.
- 04: o que é um DCA, sorteio em R, planilha do feijão longa e larga, decomposição da SQ e E[QM].
- 05: reescrita completa (subamostragem): planilha, tabela de médias, decomposição de uma linha,
  ponte média do aquário → DCA, E[QM] deduzidos, tilápia por médias + `lmer`.
- 06: exemplo de ordem de execução tinha ordem **confundida** com tratamento (agora sorteada);
  slides novos "o que acontece se um pressuposto falhar" e "de onde vêm as transformações"
  (método delta, Box, 1954).
- 07: "de uma pergunta para os coeficientes", tabela de coeficientes polinomiais, guia "qual
  procedimento usar".
- 08: médias ajustadas em números (grupo de 50 mg com basal alto por acaso), forma de H como SQ dos
  postos, frase do mediador corrigida.
- 09: sorteio por bloco, "DCA ou DBCA? a mesma tabela, significados diferentes",
  SQ_Erro(DCA) = SQ_Bloco + SQ_Erro(DBCA).

### Bug grave encontrado: o verificador de equações nunca funcionou
`scripts/verificar_equacoes.R` tinha a regex escapada em dobro e **nunca detectava nada**; todo
"Nenhuma" anterior a 12/10/2026 não vale. Corrigido e testado contra um deck quebrado de propósito.
Ao rodá-lo corrigido apareceram **34 slides com LaTeX cru já publicados** (aulas 07, 09–14), causados
por `$$` em várias linhas e `$` colado a palavras. Todos corrigidos; documentado como footgun #10 no
CLAUDE.md. Lição: teste o verificador contra um caso positivo antes de confiar num "zero".

### Outros
- `pagedown` instalado (CRAN).
- Scripts de Python que editam `.Rmd` devem usar strings raw (`r'''...'''`): numa string comum
  `\alpha` vira o caractere de controle BEL e `\tfrac` vira TAB (aconteceu na aula 02; o verificador
  de equações não pega isso). Procurar com `grep -P '\x07|\x08|\t'`.

## O que falta

1. Revisão de clareza das aulas 10–14 (lote 3), e as seções correspondentes do livro, se o professor
   quiser.
2. Pontos antigos ainda sem segunda revisão humana: cap. 7 do livro inteiro; nenhuma leitura humana
   corrida do livro completo depois das expansões.
3. Um detalhe menor: no cap. 3, o texto inline mostra `F ≈ 12.98` com ponto decimal.

## Como retomar

1. Ler o pedido do professor. Conteúdo em português.
2. Editar aqui, re-renderizar, **conferir o artefato** (abrir HTML/PNG/PDF), não só o exit code.
3. Sincronizar com o clone (comandos acima, sem `--delete`).
4. Commitar e pushar só com OK explícito do professor (o push publica no GitHub Pages).

### Comandos de verificação

```bash
# Livro -- sempre os 3 passos, nesta ordem, e nunca dois renders do Livro/ em paralelo
cd Livro
rm -rf _bookdown_files *.md 0*_files
Rscript -e 'bookdown::render_book("index.Rmd", quiet=TRUE)'
for d in _bookdown_files/*_files; do cp -r "$d" "./$(basename "$d")"; done
cd .. && Rscript scripts/gerar_pdf_livro.R      # PDF (para com erro se faltar figura)

# imagens quebradas e \@ref cru no site
cd Livro
python3 -c "
import re, glob, os
total = missing = 0
for fn in glob.glob('*.html'):
    for src in re.findall(r'<img src=\"([^\"]+)\"', open(fn, encoding='utf-8', errors='ignore').read()):
        total += 1
        if not src.startswith('http') and not os.path.exists(src.split('?')[0]):
            missing += 1; print('FALTANDO:', fn, src)
print(f'{total} imagens, {missing} faltando')
"
grep -l '@ref(' *.html || echo "0 referencias quebradas"
cd ..

# Aulas -- um deck por processo R (footgun #5)
for i in $(seq -w 1 14); do Rscript -e "rmarkdown::render('Aulas2026/MATD48-$i.Rmd', quiet=TRUE)"; done
Rscript scripts/verificar_slides.R      # transbordo
Rscript scripts/verificar_equacoes.R    # LaTeX cru (corrigido em 12/10/2026)
```

Na sessão de 12/10/2026 o estado final foi: 14 decks sem transbordo e sem LaTeX cru; livro com
77/77 imagens, 0 `\@ref` quebrado, 0 tabela com legenda duplicada; PDF com 217 páginas, 75 imagens,
aviso e rodapé em todas as páginas.
