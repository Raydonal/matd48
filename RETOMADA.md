# Retomada — MATD48 2026

**Última atualização:** 2026-10-12 (sessão de revisão de clareza das aulas, PDF do livro e aviso
de direitos autorais). Este arquivo existe para retomar o trabalho sem reconstruir o contexto do
zero. Histórico completo, rodada a rodada, em `PLANO_CONTEUDO.md` (seção "Status").

## Estado em que esta sessão parou (leia primeiro)

- **Publicado no GitHub** (commits `a97c61a` e `d00f53f`, 12/10): PDF do livro, fórmulas nas caixas
  do livro, aula 05 reescrita, aulas 01–04 revisadas, troca "submuestreo" → "subamostragem".
- **Sincronizado no clone, mas NÃO commitado nem pushado** (aguardando aprovação do professor para
  publicar): aulas 06–09 revisadas, correção de fórmulas cruas nas aulas 07 e 09–14, seção de
  subamostragem do cap. 3 do livro, aviso de direitos autorais no livro (site e PDF), PDF com as
  figuras de volta, correção do `scripts/verificar_equacoes.R`. Para publicar: no clone,
  `git add -A Livro Aulas2026 scripts && git commit && git push`.
- **Pergunta em aberto ao professor:** pôr o mesmo aviso de direitos autorais também nos slides
  (`Aulas2026/`) e nas listas (`Listas2026/`)?
- **Revisão de clareza das aulas:** feitas 01–09 (lotes 1 e 2). **Faltam 10–14** (lote 3), no mesmo
  padrão descrito abaixo.

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

1. Publicar o que está sincronizado e não commitado (depende de OK do professor).
2. Revisão de clareza das aulas 10–14 (lote 3), e as seções correspondentes do livro, se o professor
   quiser.
3. Decidir sobre o aviso de direitos autorais nos slides e nas listas.
4. Pontos antigos ainda sem segunda revisão humana: cap. 7 do livro inteiro; nenhuma leitura humana
   corrida do livro completo depois das expansões.
5. Um detalhe menor: no cap. 3, o texto inline mostra `F ≈ 12.98` com ponto decimal.

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
