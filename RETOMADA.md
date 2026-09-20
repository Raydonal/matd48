# Retomada — MATD48 2026 (reescrita completa do curso)

**Última atualização:** 2026-09-19, ~21:20 (horário local). Estado: **commitado e pushado para o
GitHub**. Este arquivo existe para retomar o trabalho sem precisar reconstruir o contexto do zero.
(Substitui a versão anterior, de 2026-08-13, que descrevia um estado pré-commit e uma estrutura de
diretórios que não existe mais — ver "Mudança estrutural grande" abaixo. Histórico completo,
rodada a rodada, em `PLANO_CONTEUDO.md`.)

## Onde as coisas estão (isto mudou — leia antes de qualquer coisa)

Esta cópia (`/home/raydonal/MEGA/Claude/matd48`, sincronizada via MEGA) **não tem `.git`**. O
repositório git real, com histórico e remoto, é `/home/raydonal/Github/Cursos/matd48` (remoto
`git@github.com:Raydonal/matd48.git`, branch `main`). O fluxo normal é: editar/renderizar aqui (ou
lá), sincronizar as duas cópias (`rsync`, ver comando abaixo), commitar e pushar a partir do clone.
`CLAUDE.md` documenta isso na seção "Working copy has no local `.git`".

```bash
rsync -a --delete \
  --exclude='.git' --exclude='.claude' --exclude='.gitignore' \
  --exclude='cosa.txt' --exclude='Livro/_bookdown_files' \
  /home/raydonal/MEGA/Claude/matd48/ /home/raydonal/Github/Cursos/matd48/
```

## Backup

Snapshot completo do clone git (com `.git`, histórico incluído — restaura tudo, inclusive commits)
em: `/home/raydonal/MEGA/Claude/matd48-backups/matd48_backup_20260919-2121.tar.gz` (267 MB,
integridade de gzip verificada). Backups anteriores (mais antigos, de antes da reestruturação de
setembro) ainda preservados no mesmo diretório e em `/home/raydonal/Github/Cursos/matd48-backups/`.
Gere um novo antes de qualquer mudança grande futura:

```bash
TS=$(date +%Y%m%d-%H%M)
cd /home/raydonal/Github/Cursos
tar --exclude='matd48/Livro/_bookdown_files' -czf "/home/raydonal/MEGA/Claude/matd48-backups/matd48_backup_${TS}.tar.gz" matd48
gzip -t "/home/raydonal/MEGA/Claude/matd48-backups/matd48_backup_${TS}.tar.gz" && echo OK
```

Separado disso: `/home/raydonal/Github/Cursos/matd48-arquivo-nao-publico/` guarda uma cópia completa
de tudo que foi **removido do repositório público** em setembro/2026 (ver seção abaixo) —
`Aulas-2025/`, `index-2025.Rmd/.html`, `Material/`, `ExperimentalDesign/`, `ApoioLuz/`. Nunca vai
para o GitHub; é só para referência local do professor.

## Mudança estrutural grande (setembro/2026) — o que aconteceu e por quê

O professor pediu para restringir o **site público** a livro + slides + listas de exercícios +
projetos, e tirar do ar o material do ano passado (que estava confundindo os alunos) e qualquer
PDF de livro com direito de autor — sem perder nada, guardando cópia completa fora do repositório.
Como o GitHub Pages serve **qualquer** arquivo do repo por URL direta (link ou não), a única forma
de cumprir isso de verdade foi remover os diretórios do repositório (não só tirar o link da home).

O que saiu do repositório público (commit `011b851`, `git rm`, cópia completa preservada em
`matd48-arquivo-nao-publico/`):
- `Aulas/` — material de 2025 (também guardava assets que `Aulas2026/`/`Livro/` ainda usavam —
  ver próximo parágrafo).
- `index-2025.Rmd`/`.html` — a home antiga.
- `Material/`, `ExperimentalDesign/`, `ApoioLuz/` — PDFs de livros com copyright (Montgomery,
  Box/Hunter/Hunter, Kuehl, Luz Mery González García) e material de terceiros.

`Aulas/` também era o repositório de assets compartilhados (`images/`, `refs.bib`, `apa.csl`,
`custom-styles.css`, `Bairros_Recife/` shapefile, 5 CSVs linkados na home) que `Aulas2026/*.Rmd` e
`Livro/*.Rmd` referenciavam por caminho relativo. Antes de remover, migrei só os arquivos
efetivamente usados para uma pasta nova, **`Aulas2026/assets/`**, e atualizei todo caminho relativo
(`../Aulas/...` → `assets/...` em `Aulas2026/`, `../Aulas2026/assets/...` em `Livro/`). Depois
re-renderizei tudo (livro + 14 decks + home) e revalidei do zero: 77/77 imagens, 0 `@ref()` não
resolvido, 0 citação crua nos slides, 0 slide com transbordo. Ver `CLAUDE.md` → "How the site is
published, and what is public" para os detalhes e para a lista do que **não** resgatar sem OK do
professor.

`CLAUDE.md` também deixou de ser versionado (`git rm --cached` + `.gitignore`) — fica só no disco
local, porque o professor não quer nenhum traço de "informação do Claude" alcançável no site
publicado. Não commitar esse arquivo de volta.

**Importante:** isso não reescreve o histórico antigo do git. Quem olhar commits anteriores a
`011b851` no GitHub (ou clonar e voltar no tempo) ainda encontra `Aulas/2025` e os PDFs — só a
versão atual do site (a que o GitHub Pages serve agora) parou de servi-los. O professor foi avisado
disso explicitamente antes do push e concordou.

## Estrutura atual do repositório público

1. **`Livro/`** — bookdown, 8 capítulos (Princípios, Modelos lineares, DCA, Blocos, Fatoriais,
   Fatoriais avançados, Superfície de resposta, A/B testing e bandits — o 8º é além do programa do
   semestre). Ver `CLAUDE.md` → "Directory layout" para o detalhe de cada um.
2. **`Aulas2026/`** — 14 decks xaringan (`MATD48-01` a `14`), Teoria→Aplicação→Discussão→Uso do R,
   mais `Aulas2026/assets/` (ver acima).
3. **`Listas2026/`** — 14 listas em LaTeX, prática, sem gabarito público (gabaritos ficam em
   `/home/raydonal/Github/Cursos/matd48-gabaritos-privados/`, decisão de agosto/2026).
4. **`Projetos/Projeto-I/II/III/IV`** — Projeto I é referência histórica (prova 2025); II/III/IV são
   os 3 projetos-desafio incrementais que fecham N1/N2/N3 (40% projeto + 60% prova escrita cada,
   sistema atual — não é mais o 50/50 de agosto).
5. **`index.Rmd`** — homepage, cronograma 2026.2, sem link para `index-2025.html` nem para PDFs de
   `Material/`.

## Estado do git

Branch `main` do clone está sincronizada com `origin/main` (push feito, commit `011b851`). Working
tree limpa. Nada pendente de commit no momento em que este arquivo foi escrito.

## O que falta — pendente de revisão humana

Nada foi pedido explicitamente para a próxima rodada. Pontos que ainda não tiveram uma segunda
verificação independente, deixados de rodadas anteriores (ver `PLANO_CONTEUDO.md` para o histórico
completo):
- Cap.7 (Superfície de Resposta) do livro é o conteúdo mais antigo sem segunda revisão humana
  ponta-a-ponta.
- Nenhuma leitura humana corrida do livro inteiro aconteceu ainda depois de todas as rodadas de
  expansão — só verificação automatizada (imagens, `@ref`, citações, transbordo) e local por seção.
- Confirmar se o professor quer distribuir o conteúdo do antigo `Material/`/`ExperimentalDesign/`
  por outro canal (não mais público no site) — hoje só ficam como referência em texto na
  bibliografia da home, sem link para o PDF.

## Como retomar

1. Ler o pedido específico do professor.
2. Se for edição pontual: editar aqui (MEGA) ou no clone, re-renderizar (comandos abaixo), conferir
   **abrindo o HTML/PNG gerado**, não só checando exit code. Depois sincronizar as duas cópias
   (comando `rsync` no topo deste arquivo) antes de commitar no clone.
3. Se envolver adicionar/mover arquivos que outros `.Rmd` referenciam por caminho relativo, grep por
   todo uso do caminho antigo antes de mover — foi assim que a reestruturação de setembro descobriu
   que `Aulas/` guardava assets ainda em uso (ver acima).
4. Só commitar quando o professor pedir explicitamente. Push só depois de confirmar com ele
   (ação visível/pública, ver "Executing actions with care").

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

# Aulas2026 (xaringan) -- um deck por processo R (footgun #5 do CLAUDE.md)
cd ../Aulas2026
for i in $(seq -w 1 14); do Rscript -e "rmarkdown::render('MATD48-$i.Rmd', quiet=TRUE)"; done
# checagem de citação crua sobrando em qualquer slide
for f in MATD48-*.html; do grep -o '\[@[a-zA-Z0-9_]*\]' "$f" && echo "quebrado em $f"; done

# transbordo de slide (Chrome headless)
cd ..
Rscript scripts/verificar_slides.R

# home do curso
Rscript -e 'rmarkdown::render("index.Rmd", quiet=TRUE)'
```

## Histórico completo

Toda rodada de trabalho (o que foi pedido, o que foi feito, o que foi verificado) está registrada
em `PLANO_CONTEUDO.md`, na seção "Status", em ordem cronológica — é o log detalhado por trás deste
resumo.
