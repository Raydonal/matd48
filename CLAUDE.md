# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository purpose

This is the course website/content repository for **MATD48 — Planejamento de Experimentos A** (Design of Experiments), taught by Prof. Raydonal Ospina Martínez at UFBA. It is not a software application — there is no build, lint, or test tooling. The repository is a collection of R Markdown source files and their rendered output (HTML/PDF), published via GitHub Pages at `https://raydonal.github.io/matd48/`.

Content and comments in this repo are primarily in **Portuguese**. Keep new content consistent with that.

## How the site is published

GitHub Pages serves directly from the root of `main` (there is no `gh-pages` branch, no `_site.yml`, no CI workflow). `index.html` at the repo root is the homepage and is rendered from `index.Rmd`. Rendering is done locally in RStudio/R and the resulting HTML/PDF output files are committed alongside their `.Rmd` sources — **whenever you edit a `.Rmd`, the corresponding output file(s) need to be re-knit and committed too**, since there is no automated pipeline to do this.

The 2026 rewrite of the course (see below) archived the previous homepage byte-for-byte as `index-2025.Rmd`/`index-2025.html` before `index.Rmd`/`index.html` were replaced — follow the same pattern (copy, don't overwrite) before any future major homepage rewrite, so past semesters stay browsable.

## Rendering R Markdown

There's no single project-wide render script. Each `.Rmd` declares its own output format in its YAML front matter and is knit individually, e.g. from an R console:

```r
rmarkdown::render("Aulas/MATD48-05.Rmd")
bookdown::render_book("Livro/index.Rmd")   # renders the whole book; always rm -rf Livro/_bookdown_files first (see below)
```

Formats used across the repo, by directory convention:
- `index.Rmd` (root) — `prettydoc::html_pretty` (course syllabus/homepage)
- `Aulas/MATD48-*.Rmd`, `Aulas2026/MATD48-*.Rmd` (lecture slides) — `xaringan::moon_reader` (reveal-style HTML slides), cite `refs.bib` via `csl: apa.csl`
- `Livro/*.Rmd` — `bookdown::gitbook`, one chapter per `0N-slug.Rmd`, listed explicitly in `Livro/_bookdown.yml` (`rmd_files:`); renders in place (`output_dir: "."`) so HTML sits next to the `.Rmd` sources, matching the rest of the repo
- `Projetos/*.Rmd` — mixed `html_document` (custom CSS in `Projetos/config/`) and `pdf_document` output, used for assignments/projects
- `Listas2026/*.tex` — plain LaTeX (not knitr), compiled with `latexmk -pdf`, sharing `Listas2026/preamble.tex`

Rendering these files requires a working R + RStudio/pandoc/LaTeX environment with the packages loaded in each file's setup chunk (tidyverse, xaringan, bookdown, prettydoc, kableExtra, gamlss family, car, lmtest, etc.). Do not assume this environment is available in a plain shell — flag to the user if a render is requested but R is not installed.

**Known footgun:** `Livro/_bookdown_files/` (bookdown's gitbook resource-copy cache) will self-nest into `_bookdown_files/_bookdown_files/...` and balloon to hundreds of MB if two `bookdown::render_book()` calls run concurrently against the same `Livro/` directory (a race in gitbook's "copy resources to output_dir" step, triggered because `output_dir` is `"."`, the same as the source dir). It's gitignored and safe to delete; always `rm -rf Livro/_bookdown_files` before rendering the book, and avoid running two renders of `Livro/` in parallel.

**Known footgun #2 (figures silently missing from the published book):** with this same `output_dir: "."` configuration, `bookdown::render_book()` does **not** reliably copy each chapter's generated figures (`0N-chapter_files/figure-html/*.png`) out of `_bookdown_files/` to the top level of `Livro/` — the final `.html` pages link to `0N-chapter_files/figure-html/...` (no `_bookdown_files/` prefix), but the PNGs may only exist nested inside `_bookdown_files/0N-chapter_files/figure-html/...`, so every knitr-generated plot/diagram renders as a broken image (plain `../Aulas/images/*.png` references are unaffected, since those are static files, not knitr output). This reproduces even on a fully clean render (confirmed by deleting `_bookdown_files/`, all `*.md`, and all `0N-chapter_files/` before rendering). **After every `bookdown::render_book()` call, verify and fix with:**
```bash
cd Livro
for d in _bookdown_files/*_files; do cp -r "$d" "./$(basename "$d")"; done
```
then confirm no broken images remain, e.g. by checking that every `<img src="...">` in `Livro/*.html` resolves to a real file. These copied `0N-chapter_files/` directories are **not** gitignored (only `_bookdown_files/` is) — commit them alongside the `.html`, matching the repo's "HTML output sits next to `.Rmd` sources" convention.

**Known footgun #3 (`\@ref()` cross-references silently unresolved):** bookdown only resolves `\@ref(...)` inside normal Markdown prose. It is **not** resolved — and appears as literal broken text like `Figura \@ref(fig:xyz)` in the published HTML — in three contexts: (a) inside a chunk's `fig.cap="..."` string (even when referencing a *different* figure's label); (b) inside an R code comment (`# ...`) in a chunk, since comments are displayed verbatim; (c) inside a raw ```` ```{=html} ```` block (the `caixa-aplicacao`/`caixa-discussao`/`caixa-r` boxes used throughout `Livro/`). In all three cases, write the section/figure reference as plain prose (e.g. "ver a seção sobre diagramas de Hasse") instead of `\@ref(...)`. This same caveat applies to `Aulas2026/*.Rmd`: those are standalone xaringan decks, not part of the bookdown site, so `\@ref(...)` is *never* resolved there — don't use bookdown cross-reference syntax to point at the book from a slide deck, only plain text (e.g. `(Seção "O modelo linear particionado" do livro)`).

## Directory layout

- `Aulas/` — original (pre-2026) lecture materials: numbered `MATD48-01.Rmd` through `MATD48-12.Rmd` (source) with matching `.html` (knit output) and `MATD48-NN_files/` (xaringan/knitr dependency assets, do not hand-edit). Also holds shared assets reused by `Aulas2026/` via relative path (`../Aulas/...`): `images/`, `data/` (raw datasets like Montgomery textbook data), `www/` (shared CSS/JS/SVG for slides), `refs.bib` + `apa.csl` (citations, shared across both `Aulas/` and `Aulas2026/`), `custom-styles.css`. `misc/` and `Misc/` are unrelated (draft Rmd variants vs. reference PDFs, respectively) — don't conflate them.
- `Aulas2026/` — the rewritten 2026 lecture deck (`MATD48-01.Rmd` … `MATD48-14.Rmd`, xaringan), each structured in four explicit blocks in this order: **Teoria → Aplicação → Discussão → Uso do R**. Reuses `Aulas/images|www|refs.bib|apa.csl|custom-styles.css` by relative path rather than duplicating assets.
- `Livro/` — bookdown textbook (`index.Rmd` + `01-principios.Rmd` … `06-fatoriais-avancado.Rmd`), the primary 2026 reference text, mirroring the topic progression of `ApoioLuz/`'s source PDF but with **entirely original PT-BR prose** (see `PLANO_CONTEUDO.md`'s "nota de originalidade" — never translate the source PDF verbatim). Has no exercises of its own by design; exercises live only in `Listas2026/`. `Livro/data/*.csv` holds real datasets extracted from `ApoioLuz/BasesDatosDE.xlsx`.
- `Listas2026/` — weekly exercise lists (`ListaNN.tex`) with **answer keys kept in separate files** (`GabaritoNN.tex`), one pair per `Aulas2026` lecture, sharing `preamble.tex`.
- `PLANO_CONTEUDO.md` (repo root) — the living content map for the `Livro/`+`Aulas2026/`+`Listas2026/` project: chapter↔aula↔lista alignment, which datasets/domains (psychology/agriculture/data science) go where, and the required rigor bar (matrix notation, Neyman-Rubin causal framing, a proof/derivation question per list, discussed graphics). Read it before adding to any of these three directories.
- `Projetos/` — student project/assignment specs (`Projeto-I.Rmd`/`.html`), with shared styling in `Projetos/config/`.
- `Material/` — reference textbooks and papers (PDFs) on design of experiments (Montgomery, Box/Hunter/Hunter, Oehlert, etc.) — read-only reference material, not course output.
- `ExperimentalDesign/` — supplementary reference PDFs and a vendored `university-notes-master` bookdown project (third-party notes, not authored here).
- `Configuracoes/` — misc course logistics assets (sampling PDF, gif).
- `ApoioLuz/` — untracked working folder (see `git status`) holding the Luz Mery González García DOE textbook PDF (Spanish) used only as a topic-progression reference for `Livro/`, plus `BasesDatosDE.xlsx` (source of `Livro/data/*.csv`) — never copy its prose directly into the repo's own content.

## Working with `.Rmd` files

- Each lecture Rmd's setup chunk loads a long, fairly fixed list of packages (tidyverse, ggpubr, MASS, car, lmtest, kableExtra, gamlss + extensions, broom, cowplot, viridis, latex2exp, etc.) — match this style when adding chunks rather than introducing new plotting/analysis idioms.
- Slide decks (`xaringan::moon_reader`) use `--` / `---` slide separators and Markdown incremental-reveal conventions; check a neighboring lecture file for the established pattern (fragment classes, `class:` slide attributes, `background-image` usage) before adding new slides.
- Simulated/didactic datasets are often generated inline with `set.seed()` for reproducibility (see `index.Rmd`) rather than loaded from file — follow this pattern for new teaching examples unless real data (from `Aulas/data/` or `Livro/data/`) is the point of the example.
- For `Livro/`, `Aulas2026/`, `Listas2026/`: this is graduate-level content by explicit instructor request — no simplified/"intro" treatment. Every model gets matrix notation alongside scalar notation, every treatment-comparison example gets the potential-outcomes/Neyman-Rubin causal framing, every fitted model gets a discussed (not bare) graphic, and every exercise list has at least one proof/derivation question. Full rationale in `PLANO_CONTEUDO.md`.
