# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository purpose

This is the course website/content repository for **MATD48 — Planejamento de Experimentos A** (Design of Experiments), taught by Prof. Raydonal Ospina Martínez at UFBA. It is not a software application — there is no build, lint, or test tooling. The repository is a collection of R Markdown source files and their rendered output (HTML/PDF), published via GitHub Pages at `https://raydonal.github.io/matd48/`.

Content and comments in this repo are primarily in **Portuguese**. Keep new content consistent with that.

## How the site is published

GitHub Pages serves directly from the root of `main` (there is no `gh-pages` branch, no `_site.yml`, no CI workflow). `index.html` at the repo root is the homepage and is rendered from `index.Rmd`. Rendering is done locally in RStudio/R and the resulting HTML/PDF output files are committed alongside their `.Rmd` sources — **whenever you edit a `.Rmd`, the corresponding output file(s) need to be re-knit and committed too**, since there is no automated pipeline to do this.

## Rendering R Markdown

There's no single project-wide render script. Each `.Rmd` declares its own output format in its YAML front matter and is knit individually, e.g. from an R console:

```r
rmarkdown::render("Aulas/MATD48-05.Rmd")
```

Formats used across the repo, by directory convention:
- `index.Rmd` (root) — `prettydoc::html_pretty` (course syllabus/homepage)
- `Aulas/MATD48-*.Rmd` (lecture slides) — `xaringan::moon_reader` (reveal-style HTML slides), most cite `refs.bib` via `csl: apa.csl`
- `Projetos/*.Rmd` — mixed `html_document` (custom CSS in `Projetos/config/`) and `pdf_document` output, used for assignments/projects

Rendering these files requires a working R + RStudio/pandoc/LaTeX environment with the packages loaded in each file's setup chunk (tidyverse, xaringan, prettydoc, kableExtra, gamlss family, car, lmtest, etc.). Do not assume this environment is available in a plain shell — flag to the user if a render is requested but R is not installed.

## Directory layout

- `Aulas/` — lecture materials: numbered `MATD48-01.Rmd` through `MATD48-12.Rmd` (source) with matching `.html` (knit output) and `MATD48-NN_files/` (xaringan/knitr dependency assets, do not hand-edit). Also holds shared assets used across lectures: `images/`, `data/` (raw datasets like Montgomery textbook data), `www/` (shared CSS/JS/SVG for slides), `refs.bib` + `apa.csl` (citations), `custom-styles.css`. `misc/` and `Misc/` are unrelated (draft Rmd variants vs. reference PDFs, respectively) — don't conflate them.
- `Projetos/` — student project/assignment specs (`Projeto-I.Rmd`/`.html`), with shared styling in `Projetos/config/`.
- `Material/` — reference textbooks and papers (PDFs) on design of experiments (Montgomery, Box/Hunter/Hunter, Oehlert, etc.) — read-only reference material, not course output.
- `ExperimentalDesign/` — supplementary reference PDFs and a vendored `university-notes-master` bookdown project (third-party notes, not authored here).
- `Configuracoes/` — misc course logistics assets (sampling PDF, gif).
- `ApoioLuz/` — untracked working folder (see `git status`) with a corrected thesis/course PDF and a dataset; treat as in-progress material, not yet integrated into the published site.

## Working with `.Rmd` files

- Each lecture Rmd's setup chunk loads a long, fairly fixed list of packages (tidyverse, ggpubr, MASS, car, lmtest, kableExtra, gamlss + extensions, broom, cowplot, viridis, latex2exp, etc.) — match this style when adding chunks rather than introducing new plotting/analysis idioms.
- Slide decks (`xaringan::moon_reader`) use `--` / `---` slide separators and Markdown incremental-reveal conventions; check a neighboring lecture file for the established pattern (fragment classes, `class:` slide attributes, `background-image` usage) before adding new slides.
- Simulated/didactic datasets are often generated inline with `set.seed()` for reproducibility (see `index.Rmd`) rather than loaded from file — follow this pattern for new teaching examples unless real data (from `Aulas/data/`) is the point of the example.
