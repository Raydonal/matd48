#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# gerar_pdf_livro.R -- gera Livro/livro-doe.pdf a partir das MESMAS fontes do
# site (gitbook), sem passar por LaTeX.
#
# Por que nao bookdown::pdf_book: o livro usa caixas em HTML cru
# (```{=html} caixa-aplicacao/discussao/r), tabelas kableExtra em HTML
# (options(knitr.table.format = "html"), footgun #8) e diagramas de Hasse em
# SVG. No LaTeX as caixas sao DESCARTADAS em silencio, as tabelas somem e o
# SVG nao entra -- o PDF sairia sem parte do conteudo e com exit code 0.
#
# Metodo: renderiza o livro num diretorio temporario como UM documento
# paginado (pagedown::html_paged, que junta os capitulos e numera paginas e
# sumario) e imprime no Chrome headless (pagedown::chrome_print).
#
# Renderiza numa COPIA para nao sobrescrever o index.html do site (o livro usa
# output_dir: "." -- ver footguns #1 e #2 no CLAUDE.md).
#
# uso (da raiz do repositorio):  Rscript scripts/gerar_pdf_livro.R
# requer: pagedown, Google Chrome/Chromium, internet (MathJax via CDN)
# ---------------------------------------------------------------------------
raiz <- normalizePath(".")
stopifnot(file.exists(file.path(raiz, "Livro", "_bookdown.yml")))

# LIVRO_PDF_BUILD permite fixar (e depois inspecionar) a pasta de build
build <- Sys.getenv("LIVRO_PDF_BUILD", file.path(tempdir(), "livro-pdf"))
unlink(build, recursive = TRUE)
dir.create(file.path(build, "Aulas2026"), recursive = TRUE)
# os capitulos referenciam ../Aulas2026/assets/... (imagens, apa.csl)
file.copy(file.path(raiz, "Aulas2026", "assets"), file.path(build, "Aulas2026"),
          recursive = TRUE)
dir.create(file.path(build, "Livro"))
fontes <- c(Sys.glob(file.path(raiz, "Livro", "*.Rmd")),
            file.path(raiz, "Livro", c("_bookdown.yml", "style.css", "refs.bib",
                                       "hasse_helpers.R", "mathjax-config.html",
                                       "data", "figuras")))
file.copy(fontes, file.path(build, "Livro"), recursive = TRUE)
file.copy(file.path(raiz, "scripts", "livro-pdf.css"), file.path(build, "Livro"))

setwd(file.path(build, "Livro"))
# no gitbook a bibliografia sai por pagina; no PDF ela cairia colada ao fim do
# ultimo capitulo, sem titulo -- da-lhe um capitulo proprio (so nesta copia)
ultimo <- tail(yaml::read_yaml("_bookdown.yml")$rmd_files, 1)
cat('\n\n# Referências {-}\n\n<div id="refs"></div>\n', file = ultimo, append = TRUE)
html <- bookdown::render_book(
  "index.Rmd",
  output_format = pagedown::html_paged(
    css = c("default-fonts", "default-page", "default", "style.css", "livro-pdf.css"),
    toc = TRUE, toc_depth = 2, number_sections = TRUE,
    includes = rmarkdown::includes(in_header = "mathjax-config.html"),
    pandoc_args = c("--metadata", "toc-title=Sumário", "--metadata", "lang=pt-BR"),
    self_contained = FALSE
  ),
  output_dir = "_pdf", quiet = TRUE
)

# Footgun #2 do CLAUDE.md, tambem aqui: o render deixa os PNGs dos graficos em
# _bookdown_files/0N-capitulo_files/, mas o HTML aponta para 0N-capitulo_files/.
# Sem esta copia, todo grafico gerado pelo R sai em branco no PDF.
for (d in Sys.glob("_bookdown_files/*_files"))
  file.copy(d, dirname(html), recursive = TRUE)

# Toda <img> do HTML paginado tem de apontar para um arquivo que existe. Um
# render "limpo" (exit 0) pode deixar figura quebrada -- e no PDF ela some sem aviso.
pagina <- paste(readLines(html, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
srcs <- regmatches(pagina, gregexpr('<img src="[^"]+"', pagina))[[1]]
srcs <- sub('^<img src="', "", sub('"$', "", srcs))
srcs <- srcs[!grepl("^(https?:|data:)", srcs)]
faltando <- srcs[!file.exists(file.path(dirname(html), URLdecode(srcs)))]
cat(length(srcs), "imagens no HTML,", length(faltando), "faltando\n")
if (length(faltando)) stop("Figuras ausentes no PDF:\n", paste(faltando, collapse = "\n"))

pdf <- file.path(raiz, "Livro", "livro-doe.pdf")
pagedown::chrome_print(html, output = pdf, timeout = 600, verbose = 0)
cat("PDF gerado:", pdf, sprintf("(%.1f MB)\n", file.size(pdf) / 2^20))
