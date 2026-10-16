#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# gerar_pdf_livro.R -- gera Livro/livro-doe.pdf com LaTeX (xelatex), localmente.
#
# O livro foi escrito para o site (gitbook) e tem três coisas que o LaTeX descartaria EM
# SILÊNCIO num bookdown::pdf_book ingênuo. Este script trata as três:
#   1. caixas em ```{=html} (aplicação, discussão, uso do R, aviso) -> filtro
#      scripts/caixas-latex.lua converte cada uma num ambiente tcolorbox;
#   2. tabelas kable forçadas para HTML (options(knitr.table.format = "html"), footgun #8)
#      -> na CÓPIA de build a opção vira "latex" quando a saída é PDF, e tabelas largas são
#      reduzidas para caber na página (kable_styling(latex_options = "scale_down"));
#   3. diagramas de Hasse em SVG -> hasse_helpers.R devolve um PDF do Graphviz quando a
#      saída é LaTeX; SVGs estáticos de Aulas2026/assets/images são convertidos com o Inkscape.
# Depois do render, o script CONFERE que o PDF tem o mesmo número de caixas e figuras que o
# HTML-fonte e só então substitui Livro/livro-doe.pdf (o PDF anterior nunca é apagado antes).
#
# Renderiza numa CÓPIA para não tocar no site (o livro usa output_dir: "." -- footguns 1 e 2).
# uso (da raiz do repositório):  Rscript scripts/gerar_pdf_livro.R
# requer: xelatex (TeX Live), Graphviz (dot), Inkscape, pandoc >= 2.11 (RSTUDIO_PANDOC)
# alternativa antiga, via Chrome/pagedown: scripts/gerar_pdf_livro_chrome.R
# ---------------------------------------------------------------------------
raiz <- normalizePath(".")
stopifnot(file.exists(file.path(raiz, "Livro", "_bookdown.yml")))

build <- Sys.getenv("LIVRO_PDF_BUILD", file.path(tempdir(), "livro-pdf-latex"))
unlink(build, recursive = TRUE)
dir.create(file.path(build, "Aulas2026"), recursive = TRUE)
file.copy(file.path(raiz, "Aulas2026", "assets"), file.path(build, "Aulas2026"), recursive = TRUE)
dir.create(file.path(build, "Livro"))
fontes <- c(Sys.glob(file.path(raiz, "Livro", "*.Rmd")),
            file.path(raiz, "Livro", c("_bookdown.yml", "refs.bib", "hasse_helpers.R",
                                       "comparacoes_helpers.R", "controle_helpers.R", "data", "figuras")))
file.copy(fontes, file.path(build, "Livro"), recursive = TRUE)
file.copy(file.path(raiz, "scripts", c("caixas-latex.lua", "livro-latex-preamble.tex",
                                     "livro-latex-setup.R")),
          file.path(build, "Livro"))

# (3) SVGs estáticos -> PDF
imgs <- file.path(build, "Aulas2026", "assets", "images")
for (svg in Sys.glob(file.path(imgs, "*.svg"))) {
  pdf_svg <- sub("\\.svg$", ".pdf", svg)
  st <- system2("inkscape", c(shQuote(svg), "--export-type=pdf",
                              paste0("--export-filename=", shQuote(pdf_svg))), stdout = FALSE, stderr = FALSE)
  if (st != 0 || !file.exists(pdf_svg)) stop("Inkscape não converteu ", basename(svg))
}

setwd(file.path(build, "Livro"))
rmds <- Sys.glob("*.Rmd")
for (f in rmds) {
  s <- readLines(f, warn = FALSE, encoding = "UTF-8")
  # (2) tabelas em LaTeX, reduzidas se não couberem na largura da página
  i <- grep('^options\\(knitr\\.table\\.format = "html"\\)', s)
  if (length(i)) s[i] <- 'source("livro-latex-setup.R")'   # tabelas, cairo, legendas
  s <- gsub('format = "html"', 'format = "latex"', s, fixed = TRUE)
  # (3) SVG estático incluído direto -> a versão PDF
  s <- gsub('(assets/images/[^"]+)\\.svg"', '\\1.pdf"', s)
  writeLines(s, f, useBytes = TRUE)
}
# bibliografia como capítulo próprio no fim do livro
ultimo <- tail(yaml::read_yaml("_bookdown.yml")$rmd_files, 1)
cat('\n\n# Referências {-}\n\n::: {#refs}\n:::\n', file = ultimo, append = TRUE)

# O render do bookdown já roda o LaTeX, mas falha nas figuras: os gráficos ficam em
# _bookdown_files/<cap>_files/ e o .tex os procura em <cap>_files/ (footgun #2). Por isso o
# primeiro LaTeX pode falhar; copiam-se as figuras e compila-se de novo, à mão.
render_ok <- tryCatch({ bookdown::render_book(
  "index.Rmd",
  output_format = bookdown::pdf_book(
    latex_engine = "xelatex", toc = TRUE, toc_depth = 2, number_sections = TRUE,
    keep_tex = TRUE, citation_package = "default",
    includes = rmarkdown::includes(in_header = "livro-latex-preamble.tex"),
    pandoc_args = c("--lua-filter", "caixas-latex.lua", "--top-level-division=chapter",
                    "--metadata", "lang=pt-BR", "--metadata", "documentclass=book",
                    "--metadata", "classoption=oneside",
                    "--metadata", "geometry=a4paper,margin=2.3cm",
                    "--metadata", "mainfont=DejaVu Serif", "--metadata", "sansfont=DejaVu Sans",
                    "--metadata", "monofont=DejaVu Sans Mono", "--metadata", "fontsize=10pt",
                    "--metadata", "linkcolor=blue", "--metadata", "urlcolor=blue")
  ),
  output_dir = "_pdf", quiet = TRUE
); TRUE }, error = function(e) { message("1º LaTeX falhou (esperado se faltaram figuras): ",
                                        conditionMessage(e)); FALSE })
dir_tex <- if (file.exists(file.path("_pdf", "livro-doe.tex"))) "_pdf" else "."
if (!render_ok) {
  for (d in Sys.glob("_bookdown_files/*_files")) {
    file.copy(d, ".", recursive = TRUE); file.copy(d, "_pdf", recursive = TRUE)
  }
  owd <- setwd(dir_tex); tinytex::latexmk("livro-doe.tex", engine = "xelatex", clean = FALSE)
  setwd(owd)
}
saida <- normalizePath(file.path(dir_tex, "livro-doe.pdf"))

# --- conferência: nada do conteúdo do site pode ter sumido ---------------------------------
tex <- readLines(file.path(dir_tex, "livro-doe.tex"), warn = FALSE, encoding = "UTF-8")
n_caixas_tex <- length(grep("^\\\\begin\\{caixa", tex))
n_caixas_rmd <- sum(sapply(rmds, function(f)
  length(grep('<div class="caixa-', readLines(f, warn = FALSE, encoding = "UTF-8")))))
n_fig_tex <- length(grep("\\\\includegraphics", tex))
cat(sprintf("caixas: %d no fonte, %d no LaTeX | figuras no LaTeX: %d\n",
            n_caixas_rmd, n_caixas_tex, n_fig_tex))
if (n_caixas_tex != n_caixas_rmd) stop("Há caixas do livro que não chegaram ao PDF.")
arq_log <- file.path(dir_tex, "livro-doe.log")
log <- if (file.exists(arq_log)) readLines(arq_log, warn = FALSE) else character()
faltando <- grep("Missing character", log, value = TRUE)
if (length(faltando)) {
  cat("AVISO: caracteres sem glifo na fonte:\n"); print(unique(faltando))
}

destino <- file.path(raiz, "Livro", "livro-doe.pdf")
file.copy(saida, destino, overwrite = TRUE)
cat("PDF gerado:", destino, sprintf("(%.1f MB)\n", file.size(destino) / 2^20))
