# livro-latex-setup.R -- configuração só do PDF do livro (scripts/gerar_pdf_livro.R).
# O script de build troca, na CÓPIA de cada capítulo, a linha
#   options(knitr.table.format = "html")
# por source("livro-latex-setup.R"). Nada disto roda no site.

# Tabelas em LaTeX; as largas são reduzidas para caber na página.
options(knitr.table.format = "latex")
kable_styling <- function(kable_input, ..., latex_options = c("hold_position", "scale_down"))
  kableExtra::kable_styling(kable_input, ..., latex_options = latex_options)

# Gráficos com cairo: o dispositivo pdf() padrão não desenha caracteres como ⁴ ou ≠.
knitr::opts_chunk$set(dev = "cairo_pdf")

# Legendas de figura: o knitr as copia cruas para \caption{}; `_`, `^`, `%`, `&`, `#`, `~`
# quebram o LaTeX. Escapa-se tudo FORA dos trechos de fórmula $...$.
.esc_txt <- function(t) {
  t <- gsub("\\", "\\textbackslash{}", t, fixed = TRUE)
  t <- gsub("([&%#_])", "\\\\\\1", t)
  t <- gsub("^", "\\^{}", t, fixed = TRUE)
  t <- gsub("~", "\\~{}", t, fixed = TRUE)
  t
}
.esc_legenda <- function(x) {
  if (!is.character(x)) return(x)
  vapply(x, function(s) {
    pos <- gregexpr("\\$[^$]+\\$", s)[[1]]
    if (pos[1] == -1) return(.esc_txt(s))
    len <- attr(pos, "match.length"); out <- ""; ini <- 1
    for (k in seq_along(pos)) {
      out <- paste0(out, .esc_txt(substr(s, ini, pos[k] - 1)), substr(s, pos[k], pos[k] + len[k] - 1))
      ini <- pos[k] + len[k]
    }
    paste0(out, .esc_txt(substr(s, ini, nchar(s))))
  }, "", USE.NAMES = FALSE)
}
knitr::opts_hooks$set(fig.cap = function(options) {
  options$fig.cap <- .esc_legenda(options$fig.cap)
  options
})

# Legendas de tabela: o knitr::kable também não as escapa no LaTeX. Uma legenda com "%" ou "_"
# quebra o bloco inteiro -- a tabela sai como texto cru e o \centering dela "vaza" e centraliza
# os parágrafos seguintes do capítulo. Escapa-se do mesmo jeito (fórmulas $...$ preservadas).
kable <- function(x, ..., caption = NULL)
  knitr::kable(x, ..., caption = if (is.null(caption)) NULL else .esc_legenda(caption))

# Saída do R (blocos "## ...") com no máximo ~85 caracteres por linha, para caber na página.
options(width = 85)
