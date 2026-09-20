#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# verificar_equacoes.R -- procura LaTeX CRU visivel nos slides renderizados.
#
# Por que nao basta olhar o .Rmd ou o .html: o xaringan guarda o markdown num
# <textarea id="source"> e o remark.js + MathJax so montam a pagina no
# navegador. Um "$...$" que o remark quebrou (tipicamente por estar partido em
# DUAS linhas do fonte) chega ao aluno como texto literal -- e nenhum grep no
# arquivo pega isso, porque no arquivo o LaTeX esta correto.
#
# Metodo: abre cada slide no Chrome, deixa o MathJax terminar e le o innerText
# do slide visivel, procurando comandos LaTeX que sobraram como texto.
#
# uso: Rscript scripts/verificar_equacoes.R [arquivo.html ...]
# ---------------------------------------------------------------------------
suppressPackageStartupMessages(library(chromote))
args <- commandArgs(trailingOnly = TRUE)
decks <- if (length(args)) args else sort(Sys.glob("Aulas2026/MATD48-*.html"))

b <- ChromoteSession$new()
b$Emulation$setDeviceMetricsOverride(width = 1200, height = 900,
                                     deviceScaleFactor = 1, mobile = FALSE)
padrao <- "\\\\\\\\(mathbf|mathrm|frac|sum|hat|bar|boldsymbol|underbrace|begin|text|sigma|beta|alpha|tau|varepsilon|quad|cdot|times|le|ge|neq|approx|prec)"
achados <- list()
for (d in decks) {
  b$Page$navigate(paste0("file://", normalizePath(d)))
  b$Page$loadEventFired(wait_ = TRUE); Sys.sleep(5)
  n <- b$Runtime$evaluate("slideshow.getSlideCount()")$result$value
  for (i in seq_len(n)) {
    b$Runtime$evaluate(sprintf("slideshow.gotoSlide(%d)", i))
    Sys.sleep(0.35)
    js <- sprintf('(function(){
      var s=document.querySelector(".remark-visible .remark-slide-content");
      if(!s) return "";
      var t=s.innerText;
      var m=t.match(/%s/g);
      if(!m) return "";
      var tit=s.querySelector("h1,h2,h3");
      return (tit?tit.innerText:"(sem titulo)")+"||"+m.slice(0,4).join(" ");
    })()', padrao)
    v <- b$Runtime$evaluate(js)$result$value
    if (nzchar(v)) {
      p <- strsplit(v, "\\|\\|")[[1]]
      achados[[length(achados)+1]] <- data.frame(
        arquivo = basename(d), slide = i,
        titulo = substr(p[1], 1, 46), latex = substr(p[2], 1, 40))
    }
  }
  cat("==>", basename(d), "-", n, "slides\n")
}
b$close()
cat("\n===================== EQUACOES NAO RENDERIZADAS =====================\n")
if (!length(achados)) {
  cat("Nenhuma. Todas as equacoes dos decks renderizam.\n")
} else {
  r <- do.call(rbind, achados)
  print(r, row.names = FALSE)
  cat("\nTOTAL:", nrow(r), "slide(s) com LaTeX cru visivel\n")
}
