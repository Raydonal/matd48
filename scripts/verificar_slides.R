#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# verificar_slides.R — detecta TRANSBORDO de conteudo em slides xaringan/remark
#
# O PROBLEMA
#   Num deck xaringan cada slide e desenhado dentro de `.remark-slide-scaler`,
#   uma caixa de tamanho FIXO (908 x 681 px na razao 4:3 padrao) com
#   `overflow: hidden`, apenas escalada por CSS `transform: scale()` para caber
#   na janela. Tudo o que passa dessa caixa e simplesmente CORTADO — e isso
#   depende da ALTURA RENDERIZADA (saida de chunks R, tabelas kableExtra,
#   figuras, equacoes MathJax), que nenhuma checagem textual do .Rmd preve.
#
# O QUE **NAO** FUNCIONA (testado)
#   * `el.scrollHeight - el.clientHeight` no `.remark-slide-content`: da SEMPRE
#     zero. O `.remark-slide-content` tem `overflow: visible` e altura que
#     CRESCE com o conteudo, entao nunca ha "scroll overflow"; quem corta e o
#     ancestral `.remark-slide-scaler`.
#   * comparar os filhos com o bottom do proprio `.remark-slide-content`: pela
#     mesma razao, o resultado fica preso em ~-35 px (padding) por mais que o
#     conteudo transborde.
#
# O QUE FUNCIONA (metodo usado aqui)
#   Referencia = retangulo do `.remark-slide-scaler` (a caixa que corta).
#   Para o slide visivel percorre-se TODOS os descendentes de
#   `.remark-slide-content` e mede-se `getBoundingClientRect()`, ignorando
#   position:absolute/fixed (numero do slide), display:none/visibility:hidden e
#   caixas de area zero, e recortando cada retangulo pelos ancestrais com
#   `overflow` diferente de visible (para nao acusar conteudo ja clipado
#   internamente). Compara-se o menor top e o maior bottom/right com as bordas
#   do scaler, dividindo o excesso pelo fator de escala para reportar px em
#   coordenadas do slide (independente do tamanho da janela).
#   Metrica secundaria de conferencia: altura do proprio
#   `.remark-slide-content` menos a altura do scaler.
#   Como o remark so mantem o slide corrente visivel, navega-se slide a slide
#   com `slideshow.gotoSlide(n)` (1..`slideshow.getSlideCount()`, o que inclui
#   os passos incrementais `--`).
#
# USO
#   Rscript scripts/verificar_slides.R                    # todos Aulas2026/MATD48-*.html
#   Rscript scripts/verificar_slides.R Aulas2026/MATD48-07.html Aulas/MATD48-03.html
#   TOL=2 SHOTS=1 Rscript scripts/verificar_slides.R      # opcoes por ambiente
#     TOL   = px de transbordo tolerados (default 2)
#     SHOTS = 1 grava PNG de cada slide com transbordo em scripts/out/ (default 1)
#   Saidas: relatorio no terminal, scripts/out/verificar_slides.csv e PNGs.
#
# REQUISITOS: R + pacotes `chromote` e `jsonlite`, e google-chrome instalado.
# ---------------------------------------------------------------------------

options(chromote.timeout = 120)
suppressPackageStartupMessages({
  library(chromote)
  library(jsonlite)
})

this_file <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
repo_root <- tryCatch(normalizePath(file.path(dirname(this_file), "..")), error = function(e) getwd())

args  <- commandArgs(TRUE)
TOL   <- as.numeric(Sys.getenv("TOL", "2"))
SHOTS <- Sys.getenv("SHOTS", "1") != "0"
OUT   <- file.path(repo_root, "scripts", "out")
VW    <- 1400L; VH <- 1050L          # viewport 4:3, mesma razao do slide

files <- if (length(args)) normalizePath(args, mustWork = TRUE) else
  sort(Sys.glob(file.path(repo_root, "Aulas2026", "MATD48-*.html")))
if (!length(files)) stop("Nenhum arquivo .html encontrado.")
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

JS_MEASURE <- '(function(){
  var c = document.querySelector(".remark-visible .remark-slide-content");
  if (!c) return JSON.stringify({err: "sem slide visivel"});
  var sc = c.closest(".remark-slide-scaler") || c.parentElement;
  var R  = sc.getBoundingClientRect();                  /* caixa que CORTA */
  var scale = (sc.style && sc.style.width) ? R.width / parseFloat(sc.style.width) : 1;
  if (!isFinite(scale) || scale <= 0) scale = 1;
  var cs   = getComputedStyle(c);
  var padB = (parseFloat(cs.paddingBottom) || 0) * scale;
  var padR = (parseFloat(cs.paddingRight)  || 0) * scale;

  /* recorta o retangulo de `e` pelos ancestrais que clipam (overflow != visible) */
  function clipped(e) {
    var rr = e.getBoundingClientRect();
    var top = rr.top, bot = rr.bottom, rig = rr.right;
    var p = e.parentElement;
    while (p && p !== sc) {
      var ps = getComputedStyle(p);
      if (ps.overflow !== "visible" || ps.overflowY !== "visible" || ps.overflowX !== "visible") {
        var pr = p.getBoundingClientRect();
        if (ps.overflowY !== "visible") { top = Math.max(top, pr.top); bot = Math.min(bot, pr.bottom); }
        if (ps.overflowX !== "visible") { rig = Math.min(rig, pr.right); }
      }
      p = p.parentElement;
    }
    return {top: top, bottom: bot, right: rig, w: rr.width, h: rr.height};
  }

  var maxB = -1e9, minT = 1e9, maxR = -1e9, whoB = "", whoR = "";
  var all = c.querySelectorAll("*");
  for (var i = 0; i < all.length; i++) {
    var e = all[i], st = getComputedStyle(e);
    if (st.position === "absolute" || st.position === "fixed") continue;
    if (st.display === "none" || st.visibility === "hidden") continue;
    var q = clipped(e);
    if (q.w === 0 && q.h === 0) continue;
    if (q.bottom > q.top) {
      if (q.bottom > maxB) {
        maxB = q.bottom;
        whoB = e.tagName.toLowerCase() +
          (e.className && e.className.toString ?
            "." + e.className.toString().trim().split(/\\s+/)[0] : "");
        whoB = whoB.slice(0, 26);
      }
      if (q.top < minT) minT = q.top;
    }
    if (q.right > maxR) { maxR = q.right; whoR = e.tagName.toLowerCase(); }
  }
  if (maxB === -1e9) { maxB = R.top; minT = R.top; maxR = R.left; }

  var t  = c.querySelector("h1,h2,h3");
  var tt = t ? t.textContent : (c.querySelector(".title") ? c.querySelector(".title").textContent : "");
  var num = document.querySelector(".remark-visible .remark-slide-number");
  var rc  = c.getBoundingClientRect();
  return JSON.stringify({
    titulo:  (tt || "(sem titulo)").replace(/\\s+/g, " ").trim().slice(0, 74),
    numero:  num ? num.textContent.trim() : "",
    escala:  scale,
    h_slide: R.height / scale,
    over_bottom: (maxB - R.bottom) / scale,          /* px CORTADOS embaixo   */
    over_top:    (R.top - minT)    / scale,          /* px CORTADOS em cima   */
    over_right:  (maxR - R.right)  / scale,          /* px CORTADOS a direita */
    over_pad:    (maxB - (R.bottom - padB)) / scale, /* invade margem inferior */
    over_padr:   (maxR - (R.right  - padR)) / scale, /* invade margem direita  */
    over_caixa:  (rc.height - R.height) / scale,     /* metrica de conferencia */
    scroll_v: c.scrollHeight - c.clientHeight,       /* (sempre 0: nao serve)  */
    culpado:  whoB
  });
})()'

ev <- function(js) {
  out <- b$Runtime$evaluate(js, returnByValue = TRUE)
  if (!is.null(out$exceptionDetails)) return(NULL)
  out$result$value
}
wait_until <- function(js, max_s = 30) {
  t0 <- Sys.time()
  repeat {
    if (isTRUE(tryCatch(ev(js), error = function(e) NULL))) return(TRUE)
    if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > max_s) return(FALSE)
    Sys.sleep(0.25)
  }
}

b <- ChromoteSession$new(width = VW, height = VH)
on.exit(try(b$close(), silent = TRUE), add = TRUE)

todos <- list()
for (f in files) {
  nome <- basename(f)
  message(sprintf("==> %s", nome))
  b$Page$navigate(paste0("file://", f), wait_ = TRUE)
  wait_until('document.readyState === "complete"')
  ev('window.__ok = false;
      (function(){
         var done = function(){ document.fonts.ready.then(function(){ window.__ok = true; }); };
         if (window.MathJax && window.MathJax.Hub) MathJax.Hub.Queue(done); else setTimeout(done, 1500);
      })();')
  if (!wait_until("window.__ok === true", 45))
    message("    (aviso: MathJax/fontes nao confirmaram; medindo mesmo assim)")
  Sys.sleep(1.2)

  n <- ev("slideshow.getSlideCount()")
  if (is.null(n)) { message("    !! remark nao inicializou; pulando"); next }
  for (i in seq_len(n)) {
    ev(sprintf("slideshow.gotoSlide(%d)", i)); Sys.sleep(0.18)
    m <- tryCatch(fromJSON(ev(JS_MEASURE)), error = function(e) NULL)
    if (is.null(m) || !is.null(m$err)) next
    rec <- data.frame(arquivo = nome, slide = i, numero = m$numero, titulo = m$titulo,
                      over_bottom = round(m$over_bottom, 1), over_top = round(m$over_top, 1),
                      over_right = round(m$over_right, 1), over_pad = round(m$over_pad, 1),
                      over_padr = round(m$over_padr, 1), over_caixa = round(m$over_caixa, 1),
                      scroll_v = m$scroll_v, culpado = m$culpado, stringsAsFactors = FALSE)
    rec$corte <- max(rec$over_bottom, rec$over_top, rec$over_right)
    todos[[length(todos) + 1]] <- rec
    if (SHOTS && rec$corte > TOL) {
      png <- file.path(OUT, sprintf("%s-slide%02d.png", sub("\\.html$", "", nome), i))
      writeBin(jsonlite::base64_dec(b$Page$captureScreenshot(format = "png")$data), png)
    }
  }
}

res <- do.call(rbind, todos)
if (is.null(res)) stop("Nada medido.")
utils::write.csv(res, file.path(OUT, "verificar_slides.csv"), row.names = FALSE)

ruins <- res[res$corte > TOL, ]; ruins <- ruins[order(-ruins$corte), ]
cat("\n\n==================== SLIDES COM TRANSBORDO (conteudo cortado) ====================\n")
cat(sprintf("%-16s %-6s %-9s %8s  %s\n", "arquivo", "slide", "(n/N)", "px cortados", "titulo"))
if (!nrow(ruins)) cat("Nenhum (tolerancia ", TOL, " px).\n", sep = "") else
  for (k in seq_len(nrow(ruins))) with(ruins[k, ], cat(sprintf(
    "%-16s %-6d %-9s %8.0f  %s%s%s  [%s]\n", arquivo, slide,
    ifelse(nzchar(numero), paste0("(", numero, ")"), "-"), corte, titulo,
    ifelse(over_top   > TOL, "  <TOPO>",   ""),
    ifelse(over_right > TOL, "  <DIREITA>", ""), culpado)))

lim <- res[res$corte <= TOL & res$over_pad > 0, ]; lim <- lim[order(-lim$over_pad), ]
cat("\n-------- No limite: entra na margem inferior mas ainda nao e cortado --------\n")
if (!nrow(lim)) cat("Nenhum.\n") else
  for (k in seq_len(nrow(lim))) with(lim[k, ], cat(sprintf(
    "%-16s %-6d folga ate a borda: %4.0f px  %s\n", arquivo, slide, -corte, titulo)))

cat("\n-------------------------- Resumo por deck --------------------------\n")
for (f in unique(res$arquivo)) {
  s <- res[res$arquivo == f, ]
  cat(sprintf("%-16s %3d slides | %2d com transbordo | pior %+6.0f px %s\n",
              f, nrow(s), sum(s$corte > TOL), max(s$corte),
              ifelse(sum(s$corte > TOL) == 0, "  <- LIMPO", "")))
}
cat("\nCSV: ", file.path(OUT, "verificar_slides.csv"), "\n", sep = "")
if (SHOTS) cat("PNGs dos slides com transbordo: ", OUT, "\n", sep = "")
