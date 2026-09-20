# ---------------------------------------------------------------------------
# Diagramas de Hasse (estrutura do delineamento) — MATD48
# ---------------------------------------------------------------------------
# Compartilhado entre Livro/ (bookdown) e Aulas2026/ (xaringan). Cada arquivo
# que usa diagramas de Hasse deve dar source() neste arquivo no seu chunk de
# setup -- em Livro/: source("hasse_helpers.R"); em Aulas2026/:
# source("../Livro/hasse_helpers.R"). (O bookdown roda cada capitulo em sessao
# R nova -- new_session: yes --, entao nao ha como compartilhar a funcao de
# outra forma.)
#
# DECISOES DE PROJETO (nao alterar sem entender o porque):
#
# 1. O LAYOUT E AUTOMATICO, via Graphviz (`dot`, instalado no sistema). A versao
#    anterior pedia coordenadas x/y digitadas a mao para cada no, o que produzia
#    diagramas torcidos e obrigava a reposicionar tudo a cada no novo.
#
# 2. OS GRAUS DE LIBERDADE SAO CALCULADOS, nao digitados. Informa-se apenas o
#    numero de CLASSES de cada termo; o gl sai da regra padrao do diagrama de
#    Hasse (Oehlert 2010, cap. 12; Bailey 2008, cap. 10):
#
#        gl(v) = classes(v) - soma de gl(u) sobre TODO termo u acima de v
#
#    onde "acima" = todo ancestral no diagrama (fecho transitivo das arestas),
#    nao apenas o pai imediato. Digitar gl a mao foi fonte de erro no material
#    anterior; aqui um gl errado so pode vir de uma estrutura errada, que o
#    proprio diagrama deixa visivel.
#
# 3. A SAIDA E UM ARQUIVO SVG ESTATICO, gravado em caminho fixo e incluido com
#    knitr::include_graphics(). Isso contorna o bug conhecido do bookdown com
#    output_dir: "." (figuras geradas pelo knitr nao sao copiadas para fora de
#    _bookdown_files/ e aparecem quebradas no livro publicado -- ver CLAUDE.md,
#    footgun #2). Arquivo estatico nao passa por esse caminho.
# ---------------------------------------------------------------------------

# Ancestrais de cada no (fecho transitivo das arestas, subindo o diagrama).
.hasse_ancestrais <- function(termos, arestas) {
  pais <- lapply(termos, function(v) as.character(arestas$de[arestas$para == v]))
  names(pais) <- termos
  anc <- vector("list", length(termos))
  names(anc) <- termos
  for (v in termos) {                      # termos vem em ordem topologica
    a <- character(0)
    for (p in pais[[v]]) a <- union(a, c(p, anc[[p]]))
    anc[[v]] <- a
  }
  anc
}

# Ordena os termos topologicamente (todo pai antes de todo filho).
.hasse_ordem_topologica <- function(termos, arestas) {
  restantes <- termos
  ordem <- character(0)
  while (length(restantes)) {
    livres <- restantes[!vapply(restantes, function(v) {
      any(arestas$de %in% restantes & arestas$para == v)
    }, logical(1))]
    if (!length(livres)) {
      stop("Estrutura invalida: ha ciclo nas arestas do diagrama de Hasse.")
    }
    ordem <- c(ordem, livres)
    restantes <- setdiff(restantes, livres)
  }
  ordem
}

#' Graus de liberdade de cada termo, a partir da estrutura
#'
#' @param nos      data.frame/tibble com colunas `termo` e `classes`
#' @param arestas  data.frame/tibble com colunas `de` e `para`
#' @return o mesmo `nos`, com a coluna `gl` calculada
hasse_gl <- function(nos, arestas) {
  nos <- as.data.frame(nos, stringsAsFactors = FALSE)
  arestas <- as.data.frame(arestas, stringsAsFactors = FALSE)
  stopifnot(all(c("de", "para") %in% names(arestas)))

  # Saida para diagramas ESQUEMATICOS: quando um no agrega varios termos
  # ("Interacoes duplas (6 termos)"), nao existe "numero de classes" que gere o
  # gl pela regra do poset, e o gl e informado direto. Usar so nesse caso; o
  # caminho padrao (coluna `classes`) e o que protege contra gl digitado errado.
  if (!"classes" %in% names(nos) && "gl" %in% names(nos)) {
    nos$gl <- as.integer(nos$gl)
    return(nos)
  }
  stopifnot(all(c("termo", "classes") %in% names(nos)))

  soltos <- setdiff(unique(c(arestas$de, arestas$para)), nos$termo)
  if (length(soltos)) {
    stop("Aresta referencia termo inexistente: ", paste(soltos, collapse = ", "))
  }

  ordem <- .hasse_ordem_topologica(nos$termo, arestas)
  anc <- .hasse_ancestrais(ordem, arestas)

  gl <- stats::setNames(rep(NA_real_, nrow(nos)), nos$termo)
  classes <- stats::setNames(nos$classes, nos$termo)
  for (v in ordem) gl[v] <- classes[[v]] - sum(gl[anc[[v]]])

  if (any(gl < 0)) {
    stop("gl negativo em: ", paste(names(gl)[gl < 0], collapse = ", "),
         ". Verifique o numero de classes ou as arestas da estrutura.")
  }
  nos$gl <- as.integer(gl[nos$termo])
  nos
}

#' Desenha o diagrama de Hasse e devolve o caminho do SVG gerado
#'
#' @param nos      data.frame com `termo`, `classes` e (opcional) `rotulo`
#' @param arestas  data.frame com `de`, `para`
#' @param arquivo  caminho do SVG a gravar
#' @param titulo   titulo opcional desenhado acima do diagrama
#' @param destacar vetor de termos a realcar (fundo colorido)
#' @return o caminho do arquivo, para uso em knitr::include_graphics()
#' @param tamanho  vetor c(largura, altura) em POLEGADAS limitando o desenho. O
#'   Graphviz reduz o diagrama proporcionalmente para caber nesse retangulo (nunca
#'   amplia). E o unico jeito confiavel de controlar a ALTURA da figura: em slides
#'   xaringan, `out.width` limita so a largura, e um diagrama alto continua sendo
#'   cortado pela caixa de tamanho fixo do remark.
#' @param compacto  TRUE aproxima os niveis verticalmente (ranksep menor), util em
#'   cadeias longas (submuestreo, fatoriais de 3 fatores).
hasse_svg <- function(nos, arestas, arquivo, titulo = NULL, destacar = character(0),
                      tamanho = NULL, compacto = FALSE) {
  nos <- hasse_gl(nos, arestas)
  if (!"rotulo" %in% names(nos)) nos$rotulo <- nos$termo
  # Em rotulo HTML-like do Graphviz, "\n" nao quebra linha -- e preciso <BR/>.
  nos$rotulo <- gsub("\n", "<BR/>", nos$rotulo, fixed = TRUE)

  dir.create(dirname(arquivo), recursive = TRUE, showWarnings = FALSE)
  id <- function(x) paste0("n", match(x, nos$termo))

  # Rotulo em tabela HTML do Graphviz: nome em cima, "classes / gl" embaixo.
  linhas_nos <- vapply(seq_len(nrow(nos)), function(i) {
    cor <- if (nos$termo[i] %in% destacar) "#fdf2d0" else "white"
    borda <- if (nos$termo[i] %in% destacar) "#b8860b" else "#444444"
    sprintf(
      '  %s [label=<<TABLE BORDER="0" CELLBORDER="0" CELLSPACING="0">
         <TR><TD><B>%s</B></TD></TR>
         <TR><TD><FONT POINT-SIZE="10">%s%s</FONT></TD></TR>
       </TABLE>>, fillcolor="%s", color="%s"];',
      id(nos$termo[i]), nos$rotulo[i],
      if ("classes" %in% names(nos))
        sprintf(if (nos$classes[i] == 1) "%d classe &#183; " else "%d classes &#183; ", nos$classes[i])
      else "",
      sprintf("%d gl", nos$gl[i]),
      cor, borda
    )
  }, character(1))

  linhas_arestas <- vapply(seq_len(nrow(arestas)), function(i) {
    sprintf("  %s -> %s;", id(arestas$de[i]), id(arestas$para[i]))
  }, character(1))

  dot <- paste(c(
    "digraph hasse {",
    "  rankdir=TB;",
    "  bgcolor=\"transparent\";",
    if (!is.null(tamanho)) sprintf('  size="%s,%s";', tamanho[1], tamanho[2]),
    sprintf("  ranksep=%s; nodesep=%s;", if (compacto) "0.28" else "0.45",
            if (compacto) "0.22" else "0.35"),
    if (!is.null(titulo)) sprintf('  label=<<B>%s</B>>; labelloc="t"; fontsize=13;' , titulo),
    "  node [shape=box, style=\"rounded,filled\", fontname=\"Helvetica\", fontsize=12, margin=\"0.14,0.08\"];",
    "  edge [arrowhead=none, color=\"#777777\", penwidth=1.1];",
    linhas_nos,
    linhas_arestas,
    "}"
  ), collapse = "\n")

  dot_tmp <- tempfile(fileext = ".dot")
  writeLines(dot, dot_tmp)
  st <- system2("dot", c("-Tsvg", shQuote(dot_tmp), "-o", shQuote(arquivo)))
  unlink(dot_tmp)
  if (st != 0 || !file.exists(arquivo)) {
    stop("Falha ao gerar o SVG com o Graphviz (`dot`). Status: ", st)
  }
  arquivo
}

#' Tabela de fontes de variacao correspondente ao diagrama (para conferencia)
#'
#' Serve para o texto mostrar, ao lado do diagrama, que os gl fecham: a soma
#' dos gl de todos os termos e igual ao numero de observacoes.
hasse_tabela <- function(nos, arestas) {
  nos <- hasse_gl(nos, arestas)
  nos[, intersect(c("termo", "classes", "gl"), names(nos))]
}
