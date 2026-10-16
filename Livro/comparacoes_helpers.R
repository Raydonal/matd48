# comparacoes_helpers.R -- funções compartilhadas pelo Cap. 3 do livro e pela Aula 07 para
# explicar VISUALMENTE os procedimentos em etapas (Duncan, SNK) e a taxa de erro por família.
#   livro:  source("comparacoes_helpers.R")
#   slides: source("../Livro/comparacoes_helpers.R")
# Requer tidyverse carregado.

.fmt_br <- function(x, d = 2) formatC(x, format = "f", digits = d, big.mark = ".", decimal.mark = ",")

# Procedimento em etapas com registro de cada comparação.
#   medias: vetor NOMEADO de médias de tratamento (qualquer ordem)
#   qme, n, nu: quadrado médio do erro, réplicas por tratamento e gl do erro
#   metodo: "Duncan" (nível alpha_p = 1-(1-alpha)^(p-1)) ou "SNK" (nível alpha em toda amplitude)
#   R_fixo: se informado, usa essa régua para todas as amplitudes (ex.: Tukey) e testa todos os pares
# Devolve uma linha por par: posições i < j na ordenação, amplitude p, diferença, régua R,
# se o par chegou a ser testado (regra de coerência) e se foi declarado diferente.
passos_amplitude <- function(medias, qme, n, nu, metodo = c("Duncan", "SNK"), alpha = 0.05,
                             R_fixo = NULL) {
  metodo <- match.arg(metodo)
  ms <- sort(medias); t <- length(ms); se <- sqrt(qme / n)
  nivel <- function(p) if (metodo == "Duncan") (1 - alpha)^(p - 1) else 1 - alpha
  R <- if (is.null(R_fixo)) c(NA, sapply(2:t, function(p) qtukey(nivel(p), p, nu) * se)) else rep(R_fixo, t)
  sig <- matrix(FALSE, t, t); res <- vector("list", 0)
  for (p in t:2) for (i in 1:(t - p + 1)) {
    j <- i + p - 1
    testado <- !is.null(R_fixo) || p == t ||
      ((i == 1 || sig[i - 1, j]) && (j == t || sig[i, j + 1]))
    d <- unname(ms[j] - ms[i])
    sig[i, j] <- testado && d > R[p]
    res[[length(res) + 1]] <- tibble(i = i, j = j, menor = names(ms)[i], maior = names(ms)[j],
                                     x1 = unname(ms[i]), x2 = unname(ms[j]), p = p, dif = d,
                                     R = R[p], testado = testado, sig = sig[i, j])
  }
  bind_rows(res)
}

# Grupos homogêneos maximais: conjuntos de médias CONSECUTIVAS (na ordenação) sem nenhum par
# declarado diferente. São os "traços" sob as médias e a origem das letras.
grupos_homogeneos <- function(passos) {
  t <- max(passos$j); difere <- matrix(FALSE, t, t)
  for (k in seq_len(nrow(passos))) difere[passos$i[k], passos$j[k]] <- passos$sig[k]
  cand <- list()
  for (i in 1:t) for (j in i:t) if (!any(difere[i:j, i:j])) cand[[length(cand) + 1]] <- c(i, j)
  maximal <- Filter(function(g) !any(sapply(cand, function(h)
    h[1] <= g[1] && h[2] >= g[2] && !identical(h, g))), cand)
  tibble(ini = sapply(maximal, `[`, 1), fim = sapply(maximal, `[`, 2))
}

# Letras a partir dos grupos homogêneos (a = grupo da MAIOR média, como no agricolae)
letras_grupos <- function(passos, ms) {
  g <- grupos_homogeneos(passos) %>% arrange(desc(fim))
  sapply(seq_along(ms), function(k) paste(letters[which(g$ini <= k & g$fim >= k)], collapse = ""))
}

.cores_p <- c(`2` = "#2e8b57", `3` = "#d98324", `4` = "#8b2e6e", `5` = "#3b5bdb", `6` = "#7f8c8d")

# Base: as médias ordenadas sobre um eixo na escala da resposta, com nome, valor e posição
.base_medias <- function(ms, d = 2, ylim = c(-1, 1.6), xpad = 0.08) {
  rng <- diff(range(ms)); pts <- tibble(x = unname(ms), rot = names(ms), pos = seq_along(ms))
  ggplot() +
    geom_hline(yintercept = 0, color = "grey60") +
    geom_point(data = pts, aes(x, 0), size = 4, color = "#0B4F6C") +
    geom_text(data = pts, aes(x, -0.12, label = paste0(rot, "\n", .fmt_br(x, d), "\n(", pos, ")")),
              size = 3.6, vjust = 1, lineheight = 0.9) +
    scale_x_continuous(limits = range(ms) + c(-1, 1) * xpad * rng) +
    coord_cartesian(ylim = ylim) +
    theme_void(base_size = 13) + theme(legend.position = "top")
}

.arcos <- function(passos, altura) {
  passos %>% mutate(id = row_number()) %>% rowwise() %>%
    reframe(id, p, sig, testado, u = seq(0, 1, length.out = 80),
            x = x1 + (x2 - x1) * u, y = altura * (p - 1) * sin(pi * u))
}

# Rótulos de arcos da mesma amplitude: alterna a altura para não se sobreporem
.alterna_altura <- function(lab, alt) {
  lab %>% group_by(p) %>% arrange(xm, .by_group = TRUE) %>%
    mutate(ym = ym + ifelse(row_number() %% 2 == 0, 0.32 * alt, 0)) %>% ungroup()
}

# Figura 1: a régua das médias (só os pontos ordenados)
plot_regua_medias <- function(ms, d = 2) .base_medias(ms, d, ylim = c(-1, 0.4))

# Figura 2: todos os pares como arcos; altura e cor pela amplitude p
plot_arcos_amplitude <- function(passos, ms, d = 2) {
  alt <- 1.3 / (max(passos$p) - 1)
  a <- .arcos(passos, alt)
  lab <- passos %>% mutate(xm = (x1 + x2) / 2, ym = alt * (p - 1),
                           txt = paste0(maior, "−", menor, "  (p = ", p, ")")) %>%
    .alterna_altura(alt)
  .base_medias(ms, d) +
    geom_path(data = a, aes(x, y, group = id, color = factor(p)), linewidth = 1.2) +
    geom_label(data = lab, aes(xm, ym, label = txt, color = factor(p)), size = 3.3,
               fill = "white", show.legend = FALSE) +
    scale_color_manual(values = .cores_p, name = "amplitude p")
}

# Figura 3: uma etapa do procedimento (amplitude pp em destaque)
plot_etapa <- function(passos, ms, pp, d = 2, d_dif = 2) {
  alt <- 1.3 / (max(passos$p) - 1)
  a <- .arcos(passos, alt) %>%
    mutate(estado = case_when(p < pp ~ "etapa seguinte",
                              p > pp & sig ~ "etapa anterior: diferem",
                              p > pp & testado ~ "etapa anterior: não diferem",
                              p > pp ~ "etapa anterior: não testado",
                              !testado ~ "não testado (coerência)",
                              sig ~ "diferem", TRUE ~ "não diferem"))
  lab <- passos %>% filter(p == pp) %>%
    mutate(xm = (x1 + x2) / 2, ym = alt * (p - 1)) %>% .alterna_altura(alt) %>%
    mutate(
           txt = ifelse(testado, paste0(maior, "−", menor, ": ", .fmt_br(dif, d_dif),
                                        ifelse(sig, " > ", " ≤ "), .fmt_br(R, d_dif)),
                        paste0(maior, "−", menor, ": não testado")))
  .base_medias(ms, d) +
    geom_path(data = a, aes(x, y, group = id, color = estado, linewidth = estado)) +
    geom_label(data = lab, aes(xm, ym, label = txt), size = 3.5, fill = "white") +
    scale_color_manual(values = c(`diferem` = "#2e8b57", `não diferem` = "#c0392b",
                                  `não testado (coerência)` = "#9b59b6",
                                  `etapa anterior: diferem` = "#a9d8bd", `etapa anterior: não diferem` = "#efb1a9",
                                  `etapa anterior: não testado` = "#d7bde2", `etapa seguinte` = "grey88"),
                       name = NULL, drop = TRUE) +
    scale_linewidth_manual(values = c(`diferem` = 1.7, `não diferem` = 1.7,
                                      `não testado (coerência)` = 1.7,
                                      `etapa anterior: diferem` = 0.9, `etapa anterior: não diferem` = 0.9,
                                      `etapa anterior: não testado` = 0.9, `etapa seguinte` = 0.6),
                          guide = "none")
}

# Figura 4: diagrama de traços (médias ligadas por um traço não diferem), um painel por método
plot_tracos <- function(lista_passos, ms, d = 2) {
  rng <- diff(range(ms))
  tr <- imap_dfr(lista_passos, function(ps, nome) {
    g <- grupos_homogeneos(ps)
    g %>% mutate(k = row_number(), x1 = ms[ini] - 0.025 * rng, x2 = ms[fim] + 0.025 * rng,
                 y = -0.2 * k, metodo = nome)
  })
  pts <- imap_dfr(lista_passos, function(ps, nome)
    tibble(metodo = nome, x = unname(ms), rot = names(ms), letra = letras_grupos(ps, ms)))
  ggplot() +
    geom_point(data = pts, aes(x, 0.2), size = 4, color = "#0B4F6C") +
    geom_text(data = pts, aes(x, 0.48, label = paste0(rot, " (", .fmt_br(x, d), ")  ", letra)), size = 3.6) +
    geom_segment(data = tr, aes(x = x1, xend = x2, y = y, yend = y), linewidth = 2.4, color = "#d98324") +
    facet_wrap(~ factor(metodo, levels = names(lista_passos)), ncol = 1) +
    scale_x_continuous(limits = range(ms) + c(-0.08, 0.08) * rng) +
    coord_cartesian(ylim = c(-0.75, 0.62)) + theme_void(base_size = 13) +
    theme(strip.text = element_text(face = "bold", size = 13, hjust = 0))
}

# Intuição: distribuição de (maior - menor)/EP entre p médias sob H0 (amplitude estudentizada)
sim_amplitude <- function(ps = 2:4, nu = 16, B = 20000, seed = 5) {
  set.seed(seed)
  purrr::map_dfr(ps, function(p) tibble(p = p, Q = replicate(B, {
    z <- rnorm(p); diff(range(z)) / sqrt(rchisq(1, nu) / nu) })))
}

# FWER: em B experimentos SEM efeito nenhum (t médias iguais), quais pares deram falso positivo?
sim_grade_fwer <- function(B = 100, t = 4, n = 10, sigma = 70, seed = 42, alpha = 0.05) {
  set.seed(seed); pr <- combn(t, 2); nu <- t * (n - 1)
  purrr::map_dfr(1:B, function(b) {
    y <- matrix(rnorm(n * t, 0, sigma), n); m <- colMeans(y); se <- sqrt(mean(apply(y, 2, var)) / n)
    d <- abs(m[pr[1, ]] - m[pr[2, ]])
    bind_rows(
      tibble(exp = b, par = seq_len(ncol(pr)), metodo = paste0(ncol(pr), " testes t, cada um a 5%"),
             fp = d > qt(1 - alpha / 2, nu) * sqrt(2) * se),
      tibble(exp = b, par = seq_len(ncol(pr)), metodo = "Tukey (FWER de 5%)",
             fp = d > qtukey(1 - alpha, t, nu) * se))
  })
}
