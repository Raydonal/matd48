# controle_helpers.R -- o fio condutor "experimento controlado x estudo não controlado".
# Cada capítulo do livro (e as aulas correspondentes) termina com a mesma pergunta: o que este
# delineamento CONTROLA, e o que daria errado se a mesma pergunta fosse respondida com dados que o
# pesquisador apenas observou? Cada função cnc_capN() simula as duas situações com a MESMA
# verdade e devolve os resultados; cada grafico_capN() desenha a comparação.
#   livro:  source("controle_helpers.R")
#   slides: source("../Livro/controle_helpers.R")
# Requer tidyverse carregado. Nada aqui é sorteado sem set.seed() dentro da própria função, de
# modo que livro e slides mostram exatamente os mesmos números.

.cor_ctrl <- "#3b5bdb"   # controlado (sorteio feito pelo pesquisador)
.cor_obs  <- "#d98324"   # não controlado (observacional)
.cor_verd <- "#2b2b2b"   # valor verdadeiro
.fmt <- function(x, d = 2) {
  x[abs(x) < 0.5 * 10^-d] <- 0   # evita "-0,00"
  formatC(x, format = "f", digits = d, big.mark = ".", decimal.mark = ",")
}
.slide_cnc <- function() isTRUE(getOption("cnc.slide", FALSE))   # nos slides: options(cnc.slide = TRUE)
.tema_cnc <- function(base_size) theme_minimal(base_size = base_size) +
  theme(legend.position = if (.slide_cnc()) "top" else "bottom", panel.grid.minor = element_blank())
.ncol_leg <- function() if (.slide_cnc()) 2 else 1

# Resumo comum: média, viés, desvio-padrão e raiz do erro quadrático médio de um estimador
resumo_estimador <- function(est, verdade) {
  est %>% group_by(regime) %>%
    summarise(media = mean(estimativa), vies = mean(estimativa) - verdade,
              dp = sd(estimativa), reqm = sqrt(mean((estimativa - verdade)^2)), .groups = "drop")
}

# Densidades sobrepostas de um estimador nos dois regimes, com o valor verdadeiro
.grafico_densidades <- function(est, verdade, rotulo_x, base_size, cores) {
  medias <- est %>% group_by(regime) %>% summarise(m = mean(estimativa), .groups = "drop")
  ggplot(est, aes(estimativa, fill = regime, color = regime)) +
    geom_density(alpha = 0.35, linewidth = 0.6, adjust = 1.1) +
    geom_vline(xintercept = verdade, color = .cor_verd, linewidth = 1, linetype = "dashed") +
    geom_vline(data = medias, aes(xintercept = m, color = regime), linewidth = 0.8) +
    scale_fill_manual(values = cores, name = NULL) +
    scale_color_manual(values = cores, name = NULL) +
    guides(fill = guide_legend(ncol = .ncol_leg()), color = guide_legend(ncol = .ncol_leg())) +
    labs(x = rotulo_x, y = "Densidade") +
    .tema_cnc(base_size)
}

# --- Capítulo 1 / Aula 01: sorteio x autosseleção ---------------------------------------------
# As mesmas 40 parcelas da seção de resultados potenciais (mesma semente, mesmos Y(0), Y(1)).
# Controlado: 20 parcelas sorteadas para V2. Não controlado: o agricultor escolhe 20 parcelas
# para V2 com probabilidade crescente na fertilidade (ele "sabe" quais terras são boas).
cnc_cap1 <- function(n_rep = 4000, forca = 0.5) {
  set.seed(2026)
  N <- 40; n1 <- 20
  fert <- runif(N, 0, 10)
  Y0 <- 20 + 1.5 * fert + rexp(N, rate = 0.3)
  Y1 <- Y0 + 4 + 0.3 * fert
  ate <- mean(Y1 - Y0)
  set.seed(1)
  ctrl <- replicate(n_rep, { s <- sample(N, n1); mean(Y1[s]) - mean(Y0[-s]) })
  obs <- replicate(n_rep, { s <- sample(N, n1, prob = exp(forca * fert)); mean(Y1[s]) - mean(Y0[-s]) })
  list(ate = ate, fert = fert,
       est = tibble(regime = rep(c("Controlado: sorteio", "Não controlado: o agricultor escolhe"),
                                 each = n_rep),
                    estimativa = c(ctrl, obs)) %>% mutate(regime = fct_inorder(regime)))
}
grafico_cap1 <- function(r, base_size = 13) {
  cores <- setNames(c(.cor_ctrl, .cor_obs), levels(r$est$regime))
  .grafico_densidades(r$est, r$ate, "Diferença de médias observada (ATE estimado)", base_size, cores)
}

# --- Capítulo 2 / Aulas 02-03: variável omitida em Y = Xb + e -----------------------------------
# Tempo de reação (ms) em função de horas de sono. O estresse também aumenta o tempo de reação.
# Não controlado: quem dorme menos é também quem está mais estressado (cov(sono, estresse) < 0).
# Controlado: o pesquisador sorteia a restrição de sono (4 a 9 h); o estresse não depende do sorteio.
# Verdade: b_sono = -8 ms/h, g_estresse = +15 ms/ponto. Viés previsto (variável omitida):
# g * delta, com delta = coeficiente da regressão de estresse em sono = -0,6 -> viés = -9 ms/h.
cnc_cap2 <- function(n = 60, n_rep = 3000, b_sono = -8, g = 15, delta = -0.6) {
  set.seed(2026)
  um <- function(controlado) {
    sono <- if (controlado) sample(rep(4:9, length.out = n)) else round(runif(n, 4, 9), 1)
    estresse <- if (controlado) 3 + rnorm(n, 0, 1) else 3 + delta * (sono - 6.5) + rnorm(n, 0, 0.8)
    tr <- 250 + b_sono * sono + g * estresse + rnorm(n, 0, 12)
    tibble(sono, estresse, tr)
  }
  ajusta <- function(d) unname(coef(lm(tr ~ sono, data = d))[2])
  est <- tibble(
    regime = rep(c("Controlado: sono sorteado", "Não controlado: sono observado"), each = n_rep),
    estimativa = c(replicate(n_rep, ajusta(um(TRUE))), replicate(n_rep, ajusta(um(FALSE))))
  ) %>% mutate(regime = fct_inorder(regime))
  set.seed(7)
  list(b = b_sono, vies_previsto = g * delta, est = est,
       exemplo = bind_rows(um(TRUE) %>% mutate(regime = "Controlado: sono sorteado"),
                           um(FALSE) %>% mutate(regime = "Não controlado: sono observado")) %>%
         mutate(regime = fct_inorder(regime)))
}
grafico_cap2 <- function(r, base_size = 13) {
  cores <- setNames(c(.cor_ctrl, .cor_obs), levels(r$est$regime))
  .grafico_densidades(r$est, r$b, "Inclinação estimada do sono (ms por hora)", base_size, cores)
}
grafico_cap2_dados <- function(r, base_size = 13) {
  ggplot(r$exemplo, aes(sono, estresse, color = regime)) +
    geom_point(alpha = 0.7, size = 1.8) +
    geom_smooth(method = "lm", se = FALSE, formula = y ~ x, linewidth = 0.9) +
    facet_wrap(~regime) +
    scale_color_manual(values = setNames(c(.cor_ctrl, .cor_obs), levels(r$exemplo$regime)),
                       guide = "none") +
    labs(x = "Horas de sono", y = "Estresse (variável omitida)") +
    .tema_cnc(base_size)
}

# --- Capítulo 3 / Aulas 04-08: o teste F do DCA sob H0 ------------------------------------------
# Três condições de distração SEM efeito nenhum (H0 verdadeira). Cada participante tem uma
# velocidade basal u. Não controlado: o participante ESCOLHE a condição, e os mais lentos tendem a
# escolher "nenhuma" (força kappa da autosseleção). Controlado: sorteio balanceado, 12 por grupo.
# Devolve a taxa de rejeição do F a 5% em função de kappa.
cnc_cap3 <- function(n_por = 12, n_rep = 1000, kappas = seq(0, 2, by = 0.25)) {
  set.seed(2026)
  t <- 3; N <- t * n_por
  f_p <- function(y, g) {
    g <- factor(g); if (nlevels(g) < t || any(table(g) < 2)) return(NA_real_)
    mi <- tapply(y, g, mean); ni <- tapply(y, g, length)
    sqe <- sum(ni * (mi - mean(y))^2); sqr <- sum((y - mi[g])^2)
    pf((sqe / (t - 1)) / (sqr / (N - t)), t - 1, N - t, lower.tail = FALSE)
  }
  linhas <- lapply(kappas, function(k) {
    p_obs <- replicate(n_rep, {
      u <- rnorm(N); y <- 400 + 40 * u + rnorm(N, 0, 20)
      esc <- cbind(k * u, 0, -k * u)
      prob <- exp(esc) / rowSums(exp(esc))
      g <- apply(prob, 1, function(p) sample(1:t, 1, prob = p))
      f_p(y, g)
    })
    p_ctrl <- replicate(n_rep, {
      u <- rnorm(N); y <- 400 + 40 * u + rnorm(N, 0, 20)
      f_p(y, sample(rep(1:t, n_por)))
    })
    tibble(kappa = k,
           regime = c("Controlado: sorteio", "Não controlado: o participante escolhe"),
           rejeicao = c(mean(p_ctrl < 0.05, na.rm = TRUE), mean(p_obs < 0.05, na.rm = TRUE)))
  })
  bind_rows(linhas) %>% mutate(regime = fct_inorder(regime))
}
grafico_cap3 <- function(r, base_size = 13) {
  cores <- setNames(c(.cor_ctrl, .cor_obs), levels(r$regime))
  ggplot(r, aes(kappa, rejeicao, color = regime)) +
    geom_hline(yintercept = 0.05, linetype = "dashed", color = .cor_verd) +
    geom_line(linewidth = 1) + geom_point(size = 2.2) +
    scale_color_manual(values = cores, name = NULL) +
    guides(color = guide_legend(ncol = .ncol_leg())) +
    scale_y_continuous(labels = function(x) paste0(.fmt(100 * x, 0), "%"), limits = c(0, 1)) +
    labs(x = expression("Força da autosseleção " * kappa),
         y = "Rejeições de H0 verdadeira") +
    .tema_cnc(base_size)
}

# --- Capítulo 4 / Aulas 09-11: blocos -------------------------------------------------------------
# 8 áreas do viveiro (blocos, efeito fixo sd 6), 4 parcelas por área, adubo novo (T) x padrão (C),
# efeito verdadeiro tau = 3. Cada parcela tem uma qualidade q NÃO medida (drenagem, sd 2).
# DBCA: em cada área, 2 parcelas sorteadas para T. Não controlado: o gerente põe T mais vezes nas
# áreas férteis E, dentro da área, nas parcelas que "parecem melhores" (q alto).
cnc_cap4 <- function(b = 8, k = 4, tau = 3, n_rep = 3000) {
  set.seed(2026)
  bloco_ef <- seq(-9, 9, length.out = b)
  um <- function(controlado) {
    bl <- rep(1:b, each = k); q <- rnorm(b * k, 0, 2)
    trat <- if (controlado) unlist(lapply(1:b, function(j) sample(rep(0:1, k / 2)))) else
      rbinom(b * k, 1, plogis(0.25 * bloco_ef[bl] + 0.9 * q))
    y <- 50 + bloco_ef[bl] + tau * trat + q + rnorm(b * k, 0, 1.5)
    d <- tibble(bloco = factor(bl), trat, y)
    if (length(unique(trat)) < 2) return(c(NA, NA))
    c(sem = mean(y[trat == 1]) - mean(y[trat == 0]),
      com = unname(coef(lm(y ~ trat + bloco, data = d))["trat"]))
  }
  ctrl <- replicate(n_rep, um(TRUE)); obs <- replicate(n_rep, um(FALSE))
  list(tau = tau,
       est = tibble(regime = rep(c("DBCA: sorteio dentro da área",
                                   "Observacional, sem ajuste",
                                   "Observacional, ajustado por área"), each = n_rep),
                    estimativa = c(ctrl["com", ], obs["sem", ], obs["com", ])) %>%
         filter(!is.na(estimativa)) %>% mutate(regime = fct_inorder(regime)))
}
grafico_cap4 <- function(r, base_size = 13) {
  cores <- setNames(c(.cor_ctrl, .cor_obs, "#8b2e6e"), levels(r$est$regime))
  ggplot(r$est, aes(regime, estimativa, fill = regime)) +
    geom_hline(yintercept = r$tau, linetype = "dashed", color = .cor_verd, linewidth = 0.9) +
    geom_violin(alpha = 0.45, color = NA) +
    geom_boxplot(width = 0.14, outlier.shape = NA, alpha = 0.9) +
    scale_fill_manual(values = cores, guide = "none") +
    labs(x = NULL, y = "Efeito estimado do adubo novo") +
    .tema_cnc(base_size)
}

# --- Capítulo 5 / Aula 12: cruzamento de fatores --------------------------------------------------
# Fatorial 2x2 (temperatura x catalisador), N = 24. Planejado: 6 corridas por célula.
# Não controlado: registros de processo em que os operadores quase sempre usam temperatura alta com
# catalisador novo e baixa com o antigo (probabilidade "diagonal" p_diag). Para a interação
# (mu11 - mu12 - mu21 + mu22), Var = sigma^2 * soma(1/n_ij): o planejado dá 4/6; uma célula vazia
# torna a interação não estimável.
cnc_cap5 <- function(N = 24, p_diags = c(0.5, 0.6, 0.7, 0.8, 0.9, 0.95), n_rep = 5000) {
  set.seed(2026)
  base <- 4 / (N / 4)
  bind_rows(lapply(p_diags, function(pd) {
    pr <- c(pd / 2, (1 - pd) / 2, (1 - pd) / 2, pd / 2)
    m <- rmultinom(n_rep, N, pr)
    vazia <- colSums(m == 0) > 0
    razao <- colSums(1 / m[, !vazia, drop = FALSE]) / base
    tibble(p_diag = pd, prop_vazia = mean(vazia), razao_mediana = median(razao),
           razao_q90 = unname(quantile(razao, 0.9)))
  }))
}
# Uma tabela de contagens típica de cada regime (para mostrar ao aluno)
celulas_cap5 <- function(N = 24, p_diag = 0.85) {
  set.seed(11)
  obs <- as.vector(rmultinom(1, N, c(p_diag / 2, (1 - p_diag) / 2, (1 - p_diag) / 2, p_diag / 2)))
  tibble(Temperatura = c("Baixa", "Baixa", "Alta", "Alta"),
         Catalisador = c("Antigo", "Novo", "Antigo", "Novo"),
         `Planejado (fatorial)` = N / 4, `Registros de processo` = obs[c(1, 2, 3, 4)])
}
grafico_cap5 <- function(r, base_size = 13) {
  r2 <- r %>% select(p_diag, `Fração de bancos com célula vazia (interação não estimável)` = prop_vazia,
                     `Variância relativa da interação (mediana)` = razao_mediana) %>%
    pivot_longer(-p_diag)
  ggplot(r2, aes(p_diag, value)) +
    geom_line(color = .cor_obs, linewidth = 1) + geom_point(color = .cor_obs, size = 2.2) +
    geom_hline(data = tibble(name = "Variância relativa da interação (mediana)", y = 1),
               aes(yintercept = y), color = .cor_ctrl, linetype = "dashed", linewidth = 0.9) +
    facet_wrap(~name, scales = "free_y") +
    scale_x_continuous(labels = function(x) paste0(.fmt(100 * x, 0), "%")) +
    labs(x = "Proporção de corridas na diagonal (alta+novo ou baixa+antigo)", y = NULL) +
    .tema_cnc(base_size)
}

# --- Capítulo 6 / Aulas 13-14: confusão planejada x confusão acidental ----------------------------
# Matriz de aliases A = (X1'X1)^{-1} X1'X2 para o modelo de efeitos principais (X1) quando a
# verdade inclui interações (X2). Planejado: meia fração 2^(4-1) com I = ABCD. Não controlado:
# 8 corridas "históricas" do mesmo processo, tiradas da grade 2^4 com preferência por A e B no
# mesmo nível (o hábito dos operadores).
cnc_cap6 <- function() {
  grade <- expand.grid(A = c(-1, 1), B = c(-1, 1), C = c(-1, 1), D = c(-1, 1))
  termos_int <- c("AB", "AC", "AD", "BC", "BD", "CD", "ABC", "ABD", "ACD", "BCD", "ABCD")
  mm <- function(g) {
    X1 <- cbind(I = 1, as.matrix(g))
    X2 <- sapply(termos_int, function(s) apply(g[, strsplit(s, "")[[1]], drop = FALSE], 1, prod))
    list(X1 = X1, X2 = X2)
  }
  alias <- function(g) { m <- mm(g); solve(crossprod(m$X1), crossprod(m$X1, m$X2)) }
  plan <- grade[with(grade, A * B * C * D) == 1, ]
  set.seed(2026)
  repeat {
    hist <- grade[sample(16, 8, prob = ifelse(grade$A == grade$B, 4, 1)), ]
    if (qr(mm(hist)$X1)$rank == 5) break
  }
  para_longo <- function(A, regime) as_tibble(A, rownames = "efeito") %>%
    pivot_longer(-efeito, names_to = "interacao", values_to = "coef") %>% mutate(regime = regime)
  list(plan = plan, hist = hist, A_plan = alias(plan), A_hist = alias(hist),
       longo = bind_rows(para_longo(alias(plan), "Planejado: meia fração I = ABCD"),
                         para_longo(alias(hist), "Não controlado: 8 corridas históricas")) %>%
         mutate(regime = fct_inorder(regime),
                efeito = factor(efeito, levels = rev(c("I", "A", "B", "C", "D"))),
                interacao = factor(interacao, levels = termos_int)))
}
grafico_cap6 <- function(r, base_size = 13) {
  ggplot(r$longo, aes(interacao, efeito, fill = coef)) +
    geom_tile(color = "white") +
    geom_text(aes(label = ifelse(abs(coef) < 1e-10, "0", .fmt(coef, 2))), size = base_size / 4.2) +
    facet_wrap(~regime, ncol = 1) +
    scale_fill_gradient2(low = "#2a78d6", mid = "white", high = "#d9480f", midpoint = 0,
                         limits = c(-1, 1), name = "Coef. de\nalias") +
    labs(x = "Interação omitida do modelo", y = "Efeito estimado") +
    .tema_cnc(base_size) + theme(legend.position = "right")
}

# --- Capítulo 7 / Aula 14: dados de acaso (happenstance) x desenho ------------------------------
# Rendimento em torno do ponto de operação (unidades codificadas), gradiente verdadeiro (4, 2).
# Não controlado: os operadores compensam a deriva de temperatura (x1) mexendo no tempo (x2), de
# modo que x2 ~ -0,95 x1: os 12 registros caem quase sobre uma reta. Controlado: 2^2 com duas
# réplicas + 4 pontos centrais (12 corridas). Em ambos, ajusta-se o modelo de primeira ordem e
# mede-se o erro absoluto (em graus) do ângulo da direção de máxima subida; o verdadeiro é 26,6°.
cnc_cap7 <- function(n_rep = 3000, sd_erro = 1.5) {
  set.seed(2026)
  eta <- function(x1, x2) 80 + 4 * x1 + 2 * x2
  ang_v <- atan2(2, 4) * 180 / pi
  desenho <- tibble(x1 = c(rep(c(-1, 1, -1, 1), 2), 0, 0, 0, 0),
                    x2 = c(rep(c(-1, -1, 1, 1), 2), 0, 0, 0, 0))
  obs_x <- function() { x1 <- runif(12, -1, 1); tibble(x1, x2 = -0.95 * x1 + rnorm(12, 0, 0.1)) }
  ang <- function(d) {
    d$y <- eta(d$x1, d$x2) + rnorm(nrow(d), 0, sd_erro)
    b <- coef(lm(y ~ x1 + x2, data = d))
    e <- abs(atan2(b[3], b[2]) * 180 / pi - ang_v)   # erro absoluto do ângulo, em [0, 180]
    min(e, 360 - e)
  }
  est <- tibble(regime = rep(c("Controlado: 2² + pontos centrais", "Não controlado: registros do processo"),
                             each = n_rep),
                estimativa = c(replicate(n_rep, ang(desenho)), replicate(n_rep, ang(obs_x())))) %>%
    mutate(regime = fct_inorder(regime))
  set.seed(3)
  list(angulo = ang_v, est = est,
       pontos = bind_rows(desenho %>% mutate(regime = levels(est$regime)[1]),
                          obs_x() %>% mutate(regime = levels(est$regime)[2])) %>%
         mutate(regime = factor(regime, levels = levels(est$regime))))
}
grafico_cap7 <- function(r, base_size = 13) {
  cores <- setNames(c(.cor_ctrl, .cor_obs), levels(r$est$regime))
  grade <- expand.grid(x1 = seq(-1.3, 1.3, length.out = 60), x2 = seq(-1.3, 1.3, length.out = 60)) %>%
    mutate(eta = 80 + 4 * x1 + 2 * x2)
  p1 <- ggplot() +
    geom_contour(data = grade, aes(x1, x2, z = eta), color = "grey70", bins = 10) +
    geom_point(data = r$pontos, aes(x1, x2, color = regime), size = 2.4, alpha = 0.85) +
    annotate("segment", x = 0, y = 0, xend = 4 / sqrt(20), yend = 2 / sqrt(20),
             arrow = arrow(length = unit(0.2, "cm")), linewidth = 1, color = .cor_verd) +
    scale_color_manual(values = cores, name = NULL) + coord_equal() +
    labs(x = "x1 (temperatura, codificada)", y = "x2 (tempo, codificado)") +
    .tema_cnc(base_size) + theme(legend.position = "none")
  p2 <- ggplot(r$est, aes(estimativa, fill = regime)) +
    geom_histogram(binwidth = 5, boundary = 0, alpha = 0.75, position = "identity") +
    scale_fill_manual(values = cores, name = NULL) +
    scale_x_continuous(breaks = seq(0, 180, 30)) +
    labs(x = "Erro da direção estimada (graus)", y = "Nº de simulações") +
    .tema_cnc(base_size) + guides(fill = guide_legend(ncol = .ncol_leg()))
  cowplot::plot_grid(p1, p2, rel_widths = c(0.85, 1.15))
}

# --- Capítulo 8: A/B fixo x bandit adaptativo x adesão voluntária -------------------------------
# Taxas de conversão verdadeiras: A = 10%, B = 12% (diferença 2 p.p.). 4.000 usuários.
# A/B: metade para cada braço, sorteado. Bandit: Thompson sampling em 20 lotes de 200 usuários
# (controlado, mas a alocação depende dos resultados anteriores). Adesão voluntária: os usuários
# com maior propensão a comprar são os que ativam B.
cnc_cap8 <- function(n_rep = 1000, pA = 0.10, pB = 0.12, n_tot = 4000, lote = 200) {
  set.seed(2026)
  ab <- function() { nA <- n_tot / 2; mean(rbinom(nA, 1, pB)) - mean(rbinom(nA, 1, pA)) }
  bandit <- function() {
    s <- c(0, 0); f <- c(0, 0); p <- c(pA, pB)
    for (l in seq_len(n_tot / lote)) {
      escolha <- ifelse(rbeta(lote, 1 + s[2], 1 + f[2]) > rbeta(lote, 1 + s[1], 1 + f[1]), 2, 1)
      for (k in 1:2) { nk <- sum(escolha == k); x <- rbinom(1, nk, p[k]); s[k] <- s[k] + x; f[k] <- f[k] + nk - x }
    }
    if (any(s + f == 0)) return(NA)
    s[2] / (s[2] + f[2]) - s[1] / (s[1] + f[1])
  }
  adesao <- function() {
    u <- rnorm(n_tot); pb <- plogis(qlogis(pA) + 0.8 * u)
    adere <- rbinom(n_tot, 1, plogis(1.2 * u)) == 1
    y <- rbinom(n_tot, 1, ifelse(adere, plogis(qlogis(pb) + qlogis(pB) - qlogis(pA)), pb))
    mean(y[adere]) - mean(y[!adere])
  }
  tibble(regime = rep(c("Teste A/B (sorteio fixo)", "Bandit (sorteio adaptativo)",
                        "Adesão voluntária (observacional)"), each = n_rep),
         estimativa = c(replicate(n_rep, ab()), replicate(n_rep, bandit()), replicate(n_rep, adesao()))) %>%
    filter(!is.na(estimativa)) %>% mutate(regime = fct_inorder(regime))
}
grafico_cap8 <- function(r, verdade = 0.02, base_size = 13) {
  cores <- setNames(c(.cor_ctrl, "#8b2e6e", .cor_obs), levels(r$regime))
  ggplot(r, aes(regime, 100 * estimativa, fill = regime)) +
    geom_hline(yintercept = 100 * verdade, linetype = "dashed", color = .cor_verd, linewidth = 0.9) +
    geom_violin(alpha = 0.45, color = NA) +
    geom_boxplot(width = 0.14, outlier.shape = NA, alpha = 0.9) +
    scale_fill_manual(values = cores, guide = "none") +
    labs(x = NULL, y = "Diferença estimada B − A (pontos percentuais)") +
    .tema_cnc(base_size)
}

# --- Aula 05: o que foi sorteado foi o aquário, não o peixe --------------------------------------
# 4 probióticos x 5 aquários x 4 peixes, H0 verdadeira (probiótico sem efeito), efeito de aquário
# com dp 3 e variação entre peixes com dp 2. Análise certa: F com as médias dos aquários (UE).
# Análise errada (pseudorrepetição): F tratando cada peixe como unidade experimental.
cnc_aula05 <- function(n_rep = 2000, t = 4, r = 5, q = 4, sd_aq = 3, sd_peixe = 2) {
  set.seed(2026)
  trat <- factor(rep(1:t, each = r * q)); aq <- rep(1:(t * r), each = q)
  p <- replicate(n_rep, {
    y <- rnorm(t * r, 0, sd_aq)[aq] + rnorm(t * r * q, 0, sd_peixe)
    certo <- anova(lm(tapply(y, aq, mean) ~ factor(rep(1:t, each = r))))$`Pr(>F)`[1]
    errado <- anova(lm(y ~ trat))$`Pr(>F)`[1]
    c(certo = certo, errado = errado)
  })
  tibble(analise = c("Aquário como UE (o que foi sorteado)", "Peixe como UE (pseudorrepetição)"),
         gl_erro = c(t * (r - 1), t * r * q - t),
         rejeicao = c(mean(p["certo", ] < 0.05), mean(p["errado", ] < 0.05)))
}

# --- Aula 06: ordem de execução sorteada x ordem sistemática --------------------------------------
# 3 tratamentos x 8 réplicas, H0 verdadeira, o equipamento deriva linearmente ao longo das 24
# corridas (deriva total = 'deriva' desvios-padrão). Sistemática: todas as corridas de A, depois B,
# depois C. Sorteada: ordem aleatória. Devolve a taxa de rejeição do F a 5% para cada deriva.
cnc_aula06 <- function(n_rep = 2000, derivas = c(0, 0.5, 1, 2, 3)) {
  set.seed(2026)
  N <- 24; g_sis <- factor(rep(c("A", "B", "C"), each = 8))
  bind_rows(lapply(derivas, function(d) {
    tendencia <- d * (seq_len(N) - 1) / (N - 1)
    p <- replicate(n_rep, {
      y <- tendencia + rnorm(N)
      c(sis = anova(lm(y ~ g_sis))$`Pr(>F)`[1],
        sor = anova(lm(y ~ sample(g_sis)))$`Pr(>F)`[1])
    })
    tibble(deriva = d,
           regime = c("Ordem sorteada", "Ordem sistemática (A, depois B, depois C)"),
           rejeicao = c(mean(p["sor", ] < 0.05), mean(p["sis", ] < 0.05)))
  })) %>% mutate(regime = fct_inorder(regime))
}
grafico_aula06 <- function(r, base_size = 13) {
  cores <- setNames(c(.cor_ctrl, .cor_obs), levels(r$regime))
  ggplot(r, aes(deriva, rejeicao, color = regime)) +
    geom_hline(yintercept = 0.05, linetype = "dashed", color = .cor_verd) +
    geom_line(linewidth = 1) + geom_point(size = 2.2) +
    scale_color_manual(values = cores, name = NULL) +
    guides(color = guide_legend(ncol = .ncol_leg())) +
    scale_y_continuous(labels = function(x) paste0(.fmt(100 * x, 0), "%"), limits = c(0, 1)) +
    labs(x = "Deriva do equipamento ao longo das 24 corridas (em desvios-padrão do erro)",
         y = "Rejeições de H0 verdadeira") +
    .tema_cnc(base_size)
}
