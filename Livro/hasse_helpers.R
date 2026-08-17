# Helper compartilhado para desenhar diagramas de Hasse (estrutura do delineamento) em ggplot2.
# Cada capitulo que usa diagramas de Hasse deve dar source() neste arquivo no seu chunk de setup
# (bookdown roda cada capitulo em uma sessao R nova -- new_session: yes em _bookdown.yml -- entao
# nao ha como compartilhar a funcao via objeto de sessao, só via source de arquivo).
#
# nos:     data.frame(termo, df, x, y)      -- um no por termo/fonte de variacao, com posicao manual
# arestas: data.frame(de, para)             -- pares (termo mais grosso, termo imediatamente mais
#                                               fino) que definem as arestas do diagrama
plot_hasse <- function(nos, arestas, titulo = NULL) {
  arestas_xy <- arestas %>%
    dplyr::left_join(nos %>% dplyr::select(termo, x0 = x, y0 = y), by = c("de" = "termo")) %>%
    dplyr::left_join(nos %>% dplyr::select(termo, x1 = x, y1 = y), by = c("para" = "termo"))

  ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = arestas_xy,
      ggplot2::aes(x = x0, y = y0, xend = x1, yend = y1),
      color = "grey45", linewidth = 0.6
    ) +
    ggplot2::geom_label(
      data = nos,
      ggplot2::aes(x = x, y = y, label = paste0(termo, "\n(", df, " gl)")),
      size = 3.5, label.padding = ggplot2::unit(0.3, "lines"),
      fill = "white", color = "grey15"
    ) +
    ggplot2::theme_void(base_size = 13) +
    ggplot2::labs(title = titulo) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"))
}
