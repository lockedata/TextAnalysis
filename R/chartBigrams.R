#' Chart bigrams
#'
#' @param bigrams df containing just word1 and word2
#'
#' @export
#'
#' @examples
#' df<-data.frame(word1=LETTERS[1:5], word2=LETTERS[2:6])
#' chartBigrams(df)
#'
chartBigrams <- function(bigrams) {
  a <- grid::arrow(type = "closed")

  bigrams %>%
    igraph::graph_from_data_frame() %>%
    ggraph::ggraph() +
    ggraph::geom_edge_link(ggplot2::aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    ggraph::geom_node_point(color = "#2165B6", size = 5) +
    ggraph::geom_node_text(ggplot2::aes(label = name), vjust = 1, hjust = 1) +
    ggplot2::theme_void()
}
