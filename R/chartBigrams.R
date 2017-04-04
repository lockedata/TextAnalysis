#' Chart bigrams
#'
#' @param bigrams df containing just word1 and word2
#'
#' @export
#'
#' @examples
#' df<-data.frame(word1=LETTERS[1:5], word2=LETTERS[2:6], n=1:5)
#' chartBigrams(df)
#'
chartConnections <- function(bigrams) {
  igraph::graph_from_data_frame(bigrams) %>%
    ggraph::ggraph(layout = "fr") +
    ggraph::geom_edge_link() +
    ggraph::geom_node_point() +
    ggraph::geom_node_text(ggplot2::aes(label = name), vjust = 1, hjust = 1)+
    ggplot2::theme_void()
}
