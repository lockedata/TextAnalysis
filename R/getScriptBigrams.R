#' Get bi-grams used in Monty Python movie lines, without stop words
#'
#' @param offline Use an offline copy instead of fetching data
#' @return data.frame of bi-grams from Monty Python scripts
#' @export
#'
#' @examples
#' head(getScriptBigrams(offline=TRUE))
getScriptBigrams<-function(offline=FALSE){
  if(offline) return(scriptBigrams)

  getScriptAllBigrams() %>%
    tidyr::separate(bigram, c("word1", "word2"), sep = " ") %>%
    dplyr::filter(!word1 %in% tidytext::stop_words$word) %>%
    dplyr::filter(!word2 %in% tidytext::stop_words$word) %>%
    tidyr::unite(bigram, word1, word2, sep = " ")
}
