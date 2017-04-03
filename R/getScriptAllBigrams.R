#' Get bi-grams used in Monty Python movie lines
#'
#' @param offline Use an offline copy instead of fetching data
#' @return data.frame of bi-grams from Monty Python scripts
#' @export
#'
#' @examples
#' head(getScriptAllBigrams(offline=TRUE))
getScriptAllBigrams<-function(offline=FALSE){
  if(offline) return(scriptAllBigrams)

  getScriptSpeech() %>%
    tidytext::unnest_tokens(bigram, lines, "ngrams", n=2) %>%
    dplyr::mutate(bigramid = dplyr::row_number(bigram)) %>%
    dplyr::select(dplyr::ends_with("id"),dplyr::everything())
}
