#' Get words used in Monty Python movie lines, without stop words removed
#'
#' @param offline Use an offline copy instead of fetching data
#' @return data.frame of words from Monty Python scripts
#' @export
#'
#' @examples
#' head(getScriptAllWords(offline=TRUE))
getScriptAllWords<-function(offline=FALSE){
  if(offline) return(scriptAllWords)

  getScriptSpeech() %>%
    tidytext::unnest_tokens(word, lines) %>%
    dplyr::mutate(wordid = dplyr::row_number(word)) %>%
    dplyr::select(dplyr::ends_with("id"),dplyr::everything())
}
