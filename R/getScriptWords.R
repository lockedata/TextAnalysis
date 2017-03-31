#' Get words used in Monty Python movie lines, with stop words removed
#'
#' @param offline Use an offline copy instead of fetching data
#' @return data.frame of words from Monty Python scripts
#' @export
#'
#' @examples
#' head(getScriptWords(offline=TRUE))
getScriptWords<-function(offline=FALSE){
  if(offline) return(scriptWords)

  getScriptSpeech() %>%
    tidytext::unnest_tokens(word, lines) %>%
    dplyr::anti_join(tidytext::stop_words, by = "word") %>%
    dplyr::mutate(wordid = dplyr::row_number(word)) %>%
    dplyr::select(dplyr::ends_with("id"),dplyr::everything())
}
