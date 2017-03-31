#' Get Monty Python script lines that are speech
#'
#' @param offline Use an offline copy instead of fetching data
#' @return data.frame of lines from Monty Python scripts
#' @export
#'
#' @examples
#' head(getScriptSpeech(offline=TRUE))
getScriptSpeech<-function(offline=FALSE){
  if(offline) return(scriptSpeech)

  lines<- getScriptLines()

  lines %>%
    dplyr::filter(!is.na(Character)) %>%
    dplyr::group_by(Character) %>%
    dplyr::count() %>%
    dplyr::mutate(characterid=dplyr::row_number(Character)) %>%
    dplyr::select(-n) ->
    characterids

  lines %>%
    dplyr::inner_join(characterids, by = "Character") %>%
    dplyr::filter(Action == FALSE) %>%
    dplyr::select(dplyr::ends_with("id"),dplyr::everything())
}
