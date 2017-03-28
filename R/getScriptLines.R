#' Get Monty Python script speech lines
#'
#' @return data.frame of speech lines from Monty Python scripts
#' @export
#'
#' @examples
#' head(getScriptLines())
getScriptLines<-function(){
  getScriptData() %>%
    tidytext::unnest_tokens(lines,ScriptText,token="lines",collapse = TRUE) %>%
    dplyr::filter(lines!="") %>%
    dplyr::mutate(lineid=dplyr::row_number(lines)) %>%
    dplyr::mutate(Line = stringr::str_detect(lines,stringr::fixed(":"))) %>%
    dplyr::mutate(Action=stringr::str_detect(lines,stringr::fixed("["))) %>%
    dplyr::mutate(Character=stringr::str_extract(lines,"^(.+)\\b\\:")) %>%
    dplyr::mutate(Character=stringr::str_replace(Character,stringr::fixed(":"),"")) %>%
    dplyr::mutate(lines=stringr::str_replace(lines,"^(.+)\\:","")) %>%
    dplyr::select(dplyr::ends_with("id"),dplyr::everything())

}
