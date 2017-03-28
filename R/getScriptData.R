#' Scrape the web for Monty Python scripts
#'
#' Go get Monty Python scripts. This gets scripts
#' where the script is the multi-media version, not
#' the "working" version.
#'
#' @param verbose Lots of printing
#' @return data.frame containing script info and script text
#' @export
#'
#' @examples
#' getScriptData()
getScriptData<-function(verbose=TRUE){
  getScriptURLs() %>%
    purrr::by_row(getScript) %>%
    dplyr::bind_rows() ->
    basicdata

 if(verbose) message("Got script raw data")

  basicdata%>%
    dplyr::filter(stringr::str_detect(name,"Script")) %>%
    dplyr::filter(!stringr::str_detect(name,"Scripts")) %>%
    dplyr::filter(!stringr::str_detect(name,"Working")) %>%
    tidyr::separate(name,into=c("Script","Part")
             ,sep=stringr::fixed(" Part "),extra = "merge",fill="right") ->
    filtereddata

  if(verbose) message("Filtered raw data")

  filtereddata%>%
    dplyr::mutate(Script=stringr::str_replace(Script,stringr::fixed(" Multi-media Script"),"")) %>%
    dplyr::mutate(Script=stringr::str_replace(Script,stringr::fixed(" Multi-Media Script"),"")) %>%
    dplyr::distinct() %>%
    dplyr::group_by(URL) %>%
    dplyr::filter(dplyr::row_number(Script)==1)%>%
    dplyr::ungroup() ->
    dedupeddata

  if(verbose) message("Deduped data")

  dedupeddata %>%
    dplyr::group_by(Script) %>%
    dplyr::count() %>%
    dplyr::mutate(showid=dplyr::row_number(Script)) %>%
    dplyr::select(-n) ->
    scriptids

  dedupeddata%>%
    dplyr::inner_join(scriptids) %>%
    dplyr::mutate(scriptid=dplyr::row_number(Script)) %>%
    dplyr::filter(!is.na(.out))  %>%
    tidyr::unnest(.out) %>%
    dplyr::select(dplyr::ends_with("id"), dplyr::everything(), ScriptText=.out) ->
    outputdata

  if(verbose) message("Produced final format")

  return(outputdata)
}
