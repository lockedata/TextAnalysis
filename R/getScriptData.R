#' Scrape the web for Monty Python scripts
#'
#' Go get Monty Python scripts. This gets scripts
#' where the script is the multi-media version, not
#' the "working" version.
#'
#' @param offline Use an offline copy instead of fetching data
#' @param verbose Lots of printing
#' @return data.frame containing script info and script text
#' @export
#'
#' @examples
#' getScriptData(offline=TRUE)
getScriptData<-function(offline = FALSE, verbose=FALSE){
  if(offline) return(scriptData)

  getScriptURLs() %>%
    dplyr::mutate(ScriptText=purrr::map_chr(URL, getScript)) ->
    basicdata

 if(verbose) message("Got script raw data")

  basicdata %>%
    dplyr::filter(stringr::str_detect(name,"Script")) %>%
    dplyr::filter(!stringr::str_detect(name,"Scripts")) %>%
    dplyr::filter(!stringr::str_detect(name,"Working")) %>%
    tidyr::separate(name,into=c("Script","Part")
             ,sep=stringr::fixed(" Part "),extra = "merge",fill="right") ->
    filtereddata
 #
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

  dedupeddata %>%
    dplyr::inner_join(scriptids, by = "Script") %>%
    dplyr::mutate(scriptid=dplyr::row_number(Script)) %>%
    dplyr::filter(!is.na(ScriptText))  %>%
    dplyr::select(dplyr::ends_with("id"), dplyr::everything(), ScriptText) %>%
    dplyr::mutate(ScriptText=stringr::str_trim(
      stringr::str_replace_all(ScriptText,stringr::fixed("\t"),""))) ->
    outputdata

  if(verbose) message("Produced final format")

  return(outputdata)
}
