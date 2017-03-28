#' Scrape the web for Monty Python scripts
#'
#' Go get Monty Python scripts. This gets scripts
#' where the script is the multi-media version, not
#' the "working" version.
#'
#' @return data.frame containing script info and script text
#' @export
#'
#' @examples
#' getScriptData()
getScriptData<-function(){
  getScriptURLs() %>%
    purrr::by_row(getScript) %>%
    dplyr::bind_rows() %>%
    dplyr::filter(stringr::str_detect(name,"Script")) %>%
    dplyr::filter(!stringr::str_detect(name,"Scripts")) %>%
    dplyr::filter(!stringr::str_detect(name,"Working")) %>%
    tidyr::separate(name,into=c("Script","Part")
             ,sep=stringr::fixed(" Part "),extra = "merge",fill="right") %>%
    dplyr::mutate(Script=stringr::str_replace(Script,stringr::fixed(" Multi-media Script"),"")) %>%
    dplyr::mutate(Script=stringr::str_replace(Script,stringr::fixed(" Multi-Media Script"),"")) %>%
    dplyr::distinct() %>%
    dplyr::group_by(URL) %>%
    dplyr::filter(dplyr::row_number(Script)==1)%>%
    dplyr::ungroup() %>%
    dplyr::mutate(.,showid=dplyr::group_indices(., Script)) %>%
    dplyr::mutate(scriptid=dplyr::row_number(Script)) %>%
    dplyr::filter(!is.na(.out))  %>%
    tidyr::unnest(.out) %>%
    dplyr::select(dplyr::ends_with("id"), dplyr::everything(), ScriptText=.out)
}
