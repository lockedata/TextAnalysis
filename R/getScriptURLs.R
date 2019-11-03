#' Get Monty Python script URLs
#'
#' Based on `MONTYPYTHONSCRIPTSITE`, retrieve relevant
#' URLs of Monty Python scripts.
#'
#' @param offline Use an offline copy instead of fetching data
#' @return tibble containing script name and URL
#' @export
#'
#' @examples
#' head(getScriptURLs(offline=TRUE))
getScriptURLs <- function(offline=FALSE) {
  if(offline) return(scriptURLs)

  page <- xml2::read_html(MONTYPYTHONSCRIPTPAGE)

  page %>%
    rvest::html_nodes("table") %>%
    rvest::html_nodes("a") %>%
    rvest::html_text() ->
    names

  page %>%
    rvest::html_nodes("table") %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href") %>%
    paste0(MONTYPYTHONSCRIPTSITE,.) ->
    URL

  df <- tibble::tibble(name = names, URL)

  return(df[-1,])
}
