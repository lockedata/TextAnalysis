#' Get Monty Python script URLs
#'
#' Based on `MONTYPYTHONSCRIPTSITE`, retrieve relevant
#' URLs of Monty Python scripts.
#'
#'
#' @return tibble containing script name and URL
#' @export
#'
#' @examples
#' df<-getScriptURLs()
#' head(df)
getScriptURLs <- function() {

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

  return(df)
}
