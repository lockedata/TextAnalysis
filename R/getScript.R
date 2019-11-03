#' Return a script for a given URL
#'
#' @param url A script url
#'
#' @return a string
#' @export
#'
#' @examples
#' df<-getScriptURLs()
#' getScript(df[1,"URL"])

getScript<-function(url){
  script<-tryCatch({
    xml2::read_html(url) %>%
      rvest::html_node("#content") %>%
      rvest::html_text()
  }, error = function(e){
    warning(e)
    NA
    } )
  return(script)
}
