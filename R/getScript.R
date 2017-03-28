#' Return a script for a given URL
#'
#' @param df Data,frame
#'
#' @return a character vector
#' @export
#'
#' @examples
#' df<-getScriptURLs()
#' getScript(df[1,])
getScript<-function(df){
  script<-tryCatch({
    xml2::read_html(df$URL) %>%
      rvest::html_node("#content") %>%
      rvest::html_text()
  }, error = function(e){NA} )
  return(script)
}
