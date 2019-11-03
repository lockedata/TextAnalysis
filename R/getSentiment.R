#' Send text to Microsoft Cognitive Services' Sentiment API
#'
#' Send lines of text to an API to get the sentiment score returned
#'
#' @param textdf A data.frame consisting of two cols with colnames `c("id","text")`.
#' Optionally you can also provide a "language" column with ISO country codes,
#' otherwise it will default to "en".
#' @param apikey Your key for working with Microsoft Cognitive Services
#' @param cogapi Your API endpoint for Microsoft Cognitive Services
#'
#' @return response A data.frame with id and a sentiment score
#'
#' @export
#'

getSentiment<-function(textdf, apikey=NULL, cogapi=NULL){
  if(is.null(apikey)) apikey<-APIKEY
  if(is.null(cogapi)) cogapi<-ENDPOINT
  stopifnot(inherits(textdf, "data.frame"))
  if(!("language" %in% colnames(textdf))) textdf$language <-"en"
  tosend<-jsonlite::toJSON(list(documents= textdf))
  # Construct a request
  response<-httr::POST(cogapi,
                       httr::add_headers(`Ocp-Apim-Subscription-Key`=apikey),
                       body=tosend)

  respcontent<-httr::content(response, as="text")
  responses<-jsonlite::fromJSON(respcontent)$documents
  if(class(textdf$id) %in% c("numeric","integer")) responses$id<-as.numeric(responses$id)
  # Combine
  tidy_reponses <-dplyr::left_join(textdf, responses, by="id")

  return( tidy_reponses)
}
