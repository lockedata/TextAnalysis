% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getScript.R
\name{getScript}
\alias{getScript}
\title{Return a script for a given URL}
\usage{
getScript(url)
}
\arguments{
\item{url}{A script url}
}
\value{
a string
}
\description{
Return a script for a given URL
}
\examples{
df<-getScriptURLs()
getScript(df[1,"URL"])
}
