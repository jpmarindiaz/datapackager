
#' Creates random datapackages for a given datatype.
#' @name getDateiSample
#' @description Sets up the directory structure for a new static site
#' @param site path to the directory you want to set up
#' @return logical TRUE if successful, FALSE if directory already exists
#' @export
#' @examples \dontrun{
#' mysite <- "/home/david/github/mysite"
#' skeleton(mysite)
#' }
getDateiSample <- function(dateiId, n=20, asDp = FALSE, random=TRUE){ 
  if(dateiId == "C") 
    d <- data.frame(x=sample(LETTERS[1:4],n, replace=TRUE))
  if(dateiId == "D") 
    d <- data.frame(dat=as.POSIXct(as.Date(100*runif(n),origin = "2014-01-01")))
  if(dateiId == "N") 
    d <- data.frame(x=runif(n))
  if(dateiId == "NN") 
    d <- data.frame(x=runif(n), y = rpois(n,10.7))
  if(dateiId == "CN") 
    d <- data.frame(x=sample(LETTERS[1:4],n, replace=TRUE),
                    x=runif(n))
  if(dateiId == "CD") {
    d <- data.frame(sample(LETTERS[1:4],20, replace=TRUE),
                    dat=as.POSIXct(as.Date(100*runif(n),origin = "2014-01-01")))
    names(d) <- c("CATEGORY","DATE")
  }
  if(dateiId == "CDN") 
    d <- data.frame(x=sample(LETTERS[1:4],20, replace=TRUE),
                    dat=as.POSIXct(as.Date(100*runif(n),origin = "2014-01-01")),
                    nume=runif(n))
  if(dateiId == "C") d <- data.frame(sample(LETTERS[1:4],20, replace=TRUE))
  if(dateiId == "C") d <- data.frame(sample(LETTERS[1:4],20, replace=TRUE))
  if(is.null(d)) d <- mtcars
  if(asDp) {d <- newDatapkg(d)}
  d
}

