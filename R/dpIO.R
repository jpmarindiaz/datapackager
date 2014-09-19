
#' Load data in a data frame
#' @name loadDpDataByIdx
#' @description #' Load data in a data frame by reading the paths of the resources
#' @param Datapkg to load the data from by resource index.
#' @return Datapgk with updated data field
#' @export
#' @examples \dontrun{
#' }
loadDpDataByIdx <- function(dp, dtIdx = 1, dpPath="."){
  format <- dp$resources[[dtIdx]]$format
  delimiter <- dp$resources[[dtIdx]]$dialect$delimiter %||% ","
  path <- file.path(dpPath,dp$resources[[dtIdx]]$path)
  if(format == "csv"){
    if(is.null(httr::parse_url(path)$hostname))
      df <- read.csv(path,sep = delimiter)
    else{
      temporaryFile <- tempfile()
      download.file(path,destfile=temporaryFile, method="curl")
      df <- read.csv(temporaryFile, sep = delimiter)
    }
  }
  names(df) <- letters[1:ncol(df)] # TODO match column names with fieldId's
  dtypes <- getDatatypeByIdx(dp,dtIdx)
  dtypes <- strsplit(dtypes,"")[[1]]
  for (i in seq_along(dtypes)){
    if(dtypes[i]=="N"){df[,i]<- as.numeric(df[,i])}
    if(dtypes[i]=="C"){df[,i]<- as.factor(df[,i])}
    if(dtypes[i]=="D"){
      d <- as.Date(df[,i])
      d <- as.POSIXct(df[,i])
      df[,i]<- d
    } 
  }
  dp$resources[[dtIdx]]$data <- df
  dp
}


#' Load data in a data field for each resource
#' @name loadDpData
#' @description #' Load data in a data field for each resource
#' @param Datapkg to load the data from
#' @return Datapgk with updated data field
#' @export
#' @examples \dontrun{
#' }
loadDpData <- function(dp, dpPath = "."){
  l <- lapply(seq_along(dp$resources),function(i){loadDpDataByIdx(dp, dtIdx=i, dpPath)})
  names(l) <- Map(function(tbl){tbl$name},dp$resources)
  l
}

#' Read datapackage in json
#' @name readDatapackage
#' @description Write datapackage in json
#' @param dp
#' @return logical TRUE if successful, FALSE if directory already exists
#' @export
#' @examples \dontrun{
#' mysite <- "/home/david/github/mysite"
#' skeleton(mysite)
#' }
readDatapackage <- function(dpPath="."){  
  dp.json <- read_file(file.path(dpPath,"datapackage.json"))
  dp <- newDatapkg(dp.json)
  loadDpData(dp, dpPath)
  dp
}

#' Write datapackage in json
#' @name WriteDatapackage
#' @description Write datapackage in json
#' @param dp
#' @return logical TRUE if successful, FALSE if directory already exists
#' @export
#' @examples \dontrun{
#' mysite <- "/home/david/github/mysite"
#' skeleton(mysite)
#' }
writeDatapackage <- function(dp, path="."){  
  l <- getDataframes(dp, withNames = TRUE)
  lapply(seq_along(l),function(i){
    # Write csv files
    filename <- file.path(path,paste0(names(l)[i],".csv"))
    dp$resources[[i]]$path <- basename(filename)
    dir.create(dirname(filename),showWarnings = FALSE, recursive = TRUE)
    write.csv(l[[i]],filename,row.names = FALSE)
    # Do not clean data from dp, it might be used to write more datapackages
    # dp$resources[[i]]$data <- data.frame()
  })  
  jsonstr <- dpToJSON(dp)
  write_file(jsonstr, file.path(path,"datapackage.json")) 
  message("datapackage written to: ", file.path(path,"datapackage.json"))
}