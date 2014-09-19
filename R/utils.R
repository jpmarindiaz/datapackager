forceDatatype <- function(df, datatypes, pasted=TRUE){
  dtypes <- datatypes
  if(pasted) dtypes <- strsplit(datatypes, "")[[1]]
  if(ncol(df)!= length(dtypes)) stop("number of df cols must be the same as data types length")
  for (i in seq_along(dtypes)){
    if(dtypes[i]=="N"){df[,i]<- as.numeric(df[,i])}
    if(dtypes[i]=="C"){df[,i]<- as.factor(df[,i])}
    if(dtypes[i]=="D"){
      d <- as.Date(df[,i])
      d <- as.POSIXct(df[,i])
      df[,i]<- d
    } 
  }  
  df
} 


`%||%` <- function (x, y) 
{
  if (is.empty(x)) 
    return(y)
  else if (is.null(x) || is.na(x))
    return(y)
  else if( class(x)=="character" && nchar(x)==0 )
    return(y)
  else x
}

is.empty <- function(x){
  #   !is.null(x)
  !as.logical(length(x))  
}

read_file <- function (file, warn = F, ...) 
{
  paste(readLines(file, warn = warn, ...), collapse = "\n")
}

write_file <- function (x, file) 
{
  dir.create(dirname(file),showWarnings = FALSE, recursive = TRUE)
  writeLines(x, file)
}