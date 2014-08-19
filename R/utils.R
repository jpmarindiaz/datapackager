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