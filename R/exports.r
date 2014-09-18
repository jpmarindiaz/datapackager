#' Creates a new Datapackage
#' @name newDatapkg
#' @description Creates a new datapackage from json, data frame, list of data frames, list of data tables (see Datatbl reference class)
#' @param d might be a json string, data frame or list of data frames.
#' @return dp
#' @export
#' @examples \dontrun{
#' dp <- newDatapkg(mtcars)
#' }

newDatapkg <- function(d, name=NULL){
  if(class(d) == "data.frame"){     
    tblName <- deparse(substitute(d))
    tbl <- newDatatblFromDataframe(d, tblName)
    name <- name %||% paste0("Datapackage-",tblName)
    dp <- Datapackage$new(name=name,
                          resources=list(tbl))
  }
  if(unlist(unique(lapply(d,class))) == "data.frame"){
    if(is.null(names(d))) stop("d must be a named list, e.g. newDatapkg(list(nm1=mtcars,nm2=cars))")
    nms <- names(d) %||% paste0("table",seq_along(d))
    tblList <- lapply(seq_along(d),function(df){
      tbl <- newDatatblFromDataframe(d[[df]],nms[df])
    })
    name <- name %||% paste0("Datapackage-",paste(names(d),collapse="-"))
    dp <- Datapackage$new(name=name,
                          resources=tblList)
  }
  if(class(d)== "character"){
    json.str <- d
    dp <- Datapackage$new()
    dp$fromJSON(json.str)
  }
  dp
}

#' Creates a new Datatbl from a dataframe
#' @name newDatatblFromDataframe
#' @description Creates a new datapackage from json, data frame, list of data frames, list of data tables (see Datatbl reference class)
#' @param d might be a json string, data frame or list of data frames.
#' @return dp
#' @examples \dontrun{
#' tbl <- newDatatblFromDataframe(mtcars)
#' }
newDatatblFromDataframe <- function(df, dfname = "table1"){
  fieldNames <- names(df)
  classes <- unname(unlist(lapply(df,function(col){class(col)[1]})))
  data_types <- Map({function(c){
    if(c=="numeric"||c=="integer") "N"
    else if(c=="Date" || any(grepl("POSIX",c)) ) "D"
    else "C"
  }},classes)
  fieldList <- lapply(seq_along(fieldNames), function(i){    
    Field$new(
      name=fieldNames[i],
      type=classes[i],
      ae_field_info=list(id=letters[i], data_type = data_types[[i]])
    )
  }) 
  names(df) <- letters[1:ncol(df)]
  tbl <- Datatbl$new(name=dfname,
                     ae_resource_info = list(id=dfname, 
                                             public="true",
                                             resource_type="tbl",
                                             record_name="observation"),
                     schema = list(fields= fieldList),
                     data = df)  
  tbl
}

#' Set the data contents of a datapkg object as a data frame
#' @name setDataframe
#' @description Get the contents of a data package as a data frame
#' @param dp Input datapackage
#' @param dtIdx Index of the resource to get the data frame from. Defaults to 1.
#' @return data frame
#' @export
#' @examples \dontrun{
#' dp <- newDatapkg(mtcars)
#' getDataframe(dp, withNames=TRUE) 
#' }

setDataframe <- function(dp, df, dtIdx = 1){
  names(df) <- letters[1:ncol(df)]
  dp$resources[[dtIdx]]$data <- df
  df
}

#' Set the data contents of a datapkg object as a data frame
#' @name setRecordName
#' @description Get the contents of a data package as a data frame
#' @param dp Input datapackage
#' @param dtIdx Index of the resource to get the data frame from. Defaults to 1.
#' @return data frame
#' @export
#' @examples \dontrun{
#' dp <- newDatapkg(mtcars)
#' getDataframe(dp, withNames=TRUE) 
#' }

setRecordName <- function(dp, recordName, dtIdx = 1){
  ## TODO for dps with more than 1 resource
  dp$resources[[dtIdx]]$ae_resource_info$record_name <- recordName
  dp
}

#' Get the data contents of a datapkg object as a data frame
#' @name getRecordName
#' @description Get the contents of a data package as a data frame
#' @param dp Input datapackage
#' @param dtIdx Index of the resource to get the data frame from. Defaults to 1.
#' @return data frame
#' @export
#' @examples \dontrun{
#' dp <- newDatapkg(mtcars)
#' getDataframe(dp, withNames=TRUE) 
#' }

getRecordName <- function(dp, dtIdx = 1){
  ## TODO for dps with more than 1 resource
  dp$resources[[dtIdx]]$ae_resource_info$record_name
}

#' Get the data contents of a datapkg object as a data frame
#' @name getDataframe
#' @description Get the contents of a data package as a data frame
#' @param dp Input datapackage
#' @param dtIdx Index of the resource to get the data frame from. Defaults to 1.
#' @return data frame
#' @export
#' @examples \dontrun{
#' dp <- newDatapkg(mtcars)
#' getDataframe(dp, withNames=TRUE) 
#' }

getDataframe <- function(dp, dtIdx = 1, withNames = FALSE){
  df <- dp$resources[[dtIdx]]$data
  if(withNames) names(df) <- getFieldNamesByIdx(dp, dtIdx)
  df
}

#' Get all data contents of a datapkg object as a list of data frames.
#' @name getDataframes
#' @description Get all data contents of a datapkg object as a list of data frames.
#' @param dp, dtIdx is the id of the resource
#' @return data frame
#' @export
#' @examples \dontrun{
#' dp <- newDatapkg(list(mt = mtcars[1:4,], mycars = cars[1:2,]))
#' getDataframes(dp, withNames = TRUE) 
#' }

getDataframes <- function(dp, withNames = FALSE){
  l <- lapply(seq_along(dp$resources),function(i){
    # i <- 1
    getDataframe(dp, dtIdx=i, withNames)
    })
  names(l) <- Map(function(tbl){tbl$name},dp$resources)
  l
}


#' Get datatype by resource index
#' @name getDatatypeByIdx
#' @description Get datatype by resource index
#' @param dp datapackage
#' @param Idx resource index, defaults to 1.
#' @return character vector of datatypes for datapackage resource with index = Idx
#' @export
#' @examples \dontrun{
#' }

getDatatypeByIdx <- function(dp, dtIdx = 1){
  l <- dp$resources[[dtIdx]]$schema$fields
  l <- lapply(l,function(f){f$ae_field_info$data_type})
  paste(unlist(l),collapse="")
}

#' Get datatypes for all resources in a list
#' @name getDatatypes
#' @description Get data_types for all resources in a list
#' @param dp datapackage
#' @return list of data_types for each resource
#' @export
#' @examples \dontrun{
#' }

getDatatypes <- function(dp){
  l <- lapply(seq_along(dp$resources),function(i){
    getDatatypeByIdx(dp, dtIdx=i)
    })
  names(l) <- Map(function(tbl){tbl$name},dp$resources)
  l
}

#' Get datatypes for all resources in a list
#' @name getResourceNames
#' @description Get data_types for all resources in a list
#' @param dp datapackage
#' @return list of data_types for each resource
#' @export
#' @examples \dontrun{
#' }

getResourceNames <- function(dp){
  l <- lapply(dp$resources,function(resource){
    resource$name
  })
  unlist(l)
}

#' Get fieldnames by resource index
#' @name getFieldNamesByIdx
#' @description Get fieldnames by resource index
#' @param dp datapackage
#' @param Idx resource index, defaults to 1.
#' @return character vector of datatypes for datapackage resource with index = Idx
#' @export
#' @examples \dontrun{
#' }

getFieldNamesByIdx <- function(dp, dtIdx = 1){
  l <- dp$resources[[dtIdx]]$schema$fields
  l <- lapply(l,function(f){f$name})
  as.vector(unlist(l))
}

#' Get fieldnames in a list
#' @name getFieldNamesByIdx
#' @description Get fieldnames by resource index
#' @param dp datapackage
#' @return list of fieldanmes for the datapackage
#' @export
#' @examples \dontrun{
#' }

getFieldNames <- function(dp, withLabels = FALSE){
  l <- lapply(seq_along(dp$resources),function(i){getFieldNamesByIdx(dp, dtIdx=i)})
  names(l) <- Map(function(tbl){tbl$name},dp$resources)
  l
}

#' Set fieldnames in a list
#' @name getFieldNamesByIdx
#' @description Set fieldnames by resource index
#' @param dp datapackage
#' @return dp with updated fieldanmes
#' @export
#' @examples \dontrun{
#' }

setFieldNames <- function(dp, fieldNames){
  if(class(fieldNames)!= "list"){
    stop("labels has to be a list of character vectors")
  } 
  # fieldNames <- list(tableA = c("hola","como estás?"))
  tblnames <- names(fieldNames) %||% getResourceNames(dp)
  lapply(tblnames, function(tblname){    
    dtbl <- Find(function(r){r$name == tblname},dp$resources) 
    l <- dtbl$schema$fields
    l <- Map(function(i){
          l[[i]]$name <- fieldNames[[tblname]][i]
          l[[i]]},seq_along(l))
    dtbl$schema$fields <- l
  })
  getFieldNames(dp)
}

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
  if(dateiId == "CD") 
    d <- data.frame(sample(LETTERS[1:4],20, replace=TRUE),
                    dat=as.POSIXct(as.Date(100*runif(n),origin = "2014-01-01")))
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

#' Datapackage to JSON, removes data before transforming to JSON
#' @name dpToJSON
#' @description Write datapackage in json
#' @param dp
#' @return json string with the contents of the Datapackage
#' @export
#' @examples \dontrun{
#' }
dpToJSON <- function(dp){
  l <- dp$asList()   
  l <- emptyToNULL(l)   
  l <- removeNull(l)
  # l <- lapply(seq_along(l$resources),function(i){
  #   l$resources[[i]]$data <- NULL
  # })
  listToJSON(l)
}

#' Check if a datapackage has same structure as a reference datapackage
#' @name dpStructure
#' @description Check if two datapackages have the same structure
#' @param l Datapackage 
#' @return json string with the contents of the Datapackage
#' @export
#' @examples \dontrun{
#' }
dpStructure <- function(l){
  if(class(l)=="Datapackage") l <- l$asList()
  if(class(l) == "list" && length(l)>0){
    if(all(lapply(l,class)=="Datatbl")) l <- lapply(l,function(tbl){tbl$asList()})
    if(all(lapply(l,class)=="Field")) l <- lapply(l,function(fld){fld$asList()})
    nms <- seq_along(l)
    try(nms <- ls(l), silent=TRUE)
    ltmp <- lapply(setNames(nms,nms),function(nm){
      print(nm)
      # nm <- setNames(nms,nms)[6]
      # nm <- setNames(nms,nms)[2]
      # l[[nm]]
      if(class(l[[nm]])=="list" && length(l)>0){
        dpStructure(l[[nm]]) 
      }else{
        return(list(propvalue = l[[nm]],class=class(l[[nm]])))
        #return(class=class(l[[nm]]))
      }      
    }) 
  } else if(class(l) == "list" && length(l) == 0){
    return(list(propvalue = list(), class="list"))
  } else
  {
    return(list(propvalue = l[[nm]], class=class(l[[nm]])))
    #return(class=class(l[[nm]]))
  }  
}

#' Check if a datapackage has same ae_structure as a reference datapackage
#' @name getAeDpStr
#' @description Check if two datapackages have the same structure
#' @param dp1 Datapackage 
#' @param dp2 Datapackage 
#' @return json string with the contents of the Datapackage
#' @export
#' @examples \dontrun{
#' }
getAeDpStr <- function(dpStr){
  l <- dpStr
  lapply(l$resources,function(x){
    # x <- l$resources[[1]]
    ae_resource_info <- x$ae_resource_info
    ae_fields_info <- lapply(x$schema$fields,function(y){y$ae_field_info})
    list(ae_resource_info = ae_resource_info, ae_fields_info =  ae_fields_info)
  })     
}
