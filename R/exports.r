#' Creates a new Datapackage
#' @name newDatapkg
#' @description Creates a new datapackage from json, data frame, list of data frames, list of data tables (see Datatbl reference class)
#' @param d might be a json string, data frame or list of data frames.
#' @return dp
#' @export
#' @examples \dontrun{
#' dp <- newDatapkg(mtcars)
#' }

newDatapkg <- function(d, name=NULL, datatypes = NULL){
  if(class(d) == "data.frame"){     
    tblName <- deparse(substitute(d))
    datatypes <- datatypes %||% guessDatatype(d)
    d <- forceDatatype(d,datatypes)
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
  #getDatatypeByIdx(dp, Idx)
  #forceDatatype(df, )
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


#' Set fieldnames in a list
#' @name getDpSelection
#' @description Set fieldnames by resource index
#' @param dp datapackage
#' @return dp with updated fieldanmes
#' @export
#' @examples \dontrun{
#' }
getDpSelection <- function(dp, dtIdx = 1, cols = NULL, 
                           opts = list(unique=FALSE,
                                       noEmpty = FALSE
                                       )){
  #cols <- c("a","b")
  #dtIdx <- 1
  df <- getDataframe(dp, dtIdx = dtIdx) 
  cols <- cols %||% names(df) 
  dtypes <- getDatatypeByIdx(dp,dtIdx)
  dtypes <- strsplit(dtypes,"")[[1]]
  names(dtypes) <- letters[1:length(dtypes)]
  dtypes <- dtypes[cols]
  nms <- getFieldNamesByIdx(dp, dtIdx = dtIdx)
  names(nms) <- letters[1:length(nms)]  
  if (!is.null(cols)) {
    if(length(cols) >1) {
      dfout <- df[,cols]
    }
    else{
      dfout <- data.frame(a= df[,c(cols)])
    }    
  }  
  d <- forceDatatype(dfout, dtypes, pasted= FALSE)
  names(d) <- nms[cols]
  opts$unique <- opts$unique %||% FALSE 
  opts$noEmpty <- opts$noEmpty %||% FALSE 
  if(opts$unique){d <- d[!duplicated(d),]}
  if(opts$noEmpty){d <- d[!is.empty(d),]}
  dpout <- newDatapkg(d)  
}


#' Set fieldnames in a list
#' @name getDpSelectionByName
#' @description Set fieldnames by resource index
#' @param dp datapackage
#' @return dp with updated fieldanmes
#' @export
#' @examples \dontrun{
#' }
getDpSelectionByName <- function(dp, dtIdx = 1, cols = c()){
  # cols <- c("disp","mpg")
  colNms <- getFieldNamesByIdx(dp, dtIdx = dtIdx)
  colsIds <- letters[match(cols, colNms)]
  getDpSelection(dp, dtIdx = dtIdx, cols = colsIds) 
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
