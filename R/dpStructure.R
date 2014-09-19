
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
