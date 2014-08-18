

toJSONArray <- function(obj, json = TRUE, nonames = TRUE){
  list2keyval <- function(l){
    keys = names(l)
    lapply(keys, function(key){
      list(key = key, values = l[[key]])
    })
  }
  obj2list <- function(df){
    l = plyr::alply(df, 1, as.list)
    if(nonames){ names(l) = NULL }
    return(l)
  }
  if (json){
    RJSONIO::toJSON(obj2list(obj))
  } else {
    obj2list(obj)
  }
}





jsonObjectListToObj <- function(objl){
  if(all(lapply(objl,class)=="JSON")){
    if(is.null(names(objl))){
      l <- Map(function(i){
        objl[[i]]@jsonstr
      },seq_along(objl))
      json <- paste0("\n[ ",paste(l,collapse=", "),"]\n")
      return(new("JSON", jsonstr = json))     
    }else{
    l <- Map(function(i){
      paste0('"',i,'": ',objl[[i]]@jsonstr)
    },names(objl))
    json <- paste0("\n{\n",paste(l,collapse=",\n"),"\n}\n")
    return(new("JSON", jsonstr = json))
    }
  }  
}



toJSONobj <- function(obj){
  if(class(obj)=="JSON"){
    message("returned same object")
    return(obj)
  } else if(class(obj) == "data.frame"){
    message("data frame detected")
    json <- toJSONArray(obj)
    out <- new("JSON", jsonstr = json)
  } else if(class(obj) == "numeric"){
    json <- as.character(obj)
    out <- new("JSON", jsonstr = json)
  } else if(class(obj) == "character"){
    json <- paste0('"',obj,'"')
    out <- new("JSON", jsonstr = json)
  } else  if(class(obj) %in% c("Field","Datatbl")){
    json <- obj$toJSON()
    out <- new("JSON", jsonstr = json)
  } else  if(class(obj) == "list"){ 
    # If obj is list, and it's elements are JSON class
    if(all(lapply(obj,class)=="JSON")){
      out_coherced <- jsonObjectListToObj(obj)
      return(out_coherced)
    }
    out <- lapply(obj, function(o){            
      #if(class(o)=="JSON")
      toJSONobj(o)      
      })
    out <- jsonObjectListToObj(out) 
    return(out)
  }
  else{
    json <- RJSONIO::toJSON(obj)
    out <- new("JSON", jsonstr = json)
  }  
  out 
}


listToJSON <- function(x){
  setClass(Class = "JSON",slots = list(jsonstr = "character"))
  toJSONobj(x)@jsonstr
}

# tbl$toJSON()
# obj <- tbl
# listToJSON(tbl)
# obj <- list(1,2,3)
# cat(toJSON(list(a="1",b=2,c=3)))
# cat(toJSON(list(1,2,3)))
# cat(toJSON("hola"))
# cat(toJSON(cars[1:3,]))
# cat(toJSON(list(a="1..",b=2,c=mtcars[1:3,1:3])))
# cat(toJSON(list(a=1,b=2,c=mtcars[1:3,1:3], d=list(x=2,y=cars[1:3,]))))
# cat(toJSON(list(a=1,fields=fld1)))
# cat(toJSON(list(a=list(1,2),b=list(c=1,c1=list(d=list(e="a",f="fles"),e=list(1,2))))))

