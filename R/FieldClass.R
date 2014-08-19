#' Field class description
#' @title Field class
#' @description Description of Datapackage class
#' @export Field
#' @exportClass Field
Field <- setRefClass('Field', 
                                   fields = list(
                                     name = "character",
                                     description = "character",
                                     type = "character",
                                     format = "character",
                                     attributes = "list"
                                   ),
                                   methods = list(
                                     initialize = function(...,
                                                           name = "",
                                                           description = "",
                                                           type = "",
                                                           format = "",
                                                           attributes = list()
                                     ){
                                       name <<- name
                                       description <<- description
                                       type <<- type
                                       format <<- format
                                       l <- attributes
                                       l$id <- attributes$id %||% "a"
                                       l$label <- attributes$label %||% name
                                       l$datatype <- attributes$datatype %||% "X"
                                       attributes <<- l
                                       callSuper(...)
                                     },    
                                     toJSON = function(){ 
                                       flds <- getRefClass()$fields()                                      
                                       l <- lapply(names(flds),function(name){ 
                                         out <- .self[[name]]
                                         out
                                       })
                                      names(l) <- names(flds)
                                       jsonstr <- RJSONIO::toJSON(l)       
                                       jsonstr
                                     },
                                     fromJSON = function(datatbl.json){
                                     },
                                     asList = function(){
                                       flds <- getRefClass()$fields()                                      
                                       l <- lapply(names(flds),function(name){ 
                                         out <- .self[[name]]
                                         out
                                       })
                                       names(l) <- names(flds)
                                       l
                                     }
                                   )
                                   
) 


