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
                                     ae_field_info = "list"
                                   ),
                                   methods = list(
                                     initialize = function(...,
                                                           name = "",
                                                           description = "",
                                                           type = "",
                                                           format = "",
                                                           ae_field_info = list()
                                     ){
                                       name <<- name
                                       description <<- description %||% ""
                                       type <<- type  %||% ""
                                       format <<- format  %||% ""
                                       l <- ae_field_info
                                       l$id <- ae_field_info$id %||% "a"
                                       l$label <- ae_field_info$label %||% name
                                       l$data_type <- ae_field_info$data_type %||% "X"
                                       ae_field_info <<- l
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


