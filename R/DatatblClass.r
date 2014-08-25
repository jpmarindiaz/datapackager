#' Datapackage class description
#' @title Datatbl class
#' @description Description of Datapackage class
#' @export Datatbl
#' @exportClass Datatbl
Datatbl <- setRefClass('Datatbl', 
    fields = list(
      name = "character",
      description = "character",
      path = "character",
      ae_resource_info = "list",
      mediatype = "character",
      format = "character",
      dialect = "list",
      schema = "list",
      data ="data.frame"
    ),
    methods = list(
      initialize = function(...,
                            name = "",
                            description = "",
                            path = "",
                            ae_resource_info = list(id="table1", 
                                                    public="true",
                                                    resource_type="tbl"),
                            mediatype = "text/csv",
                            format = "csv",
                            dialect = list(delimiter=","),
                            schema = list(fields=list()),
                            data = data.frame()
                            ){
        name <<- name
        description <<- description  %||% ""
        path <<- path  %||% ""
        ae_resource_info <<- ae_resource_info %||% list(id=name,public="true",resource_type="tbl")
        mediatype <<- mediatype
        format <<- format
        dialect <<- dialect
        schema <<- schema
        data <<- data
        callSuper(...)
      },    
    toJSON = function(){       
      flds <- getRefClass()$fields()                                      
      l <- lapply(names(flds),function(name){ 
        out <- .self[[name]]
        out
      })
      names(l) <- names(flds)
      listToJSON(l)
    },
    fromJSON = function(datatbl.json){
    },
    asList = function(){
      flds <- getRefClass()$fields()                                      
      l <- lapply(names(flds),function(name){ 
        out <- .self[[name]]
        if(class(out)=="Fields"){
          out <- out$asList(out)
        }
        out
      })
      names(l) <- names(flds)
      l
    }
  )
    
) 


