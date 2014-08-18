Datatbl <- setRefClass('Datatbl', 
    fields = list(
      name = "character",
      id = "character",
      description = "character",
      path = "character",
      remote = "list",
      mediatype = "character",
      format = "character",
      dialect = "list",
      schema = "list",
      data ="data.frame"
    ),
    methods = list(
      initialize = function(...,
                            name = "",
                            id = "table1",
                            description = "",
                            path = "",
                            remote = list(security="public", remote=""),
                            mediatype = "text/csv",
                            format = "csv",
                            dialect = list(delimiter=","),
                            schema = list(fields=list()),
                            data = data.frame()
                            ){
        name <<- name
        id <<- name
        description <<- description
        path <<- path
        remote <<- remote
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


