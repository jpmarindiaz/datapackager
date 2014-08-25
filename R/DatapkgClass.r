#' Datapackage class description
#' @title Datapackage class
#' @description Description of Datapackage class
#' @export Datapackage
#' @exportClass Datapackage
Datapackage <- setRefClass('Datapackage', 
                      fields = list(
                        name = "character",
                        title = "character",
                        description = "character",
                        keywords = "list",
                        last_updated = "character",
                        license = "character",
                        sources = "list",
                        resources = "list"
                      ),
                      methods = list(
                        initialize = function(...,
                                      name = "",
                                      title = "",
                                      description = "",
                                      last_updated = "",
                                      license = "",
                                      sources = list(),
                                      resources = list(Datatbl$new())
                                        ){
                                        name <<- name
                                        title <<- title
                                        description <<- description
                                        last_updated <<- last_updated
                                        license <<- license
                                        sources <<- sources
                                        resources <<- resources
                                        callSuper(...)
                                      },
                        toJSON = function(){
                          flds <- getRefClass()$fields()                                      
                          l <- lapply(names(flds),function(name){ 
                            out <- .self[[name]]
                            out
                          })
                          names(l) <- names(flds)
                          l <- emptyToNULL(l) 
                          l <- removeNull(l)
                          listToJSON(l)
                        },
                        fromJSON = function(dp.json){
                          # dp.json <- read_file("inst/dp.json")
                          l <- RJSONIO::fromJSON(dp.json)
                          name <<- l$name
                          title <<- l$title %||% ""
                          description <<- l$description %||% ""
                          last_updated <<- l$last_updated %||% ""
                          license <<- l$license %||% "PDDL-1.0"
                          sources_tmp <- l$sources %||% list(list())
                          sources <<- as.list(sources_tmp[[1]])
                          tblsList <- lapply(l$resources, function(resource){
                            # resource <- l$resources[[2]]
                            fieldsList <- lapply(resource$schema$fields,function(field){
                              # field <- resource$schema$fields[[2]]
                              fld1 <- Field$new(name= field$name,
                                                description= field$description,
                                                type= field$type,
                                                format= field$format,
                                                ae_field_info= as.list(field$ae_field_info)
                              )
                            })
                            tbl <- Datatbl$new(name=resource$name,
                                               description = resource$description,
                                               path = resource$path,
                                               dialect = list(delimiter=unname(resource$dialect)),
                                               schema = list(fields= fieldsList),
                                               data = data.frame())
                          })
                          
                          
                          resources <<- tblsList

                        },
                        show = function(){
                          flds <- getRefClass()$fields()                                      
                          l <- lapply(names(flds),function(name){ 
                            out <- .self[[name]]
                            out
                          })
                          names(l) <- names(flds)
                          l <- emptyToNULL(l) 
                          l <- removeNull(l)
                          str(l)
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






