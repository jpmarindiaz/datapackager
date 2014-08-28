
library(devtools)
load_all()
document()

fld1 <- Field$new(name="contry name",
                   description="iso name",
                   type="string",
                   format="XX",
                   attributes=list(id="a", datatype = "C")
                   )
fld2 <- Field$new(name="value",
                  description="population number",
                  type="number",
                  format="int",
                  attributes=list(id="b", datatype = "N")
)

cat(listToJSON(list(fld1,fld2)))

tbl <- Datatbl$new(name="tablaA",
            description = "my description",
            path = "data/test.csv",
            dialect = list(delimiter=","),
            schema = list(fields=list(fld1,fld2)),
            data = data.frame())

l <- tbl$toJSON()

cat(tbl$toJSON())

dp <- Datapackage$new(name="gdp",
                      title="country, regional",
                      description="country description",
                      resources=list(tbl, tbl))
cat(dp$toJSON())

dp.json <- read_file("inst/dp.json")
dp <- Datapackage$new()
dp
dp$fromJSON(dp.json)
dp
loadDpData(dp, dpPath="inst")
dp

load_all()

getFieldNamesByIdx(dp)
setFieldNames(dp,list(tableA = c("hola","como estás?")))
setFieldNames(dp,list(tableB = c("HOLA","COMO")))
setFieldNames(dp,list(tableA = c("h","c"),tableB = c("H","C")))
getFieldNames(dp)


df <- cars[1:2,]
dp <- newDatapkg(df)
dp
getDataframes(dp)



load_all()
df <- mtcars
dp <- newDatapkg(cars)

d <- list(a = mtcars,tab2 = cars)
dp <- newDatapkg(d)
dp
writeDatapackage(dp,"tmp")
dp$toJSON()

##  TEST tarsila
library(devtools)
load_all()
library(tarsila)
vizId <- "n_gg_hist_001"
dpPath <- system.file(file.path("viz",vizId,"ext/datapackage"),package="tarsila")
dp.json <- read_file(file.path(dpPath,"datapackage.json"))
dp <- newDatapkg(dp.json)
loadDpData(dp, dpPath)
getDataframes(dp)
tarsila::runTarsila(dp,vizId)
dp
df <- getDataframe(dp)


## 
vizId <- "sigma_005"
dpPath <- system.file(file.path("viz",vizId,"ext/datapackage"),package="tarsila")
dp.json <- read_file(file.path(dpPath,"datapackage.json"))
dp <- newDatapkg(dp.json)
loadDpData(dp, dpPath)
getDataframes(dp)
tarsila::runTarsila(dp,vizId)
df <- getDataframes(dp)



### Simple dp



### Network Dp
library(devtools)
load_all()
dataPath <- system.file("network-dp", package="datapackager")
nodes <- read.csv(file.path(dataPath,"nodes.csv"), stringsAsFactors=FALSE)
edges <- read.csv(file.path(dataPath,"edges.csv"), stringsAsFactors=FALSE)
dp <- newDatapkg(list(nodes=nodes,edges=edges))
# writeDatapackage(dp,dataPath)
dp.json <- read_file(file.path(dataPath,"datapackage.json"))
dp <- newDatapkg(dp.json)

json.str <- dp.json
dp <- Datapackage$new()
dp$fromJSON(json.str)

getDataframes(dp,withNames=TRUE)


##  TEST READ dp
library(devtools)
load_all()
dpPath <- system.file("sales-dp", package="datapackager")
dp <- readDatapackage(dpPath)

dpPath <- system.file("datapackagetmp", package="datapackager")
dp <- readDatapackage(dpPath)

dp <- newDatapkg(list(mtcars,cars)) # test names of new datapackage
dp <- newDatapkg(list(c1=mtcars,c2=cars)) # test names of new datapackage


### Check structure

dpStr <- dpStructure(dp)
ae_str <- getAeDpStr(dpStr)

dp.json <- read_file(file.path(dataPath,"datapackage.json"))
dp2 <- newDatapkg(dp.json)
dp2Str <- dpStructure(dp2)
ae_str2 <- getAeDpStr(dp2Str)

str(ae_str)
str(ae_str2)

identical(ae_str,ae_str2)






