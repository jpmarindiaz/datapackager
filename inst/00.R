
library(devtools)
load_all()
document()

fld1 <- Field$new(name="contry name",
                   description="iso name",
                   type="string",
                   format="XX",
                  ae_field_info=list(id="a", datatype = "C")
                   )
fld2 <- Field$new(name="value",
                  description="population number",
                  type="number",
                  format="int",
                  ae_field_info=list(id="b", datatype = "N")
)

cat(listToJSON(list(fld1,fld2)))

tbl <- Datatbl$new(name="tablaA",
            description = "my description",
            path = "data/test.csv",
            url = "http://hola.com",
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


### TEST Date 
load_all()
df <- data.frame(date = seq(as.Date("2014/1/1"), as.Date("2014/1/30"), "days"),
                 num = runif(30))
class(df$date)
dp <- newDatapkg(df)
writeDatapackage(dp,"dn")
dp <- readDatapackage("dn")

df2 <- dp$resources[[1]]$data
df <- getDataframe(dp)


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


#### Read df as string
library(devtools)
load_all()

dfstr <- "Segmento  Rango.Sell.In	Equipo	Marca	week	qt
M Mpx	425 to 450	C905 SONY	SONY ERICSSON	2009-01-01	0
M Mpx	450 to 500	M8800 PIXON SAMSUNG	SAMSUNG	2009-01-01	0
5 Mpx	200 to 225	SGH-F480 TOUCH WIZ SAMSUNG	SAMSUNG	2009-01-01	1047
5 Mpx	450 to 500	N95 NOKIA	NOKIA	2009-01-01	915"

df <- readDfString(dfstr)
str(df)
str(forceDatatype(df, "CCCCDN"))


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

dp <- newDatapkg(list(mtcars,cars)) # won't work
dp <- newDatapkg(list(c1=mtcars,c2=cars)) # named list will work

## TEST setFieldNames
library(devtools)
load_all()
dpPath <- system.file("sales-dp", package="datapackager")
dp <- readDatapackage(dpPath)
getFieldNames(dp)

fieldNames <- list(c("x","y","z","w"),c("prod","lin"))
setFieldNames(dp,)

setFieldNames(dp,list(table1=c("x","y","z","w"),table2=c("prod","lin")))


## TEST setRecordName
library(devtools)
load_all()
document()
dpPath <- system.file("sales-dp", package="datapackager")
dp <- readDatapackage(dpPath)
getRecordName(dp)
head(getDataframe(dp, withNames = TRUE))
setRecordName(dp, "transaction")
writeDatapackage(dp,"inst/sales")

## TEST POSIX lubridate
library(devtools)
load_all()
dpPath <- system.file("cadastro-dp", package="datapackager")
dp <- readDatapackage(dpPath)

df <- read.csv(file.path(dpPath,"cadastro.csv"), stringsAsFactors=FALSE)
#writeDatapackage(dp,dpPath)

getDatatypes(dp)
str(getDataframe(dp))

df2 <- getDataframe(dp)
str(df2)
df <- df2
dp2 <- newDatapkg(df2)
getDatatypes(dp2)
str(getDataframe(dp))

### Check selection
library(devtools)
load_all()
dpPath <- system.file("CDDNdatapackage", package="datapackager")
dp <- readDatapackage(dpPath)
str(getDataframe(dp, withNames=TRUE))
dpout <- getDpSelection(dp)
dpout <- getDpSelection(dp, cols = c("b","d"))
str(getDataframe(dpout, withNames=TRUE))

dpout2 <- getDpSelectionByName(dp, cols = c("Last.email.send.date","Emails.Delivered", "First.email.send.date"))
str(getDataframe(dpout2, withNames=TRUE))

### Check structure
#### OJO mapply might help

dpStr <- dpStructure(dp)
ae_str <- getAeDpStr(dpStr)

dp.json <- read_file(file.path(dataPath,"datapackage.json"))
dp2 <- newDatapkg(dp.json)
dp2Str <- dpStructure(dp2)
ae_str2 <- getAeDpStr(dp2Str)

str(ae_str)
str(ae_str2)

identical(ae_str,ae_str2)






