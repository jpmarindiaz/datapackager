
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
dp$fromJSON(dp.json)
dp
loadDpData(dp, dpPath="inst")
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
dp

d <- list(a = mtcars,tab2 = cars)
dp <- newDatapkg(d)
dp






