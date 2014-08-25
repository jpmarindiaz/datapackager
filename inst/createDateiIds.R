
#Number of possible data structures for different columns
# Count objects with repetitions, order not important
# (n+r-1)!/(r!(n-1)!)

datStrCombn <- function(dat,n){
  df <- expand.grid(rep(list(dat),n))
  dd <- as.data.frame(t(apply(df,1,sort)))
  dd[!duplicated(dd),] 
}

dat <- c("-","C","D","N","O")
n <- 8
d <- datStrCombn(dat,n)

# Create datei

# Contatenate dataframe by row
d_args <- c(d, sep="")
labels <- do.call(paste, d_args)
labels <- gsub("-","",labels, fixed=TRUE)
ids <- paste0("d",seq(0,nrow(d)-1))


dateiIds <- data.frame(id = ids, label = labels)

save(dateiIds, file="data/dateiIds.rda")



