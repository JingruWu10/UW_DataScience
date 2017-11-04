# UW PCE Data Science Autumn 2017 Assignment 5
# Leo Salemann 11/3/17
####################################################################################

# Get the algorithm
reposURL <- "http://cran.rstudio.com/"
# install package with naive bayes if not alreay installed
if (!require("sqldf")) {install.packages("sqldf", dep=TRUE, repos=reposURL)} else {" sqldf is already installed "}
# Now that the package is installed, we want to load the package so that we can use its functions
library(sqldf)

OuterJoin1 <- function(tableA, tableB, keyA, keyB)
{
  m1 <- merge(tableA, tableB, all = TRUE)
  return(m1)
}

OuterJoin2 <- function(tableA, tableB, keyA, keyB)
{
  return(sqldf("SELECT * FROM tableA UNION ALL SELECT * GROM tableB"))
}

OuterJoin3 <- function(tableA, tableB, keyA, keyB)
{
  return(rbind(tableA, tableB))
}
