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
  m1 <- merge(x = tableA, y= tableB, by.x = keyA, by.y = keyB, all = TRUE)
  return(m1)
}

OuterJoin2 <- function(tableA, tableB, keyA, keyB)
{
  # use paste, get keys & tables.
  # LEFT JOIN UNION 
  
  # select * from tableA A left join tableB B on A.keyA = B.keyB  -- A1 A2 B1 B2
  # UNION
  # select * from tableB B left join tableA A on A.keyA = B.keyB  -- B1 B2 A1 A2
  # betware of funny colujn swapping, main query fixes this (M's email)
  
  # select A.*,  B.* from tableA A left join tableB B on A.keyA = B.keyB  -- A1 A2 B1 B2
  # UNION
  # select A.*, B.* from tableB B left join tableA A on A.keyA = B.keyB  -- B1 B2 A1 A2
  
  sqlscript <- paste("SELECT A1.*,  B1.* FROM tableA A1 left JOIN tableB B1 on A1.", keyA, " = B1.", keyB, 
                     " UNION ",
                     "SELECT A2.*,  B2.* FROM tableB B2 left JOIN tableA A2 on A2.", keyA, " = B2.", keyB)
  
  return(sqldf(sqlscript))
}

OuterJoin3 <- function(tableA, tableB, keyA, keyB)
{
  sqlscript1 <- paste("SELECT A.*,  B.* FROM tableA A left JOIN tableB B on A.", keyA, " = B.", keyB)
  sqlscript2 <- paste("SELECT A.*,  B.* FROM tableB B left JOIN tableA A on A.", keyA, " = B.", keyB)
  
  R1 <- sqldf(sqlscript1)
  R2 <- sqldf(sqlscript2) 
  
  return(unique(rbind(R1, R2)))
}
