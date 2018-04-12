###############################
# Retrieve data from SQL Server
###############################
# Do not run since database credentials are missing

library(RODBC)

ch<-odbcDriverConnect("DRIVER={ODBC Driver 13 for SQL Server};
                      SERVER=<URL of SQL Server>;Database=<db name>;uid=<User Name>;pwd=<Password>")
resultset <- sqlQuery(ch, "SELECT top 10 * from nyctaxi_sample")
head(resultset)

###############################
# Retrieve data from URL
###############################

# Option 1
# read.csv
url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data"
data <- read.csv(url, sep=",", header=FALSE)
head(data)

# Option 2, works even for https URL
# RCURL
library(RCurl)
myfile <- getURL(url, ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
mydat <- read.csv(textConnection(myfile), header=FALSE)
head(mydat)

###############################
# Join Two Data Frames
###############################
left_df <- data.frame(CompanyId = c(1:6), Company = c("Microsoft", "Walmart", "Google", "CitiBank", "Nike", "Kraft"))
right_df <- data.frame(CompanyId = c(1:6), State = c("WA", "AK", "CA", "NY", "OR", "IL"))
head(left_df)
head(right_df)

# Inner join (cross join)
merged_df <- merge(x = left_df, y = right_df, by = "CompanyId")
head(merged_df)

# Left and Right outer join 
# Dropping Walmart from the left data frame
left_df1 <- left_df
left_df1 <- left_df1[-2,]
head(left_df1)

left_merged_df <- merge(x = left_df1, y = right_df, all.x = TRUE)
head(left_merged_df)

right_merged_df <- merge(x = left_df1, y = right_df, all.y = TRUE)
head(right_merged_df)

#outer join
# Dropping IL from right_df
right_df1 <- right_df
right_df1 <- right_df1[-6,]
head(right_df1)

outer_merged_df <- merge(x = left_df1, y = right_df1, all = TRUE)
head(outer_merged_df)

