file <- "/home/vagrant/git/UW_DataScience/Q3/HW04/Retail_Churn_Data.csv"
data <- read.csv(file, sep=",", header=TRUE)
summary(data)

data[,"Timestamp"] <- as.POSIXct(data[,"Timestamp"], format = "%m/%d/%Y %H:%M")
print(paste("Minimal date=", min(data[,"Timestamp"]), ", Max date=", max(data[,"Timestamp"]), sep=""))

Start_Date <- "1/1/2001"
End_Date <- "1/10/2001"
Time_Window <- 30 #days. Only consider customers who have activities within the recent 60 days
FM_Window <- 7 #days for frequency and monetary
Start_Date_Obj <- as.POSIXct(Start_Date, format = "%m/%d/%Y")
End_Date_Obj <- as.POSIXct(End_Date, format = "%m/%d/%Y")

check_point_date <- Start_Date_Obj
UserID <- NULL
Checkpoint <- NULL
Recency <- NULL
Frequency <- NULL
Monetary_Value <- NULL
Monetary_Quantity <- NULL
while (check_point_date <= End_Date_Obj){
  window_start <- check_point_date - as.difftime(Time_Window, unit="days")
  data_checkpoint <- data[data[,"Timestamp"] >=window_start & data[,"Timestamp"] < check_point_date, ]
  unique_users <- unique(data_checkpoint[,"UserId"])
  for (user in unique_users){
    UserID <- c(UserID, user)
    Checkpoint <- c(Checkpoint, check_point_date)
    data_checkpoint_user <- data_checkpoint[data_checkpoint[,"UserId"]==user, ] # get the activity of a user in this time window
    recency <- difftime(check_point_date, max(data_checkpoint_user[,"Timestamp"]), units="days")
    FM_Window_Start <- check_point_date - as.difftime(FM_Window, unit="days")
    data_checkpoint_user_fm <- data_checkpoint_user[data_checkpoint_user[,"Timestamp"] >= FM_Window_Start,]
    frequency <- nrow(data_checkpoint_user_fm)
    value <- sum(data_checkpoint_user_fm[,"Value"])
    quantity <- sum(data_checkpoint_user_fm[,"Quantity"])
    Recency <- c(Recency,recency)
    Frequency <- c(Frequency, frequency)
    Monetary_Value <- c(Monetary_Value, value)
    Monetary_Quantity <- c(Monetary_Quantity, quantity)
  }
  check_point_date <- check_point_date + as.difftime(1, unit="days")
}

RFM_Frame <- data.frame(UserID=UserID, Checkpoint=Checkpoint, Recency=Recency, Frequency=Frequency,
                        Value=Monetary_Value, Quantity=Monetary_Quantity)

summary(RFM_Frame)
