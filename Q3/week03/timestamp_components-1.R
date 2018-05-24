library(lubridate)

datestring = "2017/11/06 19:00:13"
format_string = "%Y/%m/%d %H:%M:%S"

date_obj <- as.POSIXct(datestring, format = format_string)

print(paste("Year=", year(date_obj), sep=""))
print(paste("Month=", month(date_obj), sep=""))
print(paste("Week=", week(date_obj), sep=""))
print(paste("Day=", day(date_obj), sep=""))
print(paste("Weekday=", wday(date_obj), sep=""))
print(paste("Hour=", hour(date_obj), sep=""))
print(paste("Minute=", minute(date_obj), sep=""))
print(paste("Second=", second(date_obj), sep=""))

# How to handle Unix timestamp
ts <- 1352068320
date_obj <- as.POSIXct(ts, origin="1970-01-01")
print(date_obj)
print(paste("Year=", year(date_obj), sep=""))
print(paste("Month=", month(date_obj), sep=""))
print(paste("Week=", week(date_obj), sep=""))
print(paste("Day=", day(date_obj), sep=""))
print(paste("Weekday=", wday(date_obj), sep=""))
print(paste("Hour=", hour(date_obj), sep=""))
print(paste("Minute=", minute(date_obj), sep=""))
print(paste("Second=", second(date_obj), sep=""))