x_mean <- 100
x_sd <- 10
x0 <- rnorm(20, mean=x_mean, sd=x_sd)
print(x0)

# Now, change some values to make some outliers
x0[4] <- 1054
x0[18] <- -200
x0[7] <- 3289
print(x0)
 
x <- x0
print(paste("Mean of x = ", mean(x), " and SD = ", sd(x), sep=""))

pct_10_90 <- quantile(x, c(0.1, 0.9))
print(paste("10th percentile = ", pct_10_90[1], " and 90th percentile = ", pct_10_90[2], sep=""))
x[x<pct_10_90[1]] <- pct_10_90[1]
x[x>pct_10_90[2]] <- pct_10_90[2]

print(paste("Mean of x = ", mean(x), ", and SD = ", sd(x), " after correcting the outliers", sep=""))





# Now, let's see how robust statistics can help
median <- quantile(x0, 0.5)
x_mad <- mad(x0) #output of mad is actually 1.4826*mad
print(paste("Median = ", median, ", SD estimated from MAD = ", x_mad, sep=""))