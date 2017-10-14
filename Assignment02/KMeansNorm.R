# KMeansNorm.R
# Copyright 2017 by Ernst Henle

source("KMeansHelper.R") # ClusterPlot. Samples for observations, clusterCenters, and labels.

KMeansNorm <- function(observations = sampleObservations, clusterCenters = centersGuess, normD1 = F, normD2 = F)
{
  if (normD1)
  {
    # Determine mean and standard deviation of 1st dimension in observations
    # Put code in place of this line
    # normalize 1st dimension of observations
    # Put code in place of this line
    # normalize 1st dimension of clusterCenters
    # Put code in place of this line
  }
  if (normD2)
  {
    # Determine mean and standard deviation of 2nd dimension in observations
    # Put code in place of this line
    # normalize 2nd dimension of observations
    # Put code in place of this line
    # normalize 2nd dimension of clusterCenters
    # Put code in place of this line
  }
  #clusterCenters <- KMeans(observations, clusterCenters)
  KMeansResult <- kmeans(x=observations, centers=clusterCenters)
  clusterCenters <- KMeansResult$centers
  ClusterPlot(observations=observations, centers=clusterCenters, labels=KMeansResult$cluster)
  if (normD1)
  {
    # denormalize in first dimension
    # Put code in place of this line
  } 
  if (normD2)
  {
    # denormalize in second dimension
    # Put code in place of this line
  } 
  return(clusterCenters)
}

# Q4a
# 

# Q4b
# 
# 

# Q4c
# 
# 

# Q4d
# 
# 

# Q4e
# 
# 

# Q5
# 
# 

# Q6
# 
# 

# Q7
# 
# 
