library(ROCR)
truth <- c(0,1,0,1)
predictions <- c(0.2, 0.5, 0.6, 0.45)
pred <- prediction(predictions, truth)

perf <- performance(pred,"tpr","fpr")
plot(perf)

auc <- performance(pred, "auc")
print(paste("AUC=", slot(auc, 'y.values')[[1]], sep=""))