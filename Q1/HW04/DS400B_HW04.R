# UW PCE Data Science Autumn 2017 Assignment 4
# Leo Salemann 10/27/17
##############################################3


# 2) Confusion Matrix
# |  | Yes | No
# |--|-----|----
# |P | TP  | FP
# |N | FN  | TN

# |  | ill | ok
# |--|-----|----
# |P |  7  |  5
# |N |  3  | 85



TP <- 7
FP <- 5
TN <- 85
FN <- 3

ConfusionMatrix <- matrix(c(85, 3, 5, 7), ncol = 2, byrow=TRUE)
colnames(ConfusionMatrix) <- c("ill","ok")
rownames(ConfusionMatrix) <- c("P","N")
print("2. Confusion Matrix:")
print("--------------------")
print(ConfusionMatrix)
print("--------------------")

print(paste("2.a) Senstivty:   ", round(100*TP/(TP+FN)),                                        "%"))
print(paste("2.b) Specificity: ", round(100*TN/(TN+FP)),                                        "%"))
print(paste("2.c) Accuracy:    ", round(100*sum(ConfusionMatrix[c(1,4)])/sum(ConfusionMatrix)), "%"))
print(paste("2.d) Precision:   ", round(100*TP/(TP+FP)),                                        "%"))
print(paste("2.e) Recall:      ", round(100*TP/(TP+FN)),                                        "%"))



