file <-"/home/vagrant/git/UW_DataScience/Q3/HW03/Bank Data.csv"
data <- read.csv(file, sep=",", header=TRUE)

summary(data)

glm.full <- glm(pep ~ ., data = data, family = binomial)  #use all feaures
glm.null <- glm(pep ~ 1, data = data, family = binomial)  #use one features

library(rms)
lrm.full <- rms::lrm(pep ~ ., data = data)
fastbw(lrm.full, rule = "p", sls = 0.1)

model.aic.backward <- step(glm.full, direction = "backward", trace = 1)
summary(model.aic.backward) #minum AIC is best

model.aic.forward <- step(glm.null, direction = "forward", trace = 1, 
                          scope = ~ age + sex + region + income + married + children + car + save_act +
                            current_act + mortgage)

summary(model.aic.forward)

model.aic.both <- step(glm.null, direction = "both", trace = 1, 
                       scope = ~ age + sex + region + income + married + children + car + save_act +
                         current_act + mortgage)
summary(model.aic.both)

X <- model.matrix(pep ~ ., data=data)
fit.lasso <- glmnet(X, data[,11], family="binomial", alpha = 1, lambda=0.05)
coef(fit.lasso)

fit.ridge <- glmnet(X, data[,11], family="binomial", alpha = 0, lambda=5)
coef(fit.ridge)