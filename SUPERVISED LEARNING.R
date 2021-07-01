boston<- read.csv('http://lib.stat.cmu.edu/datasets/boston', sep = ';')
head(boston)
attach(boston)
summary(crim)
summary(medv)
## CORRELATION MATRIX
library(corrplot)
corr_matrix<-cor(Boston)
corrplot(corr_matrix, type="upper")
any(is.na(Boston))

## SPLITTING THE DATA

smp_size<-floor(0.50*nrow(Boston))
set.seed(1)
train_ind<-sample(seq_len(nrow(Boston)), size=smp_size)
train<-Boston[train_ind, ]
test<-Boston[-train_ind, ]
lm.fit1=lm(medv~crim,data=train)
summary(lm.fit1)
### PREDICT MODEL
require(Metrics)
pred<- predict(lm.fit1, test)
pred
rmse(pred,test[,14 ])
### PLOT MODEL
require(ggplot2)
require(plotly)
dat <- data.frame(crim = (1:50),
                  medv = predict(lm.fit1, data.frame(crim = (1:50))))
plot_ly() %>% 
  add_trace(x=~crim, y=~medv, type="scatter", mode="lines", data = dat, name = "Predicted Value") %>%
  add_trace(x=~crim, y=~medv, type="scatter", data = test, name = "Actual Value")


### MULTIPLE REGRESSION MODEL
lm.fit2=lm(medv~crim+rm + tax + lstat,data=train)
summary(lm.fit2)
pred1<-predict(lm.fit2, test)
pred1
rmse(pred1, test[,14])

##### NON-LINEAR MODELS
lm.fit3=lm(medv~crim+I(crim^2),data=train)
summary(lm.fit3)
dat <- data.frame(crim = (1:80),
                  medv = predict(lm.fit3, data.frame(crim = (1:80))))
plot_ly() %>% 
  add_trace(x=~crim, y=~medv, type="scatter", mode="lines", data = dat, name = "Predicted Value") %>%
  add_trace(x=~crim, y=~medv, type="scatter", data = test, name = "Actual Value")

pred2<-predict(lm.fit3, test)
pred2
rmse(pred2, test[,14])
#### MULTIPLE REGRESSION WITH A NON-LINEAR MODEL
lm.fit4=lm(medv~crim+I(crim^2)+rm + tax + lstat,data=train)
summary(lm.fit4)
pred3<-predict(lm.fit4, test)
rmse(pred3, test[, 14])
