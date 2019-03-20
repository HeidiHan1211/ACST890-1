#### Question 1 ####
P <- function(C, FV, n, y){
  t <- seq(0.5, n, 0.5)
  P <- sum(C*exp(-y*t))+FV*exp(-y[2*n]*t[2*n])
  return(P)
}
y <- rep(0.1, 2)
P(10,100,1,y)

  

#### Question 3 ####
# Part a
dataset <- read.csv(file = "singapore.economy.csv", header = T)
# Part b
dataset <- na.omit(dataset)
# Part c
dataset$time <- ts(dataset$time)
plot(dataset$time,dataset$gdp,
     type='l',
     xlab='Time',ylab='GDP (%)',main='Singapore GDP growth')
# Part d
dataset$period <- factor(dataset$period)
m <- aggregate(dataset$gdp, by=list(dataset$period), FUN = mean)
s <- aggregate(dataset$gdp, by=list(dataset$period), FUN = sd)
stat.table <- cbind(m, s[,2])
colnames(stat.table) <- c('Period','Mean','Standard deviation')
stat.table
# Part e
pairs(dataset[,3:dim(dataset)[2]])
# Part f
mdl1 <- lm(gdp~exp, data = dataset)
summary(mdl1)
# Part g
mdl2 <- lm(gdp~exp+epg+hpr+oil+gdpus+crd, data = dataset)
summary(mdl2)
# Part h
# 5% gdp
q <- quantile(dataset$gdp,probs = 0.05);q
# Create vector 'state' and add it to 'dataset'
state <- factor(ifelse(dataset$gdp < q, "crisis", "normal"))
dataset <- data.frame(cbind(dataset, state))
# Split train and test set and run logistic regression
train <- dataset[dataset$time<2008,]
test <- dataset[dataset$time>=2008,]
mdl3 <- glm(state~bci, data = train, family = binomial)
summary(mdl3)
# Compute confusion matrix
prob <- predict(mdl3, newdata = test, type = 'response')
contrasts(state)
pred <- ifelse(prob<0.05, "crisis","normal")
conf.mat <- table(pred, test$state);conf.mat
