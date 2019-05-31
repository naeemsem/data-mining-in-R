library("ISLR")
library(MASS)
auto = Auto
fix(auto)
summary(auto)

#11. In this problem, you will develop a model to predict whether a given
#car gets high or low gas mileage based on the Auto data set.
#(a) Create a binary variable, mpg01, that contains a 1 if mpg contains
#a value above its median, and a 0 if mpg contains a value below
#its median. You can compute the median using the median()
#function. Note you may find it helpful to use the data.frame()
#function to create a single data set containing both mpg01 and
#the other Auto variables.



attach(auto)
auto$mpg01 = ifelse(mpg > median(mpg), 1, 0)


#(b) Explore the data graphically in order to investigate the association
#between mpg01 and the other features. Which of the other features seem 
#most likely to be useful in predicting mpg01? Scatterplots and boxplots 
#may be useful tools to answer this question.Describe your findings.

plot(displacement, auto$mpg01)

pairs(auto, col= auto$mpg01)

#(c) Split the data into a training set and a test set.

train = 1:(0.6*nrow(auto))

auto.train = auto[train, ]
auto.test = auto[!train, ]
auto.train
?lda


#(d) Perform LDA on the training data in order to predict mpg01
#using the variables that seemed most associated with mpg01 in (b). 
#What is the test error of the model obtained?

lda.fit = lda(mpg01~displacement+horsepower+weight+acceleration, data=auto.train)
lda.fit

lda.predict = predict(lda.fit, auto.test)
lda.class = lda.predict$class
table(lda.class, auto.test$mpg01)

mean(lda.class != auto.test$mpg01)

#(e) Perform QDA on the training data in order to predict mpg01
#using the variables that seemed most associated with mpg01 in (b). 
#What is the test error of the model obtained?

qda.fit = qda(mpg01~displacement+horsepower+weight+acceleration, data=auto.train)
qda.fit


qda.predict = predict(qda.fit, auto.test)
qda.class =  qda.predict$class
table(qda.class, auto.test$mpg01)

mean(qda.class != auto.test$mpg01)

#(f) Perform logistic regression on the training data in order to predict
#mpg01 using the variables that seemed most associated with
#mpg01 in (b). What is the test error of the model obtained?

logistic.fit= glm(mpg01~displacement+horsepower+weight+acceleration, data=auto, family = binomial, subset = train)
summary(logistic.fit)

logistic.predict = predict(logistic.fit, auto.test, type = "response")
logistic.predict = ifelse(logistic.predict > 0.5, "1","0")
table(logistic.predict, auto.test$mpg01)

mean(logistic.predict != auto.test$mpg01)

#(g) Perform KNN on the training data, with several values of K, in
#order to predict mpg01. Use only the variables that seemed most
#associated with mpg01 in (b). What test errors do you obtain?
#Which value of K seems to perform the best on this data set?

require(class)
?knn
auto = na.omit(auto)

n=nrow(auto)
train = 1:round(n*0.7)
test = round(n*0.7+1):n
head(train)
head(test)
predictors = cbind(displacement, horsepower,weight,acceleration)

knn.fit1 = knn(predictors[train,],predictors[test,], auto$mpg01[train], k=1)
table(knn.fit1,auto$mpg01[test])
mean(knn.fit1 != auto$mpg01[test])
mean(knn.fit1 == auto$mpg01[test])


knn.fit5 = knn(predictors[train,],predictors[test,], auto$mpg01[train], k=5)
table(knn.fit5,auto$mpg01[test])
mean(knn.fit5 != auto$mpg01[test])
mean(knn.fit5== auto$mpg01[test])

knn.fit10 = knn(predictors[train,],predictors[test,], auto$mpg01[train], k=10)
table(knn.fit10,auto$mpg01[test])
mean(knn.fit10 != auto$mpg01[test])
mean(knn.fit10 == auto$mpg01[test])


#K =5 has the best accuracy out of all options


















