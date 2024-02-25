setwd("C:/Users/dtass/OneDrive/Desktop/R work")
#QUESTION 1

data_q1<-read.table(file = "CH03PR04.txt", header = FALSE,
                    col.names = c("minutes", "copiers", "age.copier",
                                  "experience"), sep = "")
head(data_q1)

###
#a
####

dotchart(data_q1$copiers, main = "Dot Plot of Copiers", xlab = "Copiers")

boxplot(data_q1$copiers, main = "Dot Plot of Copiers", xlab = "Copiers")

#no outliars
###c
#X is the number of copiers serviced and Y is the total 
#number of minutes

data1.fit = lm(data_q1$minutes ~ data_q1$copiers)

residuals = data1.fit$residuals

hist(residuals)
#nothing special


###e
qqnorm(residuals)
qqline(residuals)

shapiro.test(residuals)
#p value = 0.4614

###g
library(lmtest)

bptest(data1.fit)

#H0: The error variance is constant
#H1: The error variance is not constant
#
#P value =   .27
#
#In conclsusion there is not enoguh evidence to show the error varanace is not 
#constant

library(car)
leveneTest(data_q1$copiers ~ data_q1$minutes)
### come back to it becauase ERRORRRRRRR
#########
########
#########
########
#2

#A
data_q2<-read.table(file = "CH03PR08.txt", header = FALSE, 
                   col.names = c("crime.rate", "percent") )

head(data_q2)               

dotchart(data_q2$percent, main = "Dot Plot of dipolmoas", xlab = "Percent")

boxplot(data_q2$percent)

data2.fit = lm(data_q2$crime.rate ~ data_q2$percent)

res = data2.fit$residuals

##b
hist(res)

#c
yhat = predict(data2.fit)

plot(yhat, res, xlab = "yhat", ylab = "res")



#d
qqnorm(res)
qqline(res)

shapiro.test(res)
#p value .1515

##E
#
med=median(data_q2$percent)

group1 = subset(data_q2, data_q2$percent<=med)
group2 = subset(data_q2, data_q2$percent>med)

bf.test(group1, group2)
??bftest

library(lawstat)

levene.test(data_q2$crime.rate, data_q2$percent, location = "median")

#p value .8669 
#
#
#H0: var are equal
##H1: var are not equal
#fail to reject null
#not enough evidnece to reject the null

#######
##
#Q3
#a
data_q3<-read.table(file = "CH03PR18.txt", header = FALSE, 
                    col.names = c("production.time", "lot.size") )

head(data_q3) 

plot(data_q3$lot.size, data_q3$production.time, xlab = "size", ylab = "time")

#transformation looks approtarte because it tails off
#b
data3.fit= lm(data_q3$production.time ~ sqrt(data_q3$lot.size))
summary(data3.fit)
#c
plot(sqrt(data_q3$lot.size), data_q3$production.time, xlab = "sqrt size", ylab = "time")

##D
res3=data3.fit$residuals

plot(sqrt(data_q3$lot.size), res3, xlab = "sqrt size" , ylab = "res")

qqnorm(res3)
qqline(res3)
#these show that sqrt(x) was a good trasnfomration
##E
#idk why we are using oringal units

data3.fit2= lm(data_q3$production.time ~ (data_q3$lot.size))
summary(data3.fit2)

## y^ = 6.86349+ 0.53327*x


#################
#Q4
###########
#b
confint(object =data1.fit , level = 1 - .05/2)
#(Intercept)     -7.092642  5.932329
#data_q1$copiers 13.913221 16.157275
#c
# yes because 0 is in (-7.092642,  5.932329) and 14 is in (13.913221 , 16.157275)
#########
#Q5
#############

#A


new_data <- data.frame(copiers = c(3, 5, 7))
g=nrow(new_data)
g

predict(data1.fit, newdata = new_data, interval = "confidence", level = .95)


##3 (41.14760 , 47.90357)
#5 (9.63614 , 19.27404)
#7 (71.91422 , 77.27794)

#B

predict(data1.fit, newdata = new_data, interval = "prediction", level = .95)
#4 (11.064899,  47.91578)
#7(56.421325  92.77084)


#Q6

data_q6 = read.table("SO2.dat", col.names = c("Y", "X"))
data_q6

dataq6.fit = lm(data_q6$X ~ data_q6$Y)
summary(dataq6.fit)

predict(dataq6.fit, data.frame(Y = 10.5), interval = "confidence", level = 0.9)


#10  4.466681 5.317042
 #   5.032366 6.126207
#
# getting errot "Warning message:
#'newdata' had 1 row but variables found have 14 rows 
