library(openxlsx)
setwd("C:/Users/dtass/OneDrive/Desktop/R work")
#reads data
D = read.xlsx(xlsxFile= "box_cox_example.xlsx")
D

#plots data
plot(D$x , D$y)

#makes a model

model = lm(D$y ~ D$x)
summary(model)
#saves res
RES = model$residuals
#plots residuals to check for pattern
plot(RES)

#does qq plot
qqnorm(RES)
qqline(RES)

#shaprio test

shapiro.test(RES)

# does box cox transformation
library(MASS)
box<-boxcox(object = model, lambda = seq(from = -2,
                                                to = 2, by = 0.01)) #In MASS package
# extracts lamba hat
lambda.hat<-box$x[box$y == max(box$y)]
lambda.hat

# remake model

#plots transformation

plot(D$x , D$y ^ lambda.hat)

lam_model = lm(D$y ^ lambda.hat ~ D$x)
summary(lam_model)

#saves res of transformation

lam_res = lam_model$residuals

plot(lam_res)

#qq plot and sharpio test

qqnorm(lam_res)
qqline(lam_res)

shapiro.test(lam_res)

