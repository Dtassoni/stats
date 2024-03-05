library(openxlsx)
setwd("C:/Users/dtass/OneDrive/Desktop/R work")
#reads data
D = read.xlsx(xlsxFile= "x^2.xlsx")
D

#plots data
plot(D$x , D$y)

#makes a model

model = lm(D$y ~ D$x)
summary(model)
#saves res
RES = model$residuals
#plots residuals to check for pattern
plot(D$x , RES)

#does qq plot
qqnorm(RES)
qqline(RES)

#shaprio test

shapiro.test(RES)
# makes new model
model2 = lm(D$y ~ D$x + I(D$x^2))
summary(model2)

# re saves res
RES2 = model2$residuals

#new qq plot
qqnorm(RES2)
qqline(RES2)

#redoes sharpiro test
shapiro.test(RES2)

#plots res looking for patternn
plot(D$x , RES2)
#sees no pattern and is random

# plots data with new model curve
plot(D$x , D$y)
curve(expr = model2$coefficients[1] +
        model2$coefficients[2]*x +
        model2$coefficients[3]*x^2, col = "red", lty =
        "solid", lwd = 1, add = TRUE)
        

#much better linear