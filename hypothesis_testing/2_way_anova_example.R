library(openxlsx)
setwd("C:/Users/dtass/OneDrive/Desktop/R work")
#reads data
D = read.xlsx(xlsxFile= "2_way_example.xlsx")
D
#makes factors
x2 = factor(D[,2],levels= c("c","w","h"))
x1 = factor(D[,1],levels= c("x","y"))
y = D[,3]
# makes box plot
title = "Dirt removal by detergent and temp"
x_label = "group combo"
y_label = "dirt removed"


boxplot(y ~x1*x2 ,  main = title,
        xlab = x_label,
        ylab = y_label, col = c("Green","Red"))

# makes interaction plot

interaction.plot(x.factor = x2, trace.factor = x1, response = y,
                 fun = mean, col = c("blue","red"), trace.label = "detergent")

# find sds for all

sds = aggregate(y~x1+x2,FUN = sd)
sds
# making anova model
model = aov(y~x1*x2)
summary(model)

# checks normailty
RES = model$residuals

qqnorm(RES)
qqline(RES)

shapiro.test(RES)

# print P vals of anova if data is normal

P_vals = summary(model)[[1]]$"Pr(>F)"
P_vals
