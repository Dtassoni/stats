setwd("C:/Users/dtass/OneDrive/Desktop/R work")

data <- read.fwf("lpga2008.txt", width=c(30,8,8,8,8,8,8,8,8),
                      col.names=c("golfer","drive","fairway","green","putts","sandshot",
                                  "sandsave","prz","logprz"))
# makes the full model
model_pre = lm(data$logprz ~ data$drive + data$fairway + data$green + data$putts + data$sandshot
               + data$sandsave)

# reduces model using stepwise AIC
library(SignifReg)
scope <- data$logprz ~ data$drive + data$fairway + data$green + data$putts + data$sandshot+ data$sandsave

SignifReg(model_pre, scope=scope ,criterion="AIC", direction="both"
          , trace=TRUE)

# last output is lowsest AIC and reduced model