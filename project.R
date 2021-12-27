dataY <- read.csv("D:/Course/SP21/AMS 578/Project/IDYgroup358694.csv")
dataY <- dataY[,-1]
dataE <- read.csv("D:/Course/SP21/AMS 578/Project/IDEgroup358694.csv")
dataE <- dataE[,-1]
dataG <- read.csv("D:/Course/SP21/AMS 578/Project/IDGgroup358694.csv")
dataG <- dataG[,-1]
data <- merge(merge(dataY, dataE, by = "ID"), dataG, by = "ID")
data <- data[,-1]
summary(data)

# preliminary report
library(reporttools)
vars0 <- with(data, data.frame(data[, -1]))
cap <- "Summary statistics table for the data."
tableContinuous(vars = vars0, prec = 2, cap = cap, lab = "tab: summary", 
                longtable = FALSE)

library(naniar)
vis_miss(data)

library(finalfit)
missing_compare(data, "Y", colnames(data)[-1])

library(mice)
imp <- mice(data, method = "cart", m = 2, maxit = 10,
            seed = 123, print = FALSE)
data1 <- complete(imp, 1)
data2 <- complete(imp, 2)

cor_mat1 <- round(cor(data1[-1, -1]), 2)
cor_mat2 <- round(cor(data2[-1, -1]), 2)

library(car)
max(vif(lm(Y ~ ., data = data1)))
max(vif(lm(Y ~ ., data = data2)))


# final report
fit1 <- lm(Y ~ ., data = data1)
plot(fit1)
fit8 <- lm(Y ~ ., data = data2)
plot(fit8)

library(MASS)
bc1 <- boxcox(Y ~ ., data = data1, plotit = FALSE)
lambda1 <- bc1$x[which.max(bc1$y)]
bc2 <- boxcox(Y ~ ., data = data2, plotit = FALSE)
lambda2 <- bc2$x[which.max(bc2$y)]

fit2 <- lm(log(Y) ~ ., data = data1)
plot(fit2)
fit9 <- lm(log(Y) ~ ., data = data2)
plot(fit9)

step(fit2, k = log(nrow(data1)))
fit3 <- lm(log(Y) ~ E4 + E5 + R1 + R7 + R16, data = data1)

sum(fit3$residuals^2)
sum(fit3$residuals^2)/(nrow(data1)-6)
summary(fit3)$r.squared
summary(fit3)$adj.r.squared
AIC(fit3)
AIC(fit3, k = log(nrow(data1)))
library(qpcR)
PRESS(fit3)$stat
library(car)
max(vif(fit3))

step(fit9, k = log(nrow(data2)))
fit10 <- lm(log(Y) ~ E4 + E5 + R1 + R7 + R16, data = data2)

sum(fit10$residuals^2)
sum(fit10$residuals^2)/(nrow(data2)-6)
summary(fit10)$r.squared
summary(fit10)$adj.r.squared
AIC(fit10)
AIC(fit10, k = log(nrow(data2)))
PRESS(fit10)$stat
max(vif(fit10))

step(fit3, scope = . ~ .^2, k = log(nrow(data1)))
fit4 <- lm(log(Y) ~ E4 + E5 + R1 + R7 + R16 + R1:R7 + R1*R16 + R7*R16 + E4*E5, 
           data = data1)

sum(fit4$residuals^2)
sum(fit4$residuals^2)/(nrow(data1)-10)
summary(fit4)$r.squared
summary(fit4)$adj.r.squared
AIC(fit4)
AIC(fit4, k = log(nrow(data1)))
PRESS(fit4)$stat
max(vif(fit4))

step(fit10, scope = . ~ .^2, k = log(nrow(data2)))
fit11 <- lm(log(Y) ~ E4 + E5 + R1 + R7 + R16 + R1:R7 + R1*R16 + R7*R16 + E4*E5,
            data = data2)

sum(fit11$residuals^2)
sum(fit11$residuals^2)/(nrow(data2)-10)
summary(fit11)$r.squared
summary(fit11)$adj.r.squared
AIC(fit11)
AIC(fit11, k = log(nrow(data2)))
PRESS(fit11)$stat
max(vif(fit11))

fit5 <- lm(log(Y) ~ E4 + E5 + R1 + R7 + R16 + R1:R7 + R1*R16 + R7*R16, 
           data = data1)

sum(fit5$residuals^2)
sum(fit5$residuals^2)/(nrow(data1)-9)
summary(fit5)$r.squared
summary(fit5)$adj.r.squared
AIC(fit5)
AIC(fit5, k = log(nrow(data1)))
PRESS(fit5)$stat
max(vif(fit5))

fit12 <- lm(log(Y) ~ E4 + E5 + R1 + R7 + R16 + R1:R7 + R1*R16 + R7*R16,
            data = data2)

sum(fit12$residuals^2)
sum(fit12$residuals^2)/(nrow(data2)-9)
summary(fit12)$r.squared
summary(fit12)$adj.r.squared
AIC(fit12)
AIC(fit12, k = log(nrow(data2)))
PRESS(fit12)$stat
max(vif(fit12))

step(fit3, scope = . ~ .^3, k = log(nrow(data1)))
fit6 <- lm(log(Y) ~ E4 + E5 + R1 + R7 + R16 + R1:R7 + R1*R16 + R7*R16
           + R1*R7*R16, data = data1)

sum(fit6$residuals^2)
sum(fit6$residuals^2)/(nrow(data1)-10)
summary(fit6)$r.squared
summary(fit6)$adj.r.squared
AIC(fit6)
AIC(fit6, k = log(nrow(data1)))
PRESS(fit6)$stat
max(vif(fit6))

step(fit10, scope = . ~ .^3, k = log(nrow(data2)))
fit13 <- lm(log(Y) ~ E4 + E5 + R1 + R7 + R16 + R1:R7 + R1*R16 + R7*R16
            + R1*R7*R16, data = data2)

sum(fit13$residuals^2)
sum(fit13$residuals^2)/(nrow(data2)-10)
summary(fit13)$r.squared
summary(fit13)$adj.r.squared
AIC(fit13)
AIC(fit13, k = log(nrow(data2)))
PRESS(fit13)$stat
max(vif(fit13))

step(fit3, scope = . ~ .^4, k = log(nrow(data1)))

step(fit10, scope = . ~ .^4, k = log(nrow(data2)))

fit7 <- lm(log(Y) ~ 1, data = data1)

sum(fit7$residuals^2)
sum(fit7$residuals^2)/(nrow(data1)-2)
summary(fit7)$r.squared
summary(fit7)$adj.r.squared
AIC(fit7)
AIC(fit7, k = log(nrow(data1)))
PRESS(fit7)$stat
max(vif(fit7))

fit14 <- lm(log(Y) ~ 1, data = data2)

sum(fit14$residuals^2)
sum(fit14$residuals^2)/(nrow(data2)-2)
summary(fit14)$r.squared
summary(fit14)$adj.r.squared
AIC(fit14)
AIC(fit14, k = log(nrow(data2)))
PRESS(fit14)$stat
max(vif(fit14))

anova(fit7, fit5)
anova(fit5)

plot(fit5)

fit <- with(data = imp, exp = lm(log(Y) ~ E4 + E5 + R1 + R7 + R16 + R1:R7 + 
                                   R1*R16 + R7*R16))
summary(pool(fit))