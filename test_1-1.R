library("psych")
library("dplyr")
library("ggplot2")
library("GGally")

d <- cars
glimpse(d)
head(d)
tail(d)
describe(d)
ncol(d)
nrow(d)
str(d)

mean(d$speed)

d2 <- mutate(d, speed=speed*1.67, dist=0.3*dist, ratio=dist/speed)
glimpse(d2)

qplot(data=d2, dist)
qplot(data=d2, speed, dist)

model <- lm(data=d2, dist~speed)
model

beta_hat <- coef(model)
beta_hat
eps_hat <- residuals(model)
eps_hat

y <- d2$dist
y_hat <- fitted(model)

RSS <- deviance(model)
RSS
TSS <- sum(y-mean(y)^2)
TSS
ESS <- TSS-RSS
ESS
R2 <- ESS/TSS
R2
cor(y, y_hat)^2
model.matrix(model)

nd <- data.frame(speed=c(50, 30))
predict(model, nd)                 
qplot(data=d2, speed, dist)+ stat_smooth(method="lm")


####

t <- swiss
help(swiss)
glimpse(t)
describe(t) 
ggpairs(t)
model <- lm(data=t, 
            Fertility~Agriculture+Education+Catholic)
coef(model)
fitted(model)
residuals(model)
deviance(model)

report <- summary(model)
report
report$r.squared
cor(t$Fertility, fitted(model))^2

nd2 <- data.frame(Agriculture=0.5,Catholic=0.5, Education=0.2)
predict(model, nd2)
