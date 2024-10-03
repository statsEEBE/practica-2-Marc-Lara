#Codigo para problema 2   recta de regressió

mis_dades <- iris

x <- mis_dades$Petal.Length
y <- mis_dades$Sepal.Length

plot (x,y)

x_mitja<- mean(x)
y_mitja<- mean(y)
m <- sum((x-x_mitja)*(y-y_mitja))/(sum((x-x_mitja)^2)) # formula pendent
m

b<- y_mitja-m*x_mitja

x_pred <- 0:7                                         # interval 0,7
y_pred <- m*x_pred +b

plot(x,y)
lines(x_pred,y_pred)

x_pred <- x                                        # interval xi
y_pred <- m*x_pred +b
y_pred

R2<- (sum((y_pred -y_mitja)^2))/(sum((y-y_mitja)^2))   #corf. determinacó
R2

R<- sqrt(R2)                                           #coef correlació
R


modellineal<- lm(y~x)
modellineal
summary(modellineal)                                   # resum tot pendent, coefs...

y_pred2 <- predict(modellineal,data.frame(x=1.5))     # predicció segons model lineal
y_pred2




