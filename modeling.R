x <- read.csv("CaseData.csv" , stringsAsFactors = FALSE)
View(x)
#y <- data.frame(x$Sale_Years , x$Weight , x$Capesize , x$Sale_Price)
#pairs(y)
#plot(Sale_Price~Weight, data=x)

model1 = lm(Sale_Price~Year_Built, data=x, y=TRUE)
model.summary1 = summary(model1)
model.summary1
plot(Sale_Price~Year_Built, data=x)
plot(Sale_Price~Sale_Years,data=x)
plot(Sale_Price~Weight,data=x)
plot(Sale_Price~Capesize,data=x)


model2 = lm(Sale_Price~Year_Built + Sale_Years, data=x, y=TRUE)
model.summary2 = summary(model2)
model.summary2
y2 <- data.frame(x$Sale_Price , x$Year_Built , x$Sale_Years)
pairs(y2)
#plot(Sale_Price~Year_Built + Sale_Years, data=x)

model3 = lm(Sale_Price~Year_Built + Sale_Years + Weight, data=x, y=TRUE)
model.summary3 = summary(model3)
model.summary3
y3 <- data.frame(x$Sale_Price , x$Year_Built , x$Sale_Years , x$Weight)
pairs(y3)
#plot(Sale_Price~Year_Built + Sale_Years + Weight, data=x)

model4 = lm(Sale_Price~Year_Built + Sale_Years + Weight + Capesize, data=x, y=TRUE)
model.summary4 = summary(model4)
model.summary4
y4 <- data.frame(x$Sale_Price , x$Year_Built , x$Sale_Years , x$Weight , x$Capesize)
pairs(y4)
#plot(Sale_Price~Year_Built + Sale_Years + Weight + Capesize, data=x)

model5 = lm(Sale_Price~Sale_Years + Weight + Capesize, data=x, y=TRUE)
model.summary5 = summary(model5)
model.summary5
y5 <- data.frame(x$Sale_Price , x$Sale_Years , x$Weight , x$Capesize)
pairs(y5)
#plot(Sale_Price~Sale_Years + Weight + Capesize, data=x)

#model.summary = summary(model)
#model.summary
confint(model5)

predict(model5 ,data.frame(Sale_Years=11 , Weight = 172 , Capesize = 12479) , interval = "prediction")
predict(model5 ,data.frame(Sale_Years=11 , Weight = 172 , Capesize = 12479) , interval = "confidence")

#predict(model,data.frame(Assessed=170) , interval = "prediction")
#predict(model,data.frame(Assessed=170) , interval = "confidence")
#pairs(x)

