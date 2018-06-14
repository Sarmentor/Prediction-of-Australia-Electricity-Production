setwd('C:\\Users\\Rui Sarmento\\Documents\\Mestrado\\Ano 1\\Metodos de Previs?o')
load("tsa3.rda")
#dev.off()
#ver package forecast
elect <- read.csv('Electricidade.csv', header= FALSE,sep=";",dec=".")
serie <- ts(elect[289:nrow(elect),2], start=c(1980,1),frequency=12)

#plot.ts(serie[,2], ylab = "Produ??o El?ctrica mensal na Austr?lia")
min(serie)
max(serie)
mean(serie)
median(serie)

#decomposi??o da serie
library("TTR")
library("forecast")

vist2 <- ts(serie[1:(length(serie)-12)], start=c(1980,1), frequency=12)
stl_obj <- stl(vist2, s.window="periodic", robust=T, inner=3, outer=15)

windows()
plot(stl_obj)

#previs?o stl
previsaostl <- forecast.stl(stl_obj, h=12)
previsaostl

windows()
plot(previsaostl)
summary(previsaostl)

windows()
acf(previsaostl$residuals, lag.max=80)

Box.test(previsaostl$residuals, lag=20, type="Ljung-Box")

windows()
plot.ts(previsaostl$residuals)   
    
windows()
acf(previsaostl$residuals, lag=80, main="ACF dos res?duos da previs?o STL")

#Previs?o com alisamento exponencial simples
#HoltWinters
temphwa <- HoltWinters(vist2, seasonal="a")
previsaoholtwinters <- forecast.HoltWinters(temphwa, h=12)
previsaoholtwinters

#BoxCox.lambda(serie)
#serie_boxcox <- BoxCox(serie,0.2654076)
temphwa2 <- HoltWinters(serie, seasonal="a")
prevhw2 <- forecast.HoltWinters(temphwa2,h=12)

windows()
plot(previsaoholtwinters)
summary(previsaoholtwinters)

acf(previsaoholtwinters$residuals, lag.max=80)

Box.test(previsaoholtwinters$residuals, lag=20, type="Ljung-Box")
jarque.bera.test(previsaoholtwinters$residuals)

windows()
plot.ts(previsaoholtwinters$residuals)            						# make a time plot

plotForecastErrors(previsaoholtwinters$residuals) 						# make a histogram

library("forecast")

#Fun??o para verifica??o se os erros de previs?o
#s?o distribuidos normalmente e com media zero
plotForecastErrors <- function(forecasterrors)
  {
     # make a red histogram of the forecast errors:
     mybinsize <- IQR(forecasterrors)/4
     mymin  <- min(forecasterrors)*3
     mymax  <- max(forecasterrors)*3
     mybins <- seq(mymin, mymax, mybinsize)
     hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
     # freq=FALSE ensures the area under the histogram = 1
     mysd   <- sd(forecasterrors)
     # generate normally distributed data with mean 0 and standard deviation mysd
     mynorm <- rnorm(10000, mean=0, sd=mysd)
     myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
     # plot the normal curve as a blue line on top of the histogram of forecast errors:
     points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
  }


################
##ARIMA MODELS##
################

plot.ts(serie, main= "Produ??o de Eletricidade na Austr?lia", ylab = "Milh?es de KW")
windows()
acf(serie, main="ACF da S?rie", lag = 80)
windows()
pacf(serie, main="Partial ACF da S?rie", lag = 80)
windows()
plot.ts(diff(serie), main= "S?rie com diferen?a unit?ria")
serie_diff <- diff(serie)
serie_diff_saz <- diff(serie_diff, 12)
windows()
plot.ts(serie_diff_saz, main= "S?rie com diferen?a unit?ria e sazonal")
serie_saz <- diff(serie, 12)
windows()
plot.ts(serie_saz, main= "S?rie apenas com diferen?a sazonal")
windows()
acf(serie_diff_saz, main="ACF da S?rie com diferen?as simples e sazonal", lag = 80)
windows()
pacf(serie_diff_saz, main="Partial ACF da S?rie com diferen?as simples e sazonal", lag = 80)


windows()
### fit model VII=(0,1,1)(0,1,1) 12
elect.fit1=sarima(serie,0,1,1,0,1,1,12)
elect.fit1 # to view the results

windows()
elect.fit2=sarima(serie,2,1,1,0,1,1,12)
elect.fit2

windows()
elect.fit2=sarima(serie,2,1,1,1,1,1,12)
elect.fit2

modelo1<-arima(serie,order=c(0,1,1),seasonal=list(order=c(0,1,1)))
modelo1$var.coef
modelo1

forecast.Arima(modelo1,12)

browser()
#neste script experimenta-se modelar a s?rie modelos ARIMA
elect.train=serie[1:(length(serie)-12)]
#vou usar o modelo I
elect.for1=sarima.for(elect.train,12,0,1,1,0,1,1,12)
windows()
elect.for1
#windows()
elect.for1$pred # s?rie das previs?es
windows()
elect.for1$pred[1:12]# valores previstos
points(c((length(serie)-12+1):length(serie)),serie[(length(serie)-12+1):length(serie)],pch="*")

#An?lise de res?duos modelo 1
res1<-modelo1$residuals
#windows()
plot(res1)
mean(res1)
#windows()
par(mfrow=c(1,2))
acf(res1,lag.max=36)
pacf(res1,lag.max=36)
Box.test(res1, lag=36, type="L")
jarque.bera.test(res1)
shapiro.test(res1)

#windows()
plot(serie,col="blue",ylim=c(50,250))
LH.pred<-predict(modelo1,n.ahead=10)
LH.pred
lines(LH.pred$pred,col="red")
lines(LH.pred$pred+1.96*LH.pred$se,col="red",lty=3)
lines(LH.pred$pred-1.96*LH.pred$se,col="red",lty=3)
