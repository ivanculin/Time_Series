library(tseries)
#a)
podaci = as.numeric(read.table('podaci.txt', header = FALSE))
plot(podaci,col='red',type='l', xlab='Vrijeme', ylab='Vrijednost',lwd=2,main="Put vremenskog niza")
plot(podaci[1:100],col='red',type='l', xlab='Vrijeme', ylab='Vrijednost',lwd=2,main="Put vremenskog niza")
#Iz ovoga vidimo da bi sezonalna komponenta mogla biti 25
podaci2=ts(podaci,start=1,freq=25)
plot.ts(podaci2,col='red', cex=0.5)
duljina = length(podaci2)
duljina
##Trazimo sezonalnu komponentu
x<-stl(podaci2, "periodic")
komponente<-x$time.series
komponente
podaci_seasonal<-komponente[,"seasonal"]
podaci_seasonal
##samo za provjeru kada budemo rucno trend računali
podaci_trend<-komponente[,"trend"]
plot(podaci_trend)


podaci_ds=podaci2-podaci_seasonal
podaci_ds

plot(podaci_ds,col='blue', cex=0.5, xlab='Vrijeme', ylab='Vrijednost',lwd=2,main="Procjena trenda")

vrijeme=time(podaci2)
vrijeme

model1 <- lm(podaci_ds ~ vrijeme)
summary(model1)
lines(as.numeric(vrijeme),model1$fitted.values,col='red',lwd=2)
remainder=model1$residuals

plot(podaci_trend,col='blue',cex=0.5, xlab='Vrijeme', ylab='Vrijednost',lwd=2,main="Pravi trend i procjena trenda")
lines(as.numeric(vrijeme),model1$fitted.values,col='red', xlab='Vrijeme', ylab='Vrijednost',lwd=2,main="Put vremenskog niza")

#model2 <- lm(podaci_ds ~ vrijeme + I(vrijeme^2))
#summary(model2)
#lines(as.numeric(vrijeme),model1$fitted.values,col='red',lwd=2)

#model3 <- lm(podaci_ds ~ vrijeme + I(vrijeme^2))
#summary(model2)
#lines(as.numeric(vrijeme),model1$fitted.values,col='red',lwd=2)
trend = model1$coef[1]+model1$coef[2]*vrijeme
plot(trend)
podaci_bezs_bezt=podaci2-komponente[,"seasonal"]-trend
ts.plot(podaci_bezs_bezt,  xlab='Vrijeme', ylab='Vrijednost',lwd=2,main="Vremenski niz bez trenda i sezonalnosti")
acf(podaci_bezs_bezt)

###c dio

podaci.ar <- ar(podaci_bezs_bezt, method = 'yule-walker')
podaci.ar
podaci.ar$order #AR(14)

arima(podaci_bezs_bezt, order = c(14, 0, 0), include.mean = TRUE, method = 'ML')
#AIC = 6251.8 ## najbolji

arima(podaci_bezs_bezt, order = c(1, 0, 1), include.mean = TRUE, method = 'ML')
#AIC = 7047.06 

arima(podaci_bezs_bezt, order = c(0, 0, 1), include.mean = TRUE, method = 'ML')
#AIC = 7745.52

arima(podaci_bezs_bezt, order = c(0, 0, 2), include.mean = TRUE, method = 'ML')
#AIC = 7454.79

arima(podaci_bezs_bezt, order = c(0, 0, 3), include.mean = TRUE, method = 'ML')
#AIC = 7199.98

#nema smisla GARCH jer acf graf ne ide u 0

# d) biramo model AR(14) jer ima najmanji AIC

najbolji <- arima(podaci_bezs_bezt, order = c(14, 0 , 0), include.mean = TRUE, method = 'ML')
as.numeric(najbolji$coef)
#[1]  0.061256704 -0.441499173 -0.005122059
najbolji

?arima


# e)


predikcija = predict(najbolji, n.ahead = 1)
predikcija



pretp <- as.numeric(predikcija$pred)
pretp #54.898



t_novi = 26.72
tr_novi = model1$coef[1]+model1$coef[2]*t_novi
tr_novi=as.numeric(tr_novi)
sez_novi=podaci_seasonal[20]

U = pretp + 1.96*as.numeric(predikcija$se)+tr_novi+sez_novi
L = pretp - 1.96*as.numeric(predikcija$se)+tr_novi+sez_novi

plot((length(podaci)-9):length(podaci),podaci[(length(podaci)-9):length(podaci)], type = "l",xlim=c(duljina-9,duljina+1), ylim = c(min(podaci), max(podaci) + 30), ylab = "Vrijednost", xlab = "Vrijeme",main='Predviđena vrijednost' )
lines( duljina: (duljina + 1) , c( podaci[duljina], pretp+tr_novi+sez_novi), col = "red", type = "o", lty = "dashed",pch=19)
lines( duljina: (duljina + 1) , c( podaci[duljina], U ), col = "blue", type ="o", lty = "dashed",pch=19)
lines(duljina: (duljina + 1) , c( podaci[duljina], L ), col = "blue", type = "o", lty = "dashed",pch=19)
legend('topright', inset = 0.01, legend=c("Predikcijski interval","Predvidena vrijednost"), fill=c("blue","red"))

 


