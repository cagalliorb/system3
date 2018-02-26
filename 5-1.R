# mazdas.txtをpythonなどを用いて予めmazdas.csvに変換している.
data = read.csv("mazdas.csv")
#year_nsorted<- data[,1]
data <- data[order(data$year),] # yearを昇順にsort
year <- data[,1]
price <- data[,2]
plot(year, price, xlim = c(70,95),ylim = c(0,42000))

order1<-lm(price~poly(year,1))
AIC(order1)
BIC(order1)
plot.new()
plot(year, price, xlim = c(70,95),ylim = c(0,42000))
par(new=T)
lines(year,fitted(order1))

order2<-lm(price~poly(year,2))
AIC(order2)
BIC(order2)
plot.new()
plot(year, price, xlim = c(70,95),ylim = c(0,42000))
par(new=T)
lines(year,fitted(order2))

order3<-lm(price~poly(year,3))
AIC(order3)
BIC(order3)
plot.new()
plot(year, price, xlim = c(70,95),ylim = c(0,42000))
par(new=T)
lines(year,fitted(order3))

order4<-lm(price~poly(year,4))
AIC(order4)
BIC(order4)
plot.new()
plot(year, price, xlim = c(70,95),ylim = c(0,42000))
par(new=T)
lines(year,fitted(order4))

order5<-lm(price~poly(year,5))
AIC(order5)
BIC(order5)
plot.new()
plot(year, price, xlim = c(70,95),ylim = c(0,42000))
par(new=T)
lines(year,fitted(order5))

order6<-lm(price~poly(year,6))
AIC(order6)
BIC(order6)
plot.new()
plot(year, price, xlim = c(70,95),ylim = c(0,42000))
par(new=T)
lines(year,fitted(order6))

order7<-lm(price~poly(year,7))
AIC(order7)
BIC(order7)
plot.new()
plot(year, price, xlim = c(70,95),ylim = c(0,42000))
par(new=T)
lines(year,fitted(order7))

order8<-lm(price~poly(year,8))
AIC(order8)
BIC(order8)
plot.new()
plot(year, price, xlim = c(70,95),ylim = c(0,42000))
par(new=T)
lines(year,fitted(order8))

order9<-lm(price~poly(year,9))
AIC(order9)
BIC(order9)
plot.new()
plot(year, price, xlim = c(70,95),ylim = c(0,42000))
par(new=T)
lines(year,fitted(order9))

order10<-lm(price~poly(year,10))
AIC(order10)
BIC(order10)
plot.new()
plot(year, price, xlim = c(70,95),ylim = c(0,42000))
par(new=T)
lines(year,fitted(order10))
"
以下,nlsで非線形回帰分析を
行うが,オプションでcontrol=nls.control(warnOnly=TRUE)
を指定しないとパラメータによっては
---------------------------------------------
step因子を***'minFactor' 以下に縮小しました
---------------------------------------------
のようなエラーメッセージが出る.


order1<-nls(price~a+b*year,start=c(a=1,b=1)
            ,control=nls.control(warnOnly=TRUE))
summary(order1)
AIC(order1)
BIC(order1)
plot.new()
plot(year, price, xlim = c(70,95),ylim = c(0,42000))
par(new=T)
lines(year,fitted(order1))

order2<-nls(price~a+b*year+c*year^2,start=c(a=1,b=1,c=1)
            ,control=nls.control(warnOnly=TRUE))
summary(order2)
AIC(order2)
BIC(order2)
plot.new()
plot(year, price, xlim = c(70,95),ylim = c(0,42000))
par(new=T)
lines(year,fitted(order2))

order3<-nls(price~a+b*year+c*year^2+d*year^3,start=c(a=1,b=1,c=1,d=1)
            ,control=nls.control(warnOnly=TRUE))
summary(order3)
AIC(order3)
BIC(order3)
plot.new()
plot(year, price, xlim = c(70,95),ylim = c(0,42000))
par(new=T)
lines(year,fitted(order3))


order4<-nls(price~a+b*year+c*year^2+d*year^3+e*year^4,start=c(a=1,b=1,c=1,d=1,e=1)
            ,control=nls.control(warnOnly=TRUE))
summary(order4)
AIC(order4)
BIC(order4)
plot.new()
plot(year, price, xlim = c(70,95),ylim = c(0,42000))
par(new=T)
lines(year,fitted(order4))

order5<-nls(price~a+b*year+c*year^2+d*year^3+e*year^4+f*year^5
            ,start=c(a=1,b=0.1,c=10,d=0.01,e=1,f=0.1),control=nls.control(warnOnly=TRUE))
summary(order5)
AIC(order5)
BIC(order5)
plot.new()
plot(year, price, xlim = c(70,95),ylim = c(0,42000))
par(new=T)
lines(year,fitted(order5))
"