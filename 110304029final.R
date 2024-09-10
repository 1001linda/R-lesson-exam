#1

ID = seq(1, 30)
midterm = sample(60:100, 30, replace=T)
final = sample(40:100, 30, replace=T)

#a

plot(midterm, final, type="p", pch=c(1,16))

#b

min_m = min(midterm)
min_f = min(final)

for(i in 1:30){
  if(midterm[i] == min_m){
    points(midterm[i],final[i], col=2, pch=2)
    text(midterm[i]+3,final[i]-1, "lowest midterm")
  }
}
for(i in 1:30){
  if(final[i] == min_f){
    points(midterm[i],final[i], col=2, pch=3)
    text(midterm[i],final[i]-1,"lowest final")
  }
}


#c

mean_m = mean(midterm)
mean_f = mean(final)
abline(v = mean_m)
abline(h = mean_f)

#d

mean = c()
for(i in 1:30){
  mean[i] = mean(midterm[i], final[i])
}
mean
d = data.frame(midterm, final, mean)
d = d[order(d$mean),]
for(i in 1:10){
  points(d[,3][i], d[,2][i], pch=17)
}

#2

x1 = seq(-5, 5, by=0.1)
plot(x1, dnorm(x1,-1,1), type="l", main="area under the curves", 
  ylab="normal curve", xlab="")
x2 = seq(-5, 5, by=0.1)
points(x2, dnorm(x2,0,1), type="l")
abline(h=0)

x.pt = c(-4,seq(-4,-0.5,by=0.1), -0.5)
y.pt = c(0,dnorm(seq(-4,-0.5,by=0.1),0,1),0)
polygon(x.pt,y.pt,col="gray", border=NA)

x_pt = c(-0.5,seq(-0.5,4,by=0.1), 4)
y_pt = c(0,dnorm(seq(-0.5,4,by=0.1),-1,1),0)
polygon(x_pt, y_pt, col="gray", border=NA)

text(1, 0.35,expression(paste("(",-0.5,",",frac(1,sqrt(2*pi)),e^(-1/8),")")))

#3

sale.time = strptime(c("01/01/2020","12/31/2022"),"%m/%d/%Y")
time = sample(seq(sale.time[1], sale.time[2], by="day"),1000,replace=T)
price = sample(100:1000, 1000, replace=T)
subject = sample(letters[1:8], 1000, replace=T)
data = data.frame(time, subject, price)

#a

a = data[data$subject=="a","price"]
b = data[data$subject=="b","price"]
c = data[data$subject=="c","price"]
d = data[data$subject=="d","price"]
e = data[data$subject=="e","price"]
f = data[data$subject=="f","price"]
g = data[data$subject=="g","price"]
h = data[data$subject=="h","price"]

boxplot(a,b,c,d,e,f,g,h, ylab="price", xlab="subject")
axis(1, 1:8, letters[1:8])

#b

sub_a = data[data$subject=="a",]
a = sub_a[order(sub_a$time),]

a.20 = a[year(a[, "sale.time"])==2020, "price"]
a.21 = a[year(a[, "sale.time"])==2021, "price"]
a.22 = a[year(a[, "sale.time"])==2022, "price"]

boxplot(a.20,a.21,a.22, ylab="price", xlab="year", main="subject A")
axis(1, 1:3, c(2020,2021,2022))

#c

d.1 = mean(data[month(data[,"sale.time"])==01, "price"])
d.2 = mean(data[month(data[,"sale.time"])==02, "price"])
d.3 = mean(data[month(data[,"sale.time"])==03, "price"])
d.4 = mean(data[month(data[,"sale.time"])==04, "price"])
d.5 = mean(data[month(data[,"sale.time"])==05, "price"])
d.6 = mean(data[month(data[,"sale.time"])==06, "price"])
d.7 = mean(data[month(data[,"sale.time"])==07, "price"])
d.8 = mean(data[month(data[,"sale.time"])==08, "price"])
d.9 = mean(data[month(data[,"sale.time"])==09, "price"])
d.10 = mean(data[month(data[,"sale.time"])==10, "price"])
d.11 = mean(data[month(data[,"sale.time"])==11, "price"])
d.12 = mean(data[month(data[,"sale.time"])==12, "price"])
d.mean = c(d.1,d.2,d.3,d.4,d.5,d.6,d.7,d.8,d.9,d.10,d.11,d.12)

plot(c(1:12), d.mean, type="b", ylab="average price", xlab="month")









