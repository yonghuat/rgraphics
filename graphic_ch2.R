#scattered plot
library(ggplot2)
qplot(mtcars$wt, mtcars$mpg)
qplot(wt, mpg, data=mtcars)
ggplot(mtcars, aes(x=wt, y=mpg))+geom_point()

#line graph
plot(pressure$temperature, pressure$pressure, type="l")
points(pressure$temperature, pressure$pressure)
lines(pressure$temperature, pressure$pressure/2, col="red", type="l")
points(pressure$temperature, pressure$pressure/2, col="red")

qplot(pressure$temperature, pressure$pressure, geom="line")
qplot(temperature, pressure,data=pressure, geom=c("line", "point"))
ggplot(pressure, aes(x=temperature, y=pressure)) +geom_line() +geom_point()

#barchart
barplot(BOD$demand, names.arg=BOD$Time)
barplot(table(mtcars$cyl))

qplot(BOD$Time, BOD$demand, geom="bar", stat="identity")
qplot(factor(BOD$Time), BOD$demand, geom="bar", stat="identity")
qplot(mtcars$cyl)
qplot(factor(mtcars$cyl))

qplot(Time, demand, data=BOD, geom="bar", stat="identity")
ggplot(BOD, aes(x=Time, y=demand))+geom_bar(stat="identity")
qplot(factor(cyl),data=mtcars)
ggplot(mtcars, aes(x=factor(cyl)))+geom_bar()


#histogram
hist(mtcars$mpg)
hist(mtcars$mpg, breaks=10)
qplot(mtcars$mpg)
qplot(mpg, data=mtcars, binwidth=4)
ggplot(mtcars, aes(x=mpg))+geom_histogram(binwidth=4)

#boxplot
plot(ToothGrowth$supp, ToothGrowth$len)
boxplot(len~supp, data=ToothGrowth)

qplot(ToothGrowth$supp, ToothGrowth$len, geom="boxplot")
qplot(supp, len, data=ToothGrowth, geom="boxplot")
ggplot(ToothGrowth, aes(x=supp, y=len)) +geom_boxplot()

boxplot(len~supp+dose, data=ToothGrowth)
qplot(interaction(ToothGrowth$supp,ToothGrowth$dose), ToothGrowth$len, geom="boxplot")

qplot(interaction(supp, dose), len, data=ToothGrowth, geom="boxplot")
ggplot(ToothGrowth, aes(x=interaction(supp, dose), y=len))+geom_boxplot()

#function curve
curve(x^3-5*x, from=-4, to=4)
myfun<-function(xvar){
  1/(1+exp(-xvar+10))
}
curve(myfun(x), from=0, to=20)
curve(1-myfun(x), add=T, col="red")

qplot(c(0,20), fun=myfun, stat="function", geom="line")
ggplot(data.frame(x=c(0,20)), aes(x=x))+stat_function(fun=myfun, geom="line")