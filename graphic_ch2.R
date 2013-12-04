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
