#scatter plot

library(gcookbook)

ggplot(heightweight, aes(x=ageYear, y=heightIn))+geom_point()
ggplot(heightweight, aes(x=ageYear, y=heightIn))+geom_point(shape=21)
ggplot(heightweight, aes(x=ageYear, y=heightIn))+geom_point(shape=21, size=4)
ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=sex))+geom_point()
ggplot(heightweight, aes(x=ageYear, y=heightIn, shape=sex))+geom_point()
ggplot(heightweight, aes(x=ageYear, y=heightIn, shape=sex, colour=sex))+geom_point()+scale_shape_manual(values=c(1,2))+scale_colour_brewer(palette="Set1")


hw<-heightweight
hw$weightGroup<-cut(hw$weightLb, breaks=c(-Inf, 100, Inf), labels=c("<100",">=100"))

ggplot(hw, aes(x=ageYear, y=heightIn, shape=sex, fill=weightGroup))+geom_point(size=2.5)+scale_shape_manual(values=c(21,24))+scale_fill_manual(values=c(NA, 'black'), guide=guide_legend(override.aes=list(shape=21)))

#continuous variable
ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=weightLb))+geom_point()
ggplot(heightweight, aes(x=ageYear, y=heightIn, size=weightLb))+geom_point()

ggplot(heightweight, aes(x=weightLb, y=heightIn, fill=ageYear))+geom_point(shape=21, size=2.5)+scale_fill_gradient(low='black', high='white')

ggplot(heightweight, aes(x=weightLb, y=heightIn, fill=ageYear))+geom_point(shape=21, size=2.5)+scale_fill_gradient(low='black', high='white', breaks=12:17, guide=guide_legend())


ggplot(heightweight, aes(x=ageYear, y=heightIn, size=weightLb,colour=sex))+geom_point(alpha=.5)+scale_size_area()+scale_colour_brewer(palette="Set1")


#overplotting
sp<-ggplot(diamonds, aes(x=carat, y=price))
sp+geom_point()
sp+geom_point(alpha=.01)

#rectangle
sp+stat_bin2d()
sp+stat_bin2d(bins=50)+scale_fill_gradient(low='lightblue', high='red', limits=c(0,6000))

library('hexbin')
sp+stat_binhex()+scale_fill_gradient(low='lightblue', high='red', limits=c(0,8000))

sp+stat_binhex()+scale_fill_gradient(low='lightblue', high='red', breaks=c(0,250,500, 1000,2000, 4000,6000), limits=c(0,6000))


sp1<-ggplot(ChickWeight, aes(x=Time, y=weight))
sp1+geom_point()
sp1+geom_point(position='jitter')
sp1+geom_point(position=position_jitter(width=.5, height=0))

sp1+geom_boxplot(aes(group=Time))


#fitted regression line

sp<-ggplot(heightweight, aes(x=ageYear, y=heightIn))
sp+geom_point()+stat_smooth(method=lm)
sp+geom_point()+stat_smooth(method=lm, level=0.99)
sp+geom_point()+stat_smooth(method=lm, se=FALSE)

sp+geom_point(colour='grey60')+stat_smooth(method=lm, se=FALSE, colour='black')

sp+geom_point(colour='grey60')+stat_smooth()
sp+geom_point(colour='grey60')+stat_smooth(method=loess)

###logistic regression
library(MASS)
b<-biopsy
b$classn[b$class=='benign']<-0
b$classn[b$class=='malignant']<-1

#b$classn<-as.numeric(b$class-1)
ggplot(b, aes(x=V1, y=classn))+geom_point(position='jitter')
ggplot(b, aes(x=V1, y=classn))+geom_point(position=position_jitter(width=0.3, height=0.06),alpha=0.4, shape=21, size=1.5) + stat_smooth(method=glm, family=binomial)


sps<-ggplot(heightweight,aes(x=ageYear, y=heightIn, colour=sex)) +geom_point()+scale_colour_brewer(palette = 'Set1')
sps+geom_smooth()

sps+geom_smooth(method=lm, fullrange=TRUE)

model<-lm(heightIn~ageYear+I(ageYear^2), heightweight)
xmin<-min(heightweight$ageYear)
xmax<-max(heightweight$ageYear)
predicted<-data.frame(ageYear=seq(xmin, xmax, length.out=100))
predicted$heightIn<-predict(model, predicted)

sp<-ggplot(heightweight, aes(x=ageYear, y=heightIn))+geom_point(colour='grey40')
sp+geom_line(data=predicted, size=1)

###predict model

predictvals <- function(model, xvar, yvar, xrange=NULL, samples=100, ...) {
  # If xrange isn't passed in, determine xrange from the models.
    # Different ways of extracting the x range, depending on model type 
  if (is.null(xrange)) {
    if (any(class(model) %in% c("lm", "glm"))) xrange <- range(model$model[[xvar]])
    else if (any(class(model) %in% "loess")) xrange <- range(model$x)
  }
  newdata <- data.frame(x = seq(xrange[1], xrange[2], length.out = samples))
  names(newdata) <- xvar
  newdata[[yvar]] <- predict(model, newdata = newdata, ...)
  newdata
}

modlinear<-lm(heightIn~ageYear, heightweight)
modloess<-loess(heightIn~ageYear, heightweight)
lm_predicted<-predictvals(modlinear, "ageYear", "heightIn")
loess_predicted<-predictvals(modloess, "ageYear","heightIn")

sp+geom_line(data=lm_predicted, colour='red', size=.8)+geom_line(data=loess_predicted, colour='blue', size=.8)

fitlogistic<-glm(classn~V1, b, family=binomial)
glm_predicted<-predictvals(fitlogistic, "V1", "classn", type="response")
ggplot(b, aes(x=V1, y=classn))+geom_point(position=position_jitter(width=.3, height=.08), alpha=0.4, shape=21, size=1.5)+geom_line(data=glm_predicted, colour='#1177FF', size=1)



###Adding Fitted Lines from Multiple Existing Models####
make_model<-function(data){
  lm(heightIn~ageYear, data)
}

models<-dlply(heightweight, "sex", .fun= make_model)


predvals<-ldply(models, .fun=predictvals, xvar="ageYear", yvar="heightIn")

ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=sex))+geom_point()+geom_line(data=predvals)


model<-lm(heightIn~ageYear, heightweight)
summary(model)

pred<-predictvals(model,"ageYear","heightIn")
sp<-ggplot(heightweight, aes(x=ageYear, y=heightIn))+geom_point()+geom_line(data=pred)

sp+annotate("text", label="r^2=0.42", x=16.5, y=52)
sp+annotate("text", label="r^2==0.42", parse=TRUE, x=16.5, y=52)
expression(r^2==0.42)

eqn <- as.character(as.expression(
  substitute(italic(y) == a + b * italic(x) * "," ~~ italic(r)^2 ~ "=" ~ r2,
             list(a = format(coef(model)[1], digits=3),
                  b = format(coef(model)[2], digits=3),
                  r2 = format(summary(model)$r.squared, digits=2)
             ))))
sp+annotate("text", label=eqn, parse=TRUE, x=Inf, y=-Inf, hjust=1.1, vjust=-.5)



###add rug####

ggplot(faithful, aes(x=eruptions, y=waiting))+geom_point()+geom_rug()
ggplot(faithful, aes(x=eruptions, y=waiting))+geom_point()+geom_rug(position="jitter", size=.2)


####add labels####
sp<-ggplot(subset(countries, Year==2009 & healthexp>2000), aes(x=healthexp, y=infmortality))+geom_point()
sp+annotate("text", x=4350, y=5.4, label="Canada")+annotate("text", x=7400, y=6.8, label="USA")

sp+geom_text(aes(label=Name), size=4)
sp+geom_text(aes(label=Name), size=4, vjust=0)
sp+geom_text(aes(y=infmortality+.1, label=Name), size=4, vjust=0)
sp + geom_text(aes(x=healthexp+100, label=Name), size=4, hjust=0)


cdat <- subset(countries, Year==2009 & healthexp>2000)
cdat$Name1 <- cdat$Name
idx <- cdat$Name1 %in% c("Canada", "Ireland", "United Kingdom", "United States","New Zealand", "Iceland", "Japan", "Luxembourg","Netherlands", "Switzerland")
cdat$Name1[!idx]<-NA
ggplot(cdat, aes(x=healthexp, y=infmortality))+geom_point()+geom_text(aes(x=healthexp+100, label=Name1),size=4, hjust=0)+xlim(2000,10000)

####Balloon Plot####
cdat <- subset(countries, Year==2009 & Name %in% c("Canada", "Ireland", "United Kingdom", "United States","New Zealand", "Iceland", "Japan", "Luxembourg","Netherlands", "Switzerland"))

p<-ggplot(cdat, aes(x=healthexp, y=infmortality, size=GDP))+geom_point(shape=21, colour="black", fill="cornsilk")
p+scale_size_area(max_size=15)

hec<-HairEyeColor[,,"Male"]+HairEyeColor[,,"Female"]

library(reshape2)
hec<-melt(hec, value.name="count")
ggplot(hec, aes(x=Eye, y=Hair, size=count))+geom_point(shape=21, colour="black", fill="cornsilk")+scale_size_area(max_size=20, guide=FALSE)+geom_text(aes(y=as.numeric(Hair)-sqrt(count)/22,label=count), vjust=1, size=1, colour="grey60")

####scatterplot matrix####
c2009<-subset(countries, Year==2009, select=c(Name, GDP, laborrate, healthexp, infmortality))
head(c2009)
pairs(c2009[,2:5])

panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...) { 
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y, use="complete.obs"))
  txt <- format(c(r, 0.123456789), digits=digits)[1] 
  txt <-paste(prefix, txt, sep="") 
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * (1 + r) / 2)
}

panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}

panel.lm <- function (x, y, col = par("col"), bg = NA, pch = par("pch"), cex = 1, col.smooth = "black", ...) {
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  abline(stats::lm(y ~ x),  col = col.smooth, ...)
}
pairs(c2009[,2:5], upper.panel=panel.cor, diag.panel=panel.hist, lower.panel=panel.smooth)

pairs(c2009[,2:5], upper.panel=panel.cor, diag.panel=panel.hist, lower.panel=panel.lm, col.smooth='red', pch='.')

