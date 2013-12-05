ggplot(BOD, aes(x=Time, y=demand))+geom_line()

ggplot(BOD, aes(x=factor(Time), y=demand, group=1))+geom_line()

ggplot(BOD, aes(x=Time, y=demand))+geom_line()+ylim(0, max(BOD$demand))
ggplot(BOD, aes(x=Time, y=demand))+geom_line()+expand_limits(y=0)
ggplot(BOD, aes(x=Time, y=demand))+geom_line()+geom_point()

head(worldpop,10)
ggplot(worldpop, aes(x=Year, y=Population))+geom_line()+geom_point()
ggplot(worldpop, aes(x=Year, y=Population))+geom_line()+geom_point()+scale_y_log10()

#multiple line
library(plyr)
tg<-ddply(ToothGrowth, c("supp","dose"), summarise, length=mean(len))
#aggregate(len~supp*dose, data=ToothGrowth, mean)
ggplot(tg, aes(x=dose, y=length, colour=supp))+geom_line()
ggplot(tg, aes(x=dose, y=length, linetype=supp))+geom_line()
ggplot(tg, aes(x=dose, y=length, colour=supp, linetype=supp))+geom_line()

ggplot(tg, aes(x=factor(dose), y=length, colour=supp, group=supp))+geom_line()

ggplot(tg, aes(x=dose, y=length, shape=supp))+geom_line()+geom_point(size=4)
ggplot(tg, aes(x=dose, y=length, linetype=supp, colour=supp, fill=supp))+geom_line()+geom_point(size=4, shape=21)

ggplot(tg, aes(x=dose, y=length, shape=supp)) +geom_line(position=position_dodge(0.2))+geom_point(position=position_dodge(0.2), size=4)


ggplot(BOD, aes(x=Time, y=demand))+geom_line(linetype="dashed", size=1, colour="blue")

ggplot(tg, aes(x=dose, y=length, colour=supp))+geom_line()+scale_colour_brewer(palette="Set1")

ggplot(tg, aes(x=dose, y=length, group=supp))+geom_line(colour="darkgreen", size=1.5)

ggplot(tg, aes(x=dose, y=length, colour=supp))+geom_line(linetype="dashed")+geom_point(shape=22, size=3, fil="white")

#change points appearance
ggplot(BOD, aes(x=Time, y=demand))+geom_line()+geom_point(size=4, shape=22, colour="darkred", fill="pink")

#fill color is relevant for point shapes 21~25
pd<-position_dodge(0.2)
ggplot(tg, aes(x=dose, y=length, fill=supp))+geom_line(position=pd)+geom_point(position=pd, shape=21, size=3)+scale_fill_manual(values=c('black','white'))

#area chart

sunspotyear<-data.frame(
  Year=as.numeric(time(sunspot.year)),
  Sunspots=as.numeric(sunspot.year)
)
ggplot(sunspotyear, aes(x=Year, y=Sunspots))+geom_area()
ggplot(sunspotyear,aes(x=Year, y=Sunspots))+geom_area(colour="black", fill="blue", alpha=.2)
ggplot(sunspotyear, aes(x=Year, y=Sunspots))+geom_area(fill='blue', alpha=.2)+geom_line()


#stacked area chart
ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup))+geom_area()+guides(fill=guide_legend(reverse=T))

ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup))+geom_area(colour="black", size=.2, alpha=.4)+scale_fill_brewer(palette="Blues", breaks=rev(levels(uspopage$AgeGroup)))

ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup))+geom_area(colour="black", size=.2, alpha=.4)+scale_fill_brewer(palette="Blues")+guides(fill=guide_legend(reverse=T))

ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup, order=desc(AgeGroup)))+geom_area(colour='black', size=.2, alpha=.4)+scale_fill_brewer(palette='Blues')

ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup,, order=desc(AgeGroup))) +geom_area(alpha=.4)+scale_fill_brewer(palette='Blues')+geom_line(position='stack', size=.2)

#percentage area
uspopage_prop<-ddply(uspopage, "Year", transform, Percent=Thousands/sum(Thousands)*100)
ggplot(uspopage_prop, aes(x=Year, y=Percent, fill=AgeGroup))+geom_area(colour='black', size=.2, alpha=.4)+scale_fill_brewer(palette='Blues', breaks=rev(levels(uspopage_prop$AgeGroup)))


#confidence region
clim<-subset(climate, Source=="Berkeley", select=c("Year","Anomaly10y", "Unc10y"))

ggplot(clim, aes(x=Year, y=Anomaly10y))+
  geom_ribbon(aes(ymin=Anomaly10y-Unc10y, ymax=Anomaly10y+Unc10y), alpha=0.2)+geom_line()


ggplot(clim, aes(x=Year, y=Anomaly10y))+
  geom_line(aes(y=Anomaly10y-Unc10y), colour='grey50', linetype='dotted')+
  geom_line(aes(y=Anomaly10y+Unc10y), colour='grey50', linetype='dotted')+
  geom_line()

           