library(gcookbook)

ggplot(pg_mean, aes(x=group, y=weight))+geom_bar(stat='identity')
ggplot(BOD, aes(x=Time, y=demand)) +geom_bar(stat="identity")
ggplot(BOD, aes(x=factor(Time), y=demand)) +geom_bar(stat="identity")


#outline color , fill
ggplot(pg_mean, aes(x=group, y=weight))+geom_bar(stat='identity', fill='lightblue', color='black')

#grouping
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar))+geom_bar(position='dodge')

ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar))+geom_bar(position='dodge', colour='black')+scale_fill_brewer(palette="Pastel1")


#count
ggplot(diamonds, aes(x=cut))+geom_bar()

#color
upc<-subset(uspopchange, rank(Change)>40)
ggplot(upc, aes(x=Abb, y=Change, fill=Region))+geom_bar(stat='identity',colour='black') + scale_fill_manual(values=c('#669933','#FFCC66'))+xlab('State')

ggplot(upc, aes(x=Abb, y=Change, fill=Region))+geom_bar(stat='identity',colour='black') + scale_fill_brewer(palette='Pastel1')+xlab('State')

#reorder
ggplot(upc, aes(x=reorder(Abb, Change), y=Change, fill=Region))+geom_bar(stat='identity',colour='black') + scale_fill_brewer(palette='Pastel1')+xlab('State')


#diff color for -ve / +ve
csub<-subset(climate, Source=='Berkeley' & Year>=1900)
csub$pos<-csub$Anomaly10y >=0

ggplot(csub, aes(x=Year, y=Anomaly10y,fill=pos ))+geom_bar(stat='identity', position='identity')
ggplot(csub, aes(x=Year, y=Anomaly10y, fill=pos))+geom_bar(stat='identity', position='identity', color='black', size=0.25) +scale_fill_manual(values=c("#CCEEFF", "#FFDDDD"), guide=FALSE)


#bar width
ggplot(pg_mean, aes(x=group, y=weight))+geom_bar(stat='identity')
ggplot(pg_mean, aes(x=group,y=weight))+geom_bar(stat='identity', width=0.5)

ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar))+geom_bar(stat='identity', width=0.5, position='dodge')

ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar))+geom_bar(stat='identity', width=0.5, position=position_dodge(0.7))


#stacked bar
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar))+geom_bar(stat='identity')

ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar))+geom_bar(stat='identity')+guides(fill=guide_legend(reverse=T))

library(plyr)
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar, order=desc(Cultivar)))+geom_bar(stat='identity')

ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar))+geom_bar(stat='identity', colour='black')+guides(fill=guide_legend(reverse=T))+scale_fill_brewer(palette='Pastel1')

#100% stacked bar
ce<-ddply(cabbage_exp, "Date", transform, percent_weight=Weight/sum(Weight) * 100)

ggplot(ce, aes(x=Date, y=Weight, fill=Cultivar))+geom_bar(stat='identity')+guides(fill=guide_legend(reverse=T))+scale_fill_brewer(palette='Pastel1')

#add labels

ggplot(cabbage_exp, aes(x=interaction(Date, Cultivar), y=Weight))+geom_bar(stat='identity') +geom_text(aes(label=Weight), vjust=1.5, colour='white')

ggplot(cabbage_exp, aes(x=interaction(Date, Cultivar), y=Weight))+geom_bar(stat='identity')+geom_text(aes(label=Weight), vjust=-0.2)

ggplot(cabbage_exp, aes(x=interaction(Date, Cultivar), y=Weight))+geom_bar(stat='identity')+geom_text(aes(label=Weight), vjust=-0.2)+ylim(0, max(cabbage_exp$Weight)*1.05)

ggplot(cabbage_exp, aes(x=interaction(Date, Cultivar), y=Weight))+geom_bar(stat='identity')+geom_text(aes(y=Weight+0.1, label=Weight))

ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar))+geom_bar(stat='identity', position='dodge')+geom_text(aes(label=Weight), vjust=1.5, colour='white', position=position_dodge(.9), size=3)

ce<-arrange(cabbage_exp, Date, Cultivar)

ce<-ddply(ce, "Date", transform, label_y=cumsum(Weight))
ggplot(ce, aes(x=Date, y=Weight, fill=Cultivar))+geom_bar(stat='identity')+geom_text(aes(label=Weight, y=label_y), vjust=1.5, colour='white')


ce<-arrange(cabbage_exp, Date, Cultivar)
ce<-ddply(ce, "Date", transform, label_y=cumsum(Weight)-0.5*Weight)
ggplot(ce, aes(x=Date, y=Weight, fill=Cultivar))+geom_bar(stat='identity', color='black')+geom_text(aes(label=paste(Weight, "kg"), y=label_y),colour='white')+guides(fill=guide_legend(reverse=T))

#dotplot
tophit<-tophitters2001[1:25,]
ggplot(tophit, aes(x=name, y=avg))+geom_bar()
ggplot(tophit, aes(x=avg, y=name))+geom_point()
ggplot(tophit, aes(x=avg, y=reorder(name, avg)))+geom_point(size=3) + theme_bw()+theme(panel.grid.major.x=element.blank(),panel.grid.minor.x=element.blank())
ggplot(tophit, aes(x=avg, y=reorder(name, avg))) + geom_point(size=3) + theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="grey60", linetype="dashed"))

ggplot(tophit, aes(x=reorder(name, avg), y=avg)) + geom_point(size=3) +theme_bw() + theme(axis.text.x = element_text(angle=60, hjust=1),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(colour="grey60", linetype="dashed"))

nameorder<-tophit$name[order(tophit$lg, tophit$avg)]
tophit$name<-factor(tophit$name, levels=nameorder)


ggplot(tophit, aes(x=avg, y=name))+geom_segment(aes(yend=name), xend=0, colour='grey50')+geom_point(size=3, aes(colour=lg))+scale_colour_brewer(palette='Set1', limits=c("NL","AL")) +theme_bw() +
  theme(panel.grid.major.y=element_blank(),legend.position=c(1,0.55),legend.justification=c(1,0.5))


ggplot(tophit, aes(x=avg, y=name)) +geom_segment(aes(yend=name), xend=0, colour="black") + geom_point(size=3, aes(colour=lg))

ggplot(tophit, aes(x=avg, y=name))+geom_point(size=3, aes(colour=lg)) +geom_segment(aes(yend=name),xend=0, colour="grey50") + facet_grid(lg~.,scales='free_y', space='free_y')+theme_bw()+theme(panel.grid.major.y=element_blank())