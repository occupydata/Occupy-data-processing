
install.packages("chron")

source("http://blog.revolution-computing.com/downloads/calendarHeat.R")

contribs.date.agg<-aggregate(contribs.df$Amount, by=list(contribs.df$Date.of.Receipt),
 FUN=sum, na.rm=TRUE)

contribs.date.agg<-contribs.date.agg[as.Date(contribs.date.agg$Group.1) >
	as.Date("2007-01-01"), 	]

calendarHeat(contribs.date.agg$Group.1, log(contribs.date.agg$x), color="w2b")



