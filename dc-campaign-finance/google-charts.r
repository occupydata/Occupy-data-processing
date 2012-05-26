# in- and out-geography
# Total amount from source
# By year how much (by source?)
# How proportion from puppet corps has changed over time


install.packages("reshape")
install.packages("googleVis")

library(reshape)
library(googleVis)



corp.years.df<-aggregate(contribs.df$Amount, by=list(year=contribs.df$election.cycle,
																										 contribution.type=contribs.df$contribution.type.clean), FUN=sum, na.rm=TRUE)

corp.years.df<-melt(corp.years.df,  measure.vars="x")
corp.years.df<-as.data.frame(cast(corp.years.df, formula= year ~ contribution.type ))
for ( i in 1:ncol(corp.years.df)) {
	corp.years.df[is.na(corp.years.df[, i]), i]<-0
}
corp.years.df<-corp.years.df[, c(
	"year", "Individual", "Corporation", "Candidate", "Political Action Committee", "Other", "Business Partnership", "Labor Union")]

for ( i in colnames(corp.years.df)[colnames(corp.years.df)!="year"]){
	corp.years.df[, i]<-round(corp.years.df[, i])
}

corp.years.lineplot <- gvisLineChart(corp.years.df, xvar="year", yvar=colnames(corp.years.df)[colnames(corp.years.df)!="year"])
plot(corp.years.lineplot)

corp.years.barplot <- gvisBarChart(corp.years.df, xvar="year",
  yvar=colnames(corp.years.df)[colnames(corp.years.df)!="year"], 
	options = list(
	  focusTarget="category", 
	  hAxis="{format:'$###,###,###', minValue:0}", 
	  legend="right", 
	  title="Contributions by Type of Contributor and Election Cycle")
)

data.formatting<-paste("formatter.format(data,",1:(ncol(corp.years.df)-1), ");", sep="", collapse="\n")

corp.years.barplot$html$chart[["jsData"]]<-gsub("return[(]data[)];", 
  paste("var formatter = new google.visualization.NumberFormat({prefix: '$', groupingSymbol:',',fractionDigits:0});\n", data.formatting, "\nreturn(data);", sep=""),
corp.years.barplot$html$chart[["jsData"]])

corp.years.barplot$html$caption<-""
corp.years.barplot$html$footer<-""

plot(corp.years.barplot)





corp.geog.df<-aggregate(contribs.df$Amount[contribs.df$contribution.type.clean=="Corporation" & contribs.df$election.cycle %in% 2002:2012], by=list(same.geo=contribs.df$contributor.recipient.same.geo[contribs.df$contribution.type.clean=="Corporation" & contribs.df$election.cycle %in% 2002:2012],
  ward=contribs.df$recipient.ward[contribs.df$contribution.type.clean=="Corporation" & contribs.df$election.cycle %in% 2002:2012]),
  FUN=sum)

corp.geog.df<-melt(corp.geog.df,  measure.vars="x")
corp.geog.df<-as.data.frame(cast(corp.geog.df, formula= ward ~ same.geo ))

corp.geog.df$ward[corp.geog.df$ward!="Citywide"]<-paste("Ward", corp.geog.df$ward[corp.geog.df$ward!="Citywide"], sep=" ")

corp.geog.df<-corp.geog.df[, c("ward", "TRUE", "FALSE")]

colnames(corp.geog.df)[colnames(corp.geog.df)=="TRUE"]<-"Within Ward/City"

colnames(corp.geog.df)[colnames(corp.geog.df)=="FALSE"]<-"Outside of Ward/City"



########
corp.geog.barplot <- gvisBarChart(corp.geog.df, xvar="ward",
  yvar=colnames(corp.geog.df)[colnames(corp.geog.df)!="ward"], 
  options = list(
    focusTarget="category", 
    hAxis="{format:'$###,###,###', minValue:0}", 
    legend="top", 
    title="Contribution from Corporations Based Inside or Outside of Candidates' Constituencies, 2002-2012 Elections",
    vAxis="{title: 'Type of Race'}")
)

data.formatting<-paste("formatter.format(data,",1:(ncol(corp.geog.df)-1), ");", sep="", collapse="\n")

corp.geog.barplot$html$chart[["jsData"]]<-gsub("return[(]data[)];", 
  paste("var formatter = new google.visualization.NumberFormat({prefix: '$', groupingSymbol:',',fractionDigits:0});\n", data.formatting, "\nreturn(data);", sep=""),
  corp.geog.barplot$html$chart[["jsData"]])

corp.geog.barplot$html$caption<-""
corp.geog.barplot$html$footer<-""

plot(corp.geog.barplot)




corp.puppet.df<-aggregate(contribs.df$Amount[contribs.df$contribution.type.clean=="Corporation"], by=list(puppet=contribs.df$final.puppet.flag[contribs.df$contribution.type.clean=="Corporation"], year=contribs.df$election.cycle[contribs.df$contribution.type.clean=="Corporation"]), FUN=sum)

corp.puppet.df<-melt(corp.puppet.df,  measure.vars="x")
corp.puppet.df<-as.data.frame(cast(corp.puppet.df, formula= year ~ puppet ))

corp.puppet.df$puppet.proportion<-
	corp.puppet.df[, "TRUE"]/(corp.puppet.df[, "TRUE"]+corp.puppet.df[, "FALSE"])

#corp.puppet.df$puppet.proportion<-round(100*corp.puppet.df$puppet.proportion, digits=1)

colnames(corp.puppet.df)[colnames(corp.puppet.df)=="puppet.proportion"]<-"Puppet Percentage"
corp.puppet.df[, "TRUE"]<-NULL
corp.puppet.df[, "FALSE"]<-NULL

corp.puppet.lineplot <- gvisLineChart(corp.puppet.df, xvar="year",
  yvar="Puppet Percentage", 
  options = list(
    vAxis="{format:'#,###%', minValue:0}", 
    legend="none", 
    #   vAxis: {title: 'Year',  titleTextStyle: {color: 'red'}}
    vAxis="{textStyle: {fontSize: 10}}",
    title="Percentage of Total Corporate Contributions from Puppet Corporations")
)

data.formatting<-paste("formatter.format(data,",1:(ncol(corp.puppet.df)-1), ");", sep="", collapse="\n")

corp.puppet.lineplot$html$chart[["jsData"]]<-gsub("return[(]data[)];", 
  paste("var formatter = new google.visualization.NumberFormat({ pattern: '#,###.#%'});\n", data.formatting, "\nreturn(data);", sep=""),
	corp.puppet.lineplot$html$chart[["jsData"]])

corp.puppet.lineplot$html$caption<-""
corp.puppet.lineplot$html$footer<-""

plot(corp.puppet.lineplot)






top.contrib.agg<-aggregate(contribs.df$Amount[contribs.df$contribution.type.clean=="Corporation" & contribs.df$election.cycle %in% 2002:2012], by=list(corp=contribs.df$contributor.clean[contribs.df$contribution.type.clean=="Corporation" & contribs.df$election.cycle %in% 2002:2012]), FUN=sum)
top.contrib.agg<-top.contrib.agg[order(top.contrib.agg$x, decreasing=TRUE),]

colnames(top.contrib.agg)[colnames(top.contrib.agg)=="x"]<-"Total Contributions"
top.contrib.agg<-top.contrib.agg[top.contrib.agg$corp!="Independence Bank - Interest",]
top.contrib.agg<-top.contrib.agg[1:10,]

top.contrib.barplot <- gvisBarChart(top.contrib.agg, xvar="corp",
  yvar=colnames(top.contrib.agg)[colnames(top.contrib.agg)!="corp"], 
  options = list(
    hAxis="{format:'$###,###,###', minValue:0}", 
    legend="none", 
 #   vAxis: {title: 'Year',  titleTextStyle: {color: 'red'}}
    vAxis="{textStyle: {fontSize: 10}}",
    title="Top Ten Corporate Contributors, 2002-2012 Elections")
)

data.formatting<-paste("formatter.format(data,",1:(ncol(top.contrib.agg)-1), ");", sep="", collapse="\n")

top.contrib.barplot$html$chart[["jsData"]]<-gsub("return[(]data[)];", 
  paste("var formatter = new google.visualization.NumberFormat({prefix: '$', groupingSymbol:',',fractionDigits:0});\n", data.formatting, "\nreturn(data);", sep=""),
  top.contrib.barplot$html$chart[["jsData"]])

top.contrib.barplot$html$caption<-""
top.contrib.barplot$html$footer<-""

plot(top.contrib.barplot)





