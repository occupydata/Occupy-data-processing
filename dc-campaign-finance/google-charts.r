


library("reshape")


goog.barchart<-function(x) {
	
	first.line<-paste("'", paste(colnames(x), collapse="', '"), "'", sep="")
	for ( i in 1:ncol(x) ) {
		if(is.character(x[, i])) { 
			x[, i]<-paste("'", x[, i], "'", sep="") 
		}
	}
	
	x<-as.list(x)
	x[["sep"]]<-", "
	data<-do.call(what=paste, args=x)
	ret<-c(first.line, data)
	
	ret<-paste("[", ret, "],", sep="")
	
	ret[length(ret)]<-gsub("],", "]", ret[length(ret)])
	
	
	return(ret)
	
}





corp.geog.df<-aggregate(contribs.df$Amount[contribs.df$contribution.type.clean=="Corporation"], by=list(same.geo=contribs.df$contributor.recipient.same.geo[contribs.df$contribution.type.clean=="Corporation"],
  ward=contribs.df$recipient.ward[contribs.df$contribution.type.clean=="Corporation"]),
  FUN=sum)

corp.geog.df<-melt(corp.geog.df,  measure.vars="x")
corp.geog.df<-as.data.frame(cast(corp.geog.df, formula= ward ~ same.geo ))
cat(goog.barchart(corp.geog.df), sep="\n")


corp.years.df<-aggregate(contribs.df$Amount, by=list(year=contribs.df$election.cycle,
contribution.type=contribs.df$contribution.type.clean), FUN=sum, na.rm=TRUE)

corp.years.df<-melt(corp.years.df,  measure.vars="x")
corp.years.df<-as.data.frame(cast(corp.years.df, formula= year ~ contribution.type ))
for ( i in 1:ncol(corp.years.df)) {
	corp.years.df[is.na(corp.years.df[, i]), i]<-0
}
corp.years.df<-corp.years.df[, c(
	"year", "Individual", "Corporation", "Political Action Committee", "Candidate", "Other", "Business Partnership", "Labor Union")]
cat(goog.barchart(corp.years.df), sep="\n")




corp.puppet.df<-aggregate(contribs.df$Amount[contribs.df$contribution.type.clean=="Corporation"], by=list(puppet=contribs.df$final.puppet.flag[contribs.df$contribution.type.clean=="Corporation"], year=contribs.df$election.cycle[contribs.df$contribution.type.clean=="Corporation"]), FUN=sum)

corp.puppet.df<-melt(corp.puppet.df,  measure.vars="x")
corp.puppet.df<-as.data.frame(cast(corp.puppet.df, formula= year ~ puppet ))

corp.puppet.df$puppet.proportion<-
	corp.puppet.df[, "TRUE"]/(corp.puppet.df[, "TRUE"]+corp.puppet.df[, "FALSE"])






                   
         
table(contribs.df$contribution.type.clean=="Democratic PPC")
contribs.df[contribs.df$contribution.type.clean=="Statehood Green Party PPC", c("contributor.clean", "Amount")]
