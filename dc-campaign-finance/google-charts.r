


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





test.df<-aggregate(contribs.df$Amount[contribs.df$Contribution.Type %in% c("Corporation", "Business")],
  by=list(same.geo=contribs.df$contributor.recipient.same.geo[contribs.df$Contribution.Type %in% c("Corporation", "Business")],
  ward=contribs.df$recipient.ward[contribs.df$Contribution.Type %in% c("Corporation", "Business")]),
  FUN=sum)



test.df<-melt(test.df,  measure.vars="x")

test.df<-as.data.frame(cast(test.df, formula= ward ~ same.geo ))

cat(goog.barchart(test.df), sep="\n")








