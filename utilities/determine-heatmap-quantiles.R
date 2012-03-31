

install.packages("RColorBrewer")
library("RColorBrewer")

unemployment.filepath<-""
# Put unemployemnt filepath here

unemp.df<-read.csv(unemployment.filepath, stringsAsFactors=FALSE)

unemp.num.df<-unemp.df[, grepl("^X", colnames(unemp.df))]

for ( i in names(unemp.num.df)) {
	unemp.num.df[, i]<-sort(unemp.num.df[, i], decreasing=TRUE)
}

image.bins<-quantile(unemp.num.df, probs = seq(0, 1, 0.25), na.rm=TRUE)

image.bins<-quantile(unemp.num.df[, ncol(unemp.num.df)], probs = seq(0, 1, 0.25), na.rm=TRUE)

image(t(as.matrix(unemp.num.df)), 
			col=brewer.pal(length(image.bins),"Reds"), 
			breaks=image.bins, 
			xlab=names(unemp.num.df), 
			xaxt="n", yaxt="n")

axis(1, at = seq(0, 1, length.out=ncol(unemp.num.df)), labels = names(unemp.num.df), cex=.5, las=2)

abline(v=seq(0, 1, length.out=ncol(unemp.num.df))+(.5/ncol(unemp.num.df)), col="black")
