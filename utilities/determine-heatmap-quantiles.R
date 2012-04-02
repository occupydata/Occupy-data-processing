# Copyright (c) 2012 Data Committee of Occupy DC
# 	
# Permission is hereby granted, free of charge, to any person obtaining a copy of 
# this software and associated documentation files (the "Software"), to deal in 
# the Software without restriction, including without limitation the rights to 
# use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies 
# of the Software, and to permit persons to whom the Software is furnished to do 
# so, subject to the following conditions:
# 	
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
#
# Contact: data at occupydc dot org

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
