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

install.packages("shapefiles")

library(shapefiles)

line.data.df<-read.csv("/YOUR/INPUT/PATH/AND/FILE/NAME/HERE.csv", stringsAsFactors=FALSE)

coords.df<-line.data.df[, c("lat1", "long1", "lat2", "long2")]

coords.shp.df<-data.frame(lat=rep(1, 2*nrow(coords.df)), long=rep(1, 2*nrow(coords.df)))

# coords.shp.df[1:(2*(nrow(coords.df))), ] <-coords.df[1, ]

coords.shp.df[seq(from=1, to=(2*nrow(coords.df))-1, by=2), c("lat", "long" )]<-
	coords.df[, c("lat1", "long1")]

coords.shp.df[seq(from=2, to=2*nrow(coords.df), by=2), c("lat", "long" )]<-
	coords.df[, c("lat2", "long2")]

coords.shp.df<-data.frame(
	Id=rep(1:nrow(coords.df), each=2),
	X=coords.shp.df$long,
	Y=coords.shp.df$lat
	)

line.data.df<-line.data.df[, !colnames(line.data.df) == "Id"]

coords.shp.dbf<-cbind(
	data.frame(Id=1:nrow(coords.df), Item=paste("Item", 1:nrow(coords.df), sep="")),
	line.data.df)

coords.output.shp <- convert.to.shapefile(coords.shp.df, coords.shp.dbf, "Id", 3)

write.shapefile(coords.output.shp, "/YOUR/OUTPUT/PATH/AND/FILE/NAME/HERE", arcgis=T)

