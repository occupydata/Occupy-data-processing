

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

