##############
# Insert your file paths in input.csv.path and output.shp.path below
##############

input.csv.path<-""
output.shp.path<-""

 line.type<-"bezier"
# line.type<-"great circle"
# Choose your line type here. As it is written, it sets it to bezier

bezier.cyclone<-FALSE
# Cyclone-like bezier lines or not? Has no effect when drawing great circles

#

# TO DO: Add line distance to dbf
# TO DO: split into several shapefiles based on distance

# install.packages("geosphere")
# install.packages("shapefiles")
library(geosphere)
library(shapefiles)

line.data.df<-read.csv(input.csv.path, stringsAsFactors=FALSE)

line.data.df<-line.data.df[
	  !(line.data.df$long1==line.data.df$long2 & 
	  line.data.df$lat1==line.data.df$lat2), ]

coords.df<-line.data.df[, c("lat1", "long1", "lat2", "long2")]


coords.shp.df<-data.frame(lat=1, long=1, Id=1, stringsAsFactors=FALSE)[FALSE, ]


for ( i in 1:nrow(coords.df)) {
	
	x.0<-coords.df$long1[i]
	y.0<-coords.df$lat1[i]
	
	x.3<-coords.df$long2[i]
	y.3<-coords.df$lat2[i]
	
	#	x.0<-abs(coords.df$long1[i])
	#	y.0<-abs(coords.df$lat1[i])
	
	#	x.3<-abs(coords.df$long2[i])
	#	y.3<-abs(coords.df$lat2[i])
	
	dist.x <- x.0 - x.3
	
	dist.y <- y.0 - y.3
	
	lat.long.dist <- sqrt(dist.x^2 + dist.y^2)
	
	x.1 <- x.0  - .25*dist.x
	y.1 <- y.0 + .5*dist.y 
	
	x.2 <- x.0 + .75*dist.x
	y.2 <- y.0 + .5*dist.y
	
	#	x.1 <- mean(c(x.0, x.3))*1.1
	#	x.1 <- mean(c(x.0, x.3))*0.9
	x.1 <- mean(c(x.0, x.3))*1.1
	y.1 <- mean(c(y.0, y.3))*1.1
	
	x.2 <- mean(c(x.0, x.3))*1.1
	y.2 <- mean(c(y.0, y.3))*1.1
	
	x.1 <- mean(c(x.0, x.3))-.1*abs(dist.x)
	y.1 <- mean(c(y.0, y.3))+.3*abs(dist.y)
	
	x.2 <- mean(c(x.0, x.3))+.1*abs(dist.x)
	y.2 <- mean(c(y.0, y.3))+.3*abs(dist.y)
	
	#	x.1 <- mean(c(x.0, x.3))-.05*lat.long.dist
	y.1 <- mean(c(y.0, y.3))+(.01*lat.long.dist)^1.5
	
	#	x.2 <- mean(c(x.0, x.3))+.05*lat.long.dist
	y.2 <- mean(c(y.0, y.3))+(.01*lat.long.dist)^1.5

	slope.s<-(y.0 - y.3)/(x.0 - x.3)
	
	angle.s<-atan(slope.s)
	
	if (dist.x>0) {angle.s<-angle.s+pi}
	
	rot.mat<-matrix(c(cos(angle.s), sin(angle.s), -sin(angle.s), cos(angle.s)), ncol=2)
	
	if (!bezier.cyclone & ( (dist.x<0 & dist.y>0) | (dist.x>0 & dist.y<0) ) ) {
		point.1.mat<-matrix(c(-0.2*lat.long.dist, 0.4*lat.long.dist), ncol=2) %*% rot.mat
		point.2.mat<-matrix(c(-0.2*lat.long.dist, 0.6*lat.long.dist), ncol=2) %*% rot.mat
	} else {		
		point.1.mat<-matrix(c(0.2*lat.long.dist, 0.4*lat.long.dist), ncol=2) %*% rot.mat
		point.2.mat<-matrix(c(0.2*lat.long.dist, 0.6*lat.long.dist), ncol=2) %*% rot.mat
	}
	
	x.1<-point.1.mat[2]+x.0
	y.1<-point.1.mat[1]+y.0
	
	x.2<-point.2.mat[2]+x.0
	y.2<-point.2.mat[1]+y.0
	
	c.x <- 3*(x.1 - x.0)
	b.x <- 3*(x.2 - x.1) - c.x
	a.x <- x.3 - x.0 - c.x - b.x
	
	c.y <- 3*(y.1 - y.0)
	b.y <- 3*(y.2 - y.1) - c.y
	a.y <- y.3 - y.0 - c.y - b.y
	
	
#	lat.long.dist<-round(lat.long.dist/4)
	lat.long.dist<-round(lat.long.dist)+4
	
	init.rows<-nrow(coords.shp.df)
	if (init.rows==0) { init.rows<-1}
	
	coords.shp.df[init.rows:(init.rows+lat.long.dist), ]<- NA
	
	coords.shp.df[init.rows:(init.rows+lat.long.dist), "Id"]<- i
	
	if (line.type=="bezier") {
		tt <- (0:lat.long.dist)/lat.long.dist
		coords.shp.df[(0:lat.long.dist) +init.rows , "long"] <-a.x*tt^3 + b.x*tt^2 + c.x*tt + x.0
		coords.shp.df[(0:lat.long.dist)+init.rows , "lat"] <-a.y*tt^3 + b.y*tt^2 + c.y*tt + y.0  
	}
	
	if (line.type=="great circle") {
		rescale.for.long.way.around<-FALSE
		if(abs(x.0-x.3)>=180) {
		  rescale.for.long.way.around<-TRUE
		  x.0<-x.0/2
		  x.3<-x.3/2
		  }
		
		great.circle.arc<-
			gcIntermediate(c(x.0, y.0), c(x.3, y.3), n=lat.long.dist-2, addStartEnd=TRUE)
		
		coords.shp.df[(1:lat.long.dist) + init.rows , "long"]<-great.circle.arc[, "lon"]
		coords.shp.df[(1:lat.long.dist) + init.rows , "lat"] <-great.circle.arc[, "lat"]
		
		if (rescale.for.long.way.around) {
		  coords.shp.df[(1:lat.long.dist) + init.rows , "long"] <-great.circle.arc[, "lon"]*2
		}
	}
	
	cat(i, "\n")
}



coords.shp.df<-coords.shp.df[!is.na(coords.shp.df$long), ]

coords.shp.df<-data.frame(
	Id=coords.shp.df$Id,
	X=coords.shp.df$long,
	Y=coords.shp.df$lat
	)

line.data.df<-line.data.df[, !colnames(line.data.df) == "Id"]

coords.shp.dbf<-cbind(
	data.frame(Id=1:nrow(coords.df), Item=paste("Item", 1:nrow(coords.df), sep="")),
	line.data.df)

coords.output.shp <- convert.to.shapefile(coords.shp.df, coords.shp.dbf, "Id", 3)

write.shapefile(coords.output.shp, output.shp.path, arcgis=T)



