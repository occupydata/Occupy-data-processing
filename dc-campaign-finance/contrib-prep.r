
work.dir<-""
# Put your working directory here

download.file(
  "http://commondatastorage.googleapis.com/ckannet-storage/2012-02-16T040627/DC_campaign_contribs_99_part12.zip",
  paste(work.dir, "DC_campaign_contribs_99_part12.zip", sep="")
  )
  
unzip( paste(work.dir, "DC_campaign_contribs_99_part12.zip", sep=""), exdir=paste(work.dir, "raw/", sep="") )

files.to.stack<-list.files(path=paste(work.dir, "raw/", sep=""))

files.to.stack<-files.to.stack[grepl("DC_contribs_", files.to.stack)]

contribs.df<-read.csv(paste(work.dir, "raw/", files.to.stack[1], sep=""), stringsAsFactors=FALSE)

for ( targ.file in files.to.stack[-1] ) {

  contribs.temp.df<-read.csv(paste(work.dir, "raw/", targ.file, sep=""), stringsAsFactors=FALSE)

  contribs.df<-rbind(contribs.df, contribs.temp.df)

}

contribs.to.geocode.df<-contribs.df[!duplicated(contribs.df[, c("Address", "city", "state", "Zip")]), 
  c("Address", "city", "state", "Zip")]

contribs.to.geocode.df$geocode.id<-1:nrow(contribs.to.geocode.df)

contribs.df<-merge(contribs.df, contribs.to.geocode.df)

geocode.output.ls<-vector("list", length=nrow(contribs.to.geocode.df))

URLencode.vec <- Vectorize(URLencode) 
# Find a fix for slow operation: 
# http://r.789695.n4.nabble.com/RFE-vectorize-URLdecode-td901435.html

contribs.to.geocode.df$api.url<-paste(
  "http://webgis.usc.edu/Services/Geocode/WebService/GeocoderWebServiceHttpNonParsedAdvanced_V02_96.aspx?streetAddress=",
  URLencode.vec(contribs.to.geocode.df$Address),
  "&city=",
  URLencode.vec(contribs.to.geocode.df$city),
  "&state=",
  URLencode.vec(contribs.to.geocode.df$state), 
  "&zip=",
  URLencode.vec(contribs.to.geocode.df$Zip),
  							"&apikey=YOUR_API_KEY_HERE&format=csv&census=true&censusYear=2010&notStore=false&verbose=true&h=u&geom=false&version=2.96",
  sep=""
)


for ( i in 1:3) {

	geocode.output.ls[[i]]<-
    tryCatch(read.csv(contribs.to.geocode.df$api.url[i], header=FALSE))
  
}

  
  
  
  
# Examples for WebGIS:
  https://webgis.usc.edu/Services/Geocode/WebService/GeocoderWebServiceHttpNonParsedAdvanced_V02_96.aspx?streetAddress=PO%20Box%20123&city=Beverly%20Hills
  &state=ca
  &zip=90210&apikey=demo&format=csv&census=true&censusYear=2010&notStore=false&verbose=true&h=u&geom=false&version=2.96
  
  
  
  
  http://webgis.usc.edu/Services/Geocode/WebService/GeocoderWebServiceHttpNonParsedAdvanced_V02_96.aspx?streetAddress=
  	9355%20Berton%20Way
  &city=Bevirrly%20Hills&state=ca&zip=90210&apikey=demo&format=csv&census=true&censusYear=2010&notStore=false&verbose=true&sou=true&souatts=city&geom=true&version=2.96
  
  
